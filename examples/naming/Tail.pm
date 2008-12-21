package Tail;
use strict;
use warnings;

use Getopt::Long;

sub _Error {
  my $parser = shift;
  my $yydata = $parser->YYData;

    exists $yydata->{ERRMSG}
  and do {
      warn $yydata->{ERRMSG};
      delete $yydata->{ERRMSG};
      return;
  };
  my($token)=$parser->YYCurval;
  my($what)= $token->[0] ? "input: '$token->[0]'" : "end of input";
  my @expected = $parser->YYExpect();
  my $next = substr($parser->{input}, pos($parser->{input}), 5);
  local $" = ', ';
  warn << "ERRMSG";

Syntax error near $what (lin num $token->[1]). 
Incoming text: 
===
$next
===
Expected one of these terminals: @expected
ERRMSG
}


{ # closure

my $lineno = 1;
my %lexemename;

  sub set_lexemename {
    my $self = shift;
    my %names = @_;

    my @keys = keys(%names);

    @lexemename{@keys} = values(%names);

    return @lexemename{@keys};
  }

  sub lexer {
    my $parser = shift;

    my $beginline = $lineno;
    for ($parser->{input}) {    # contextualize
      m{\G[ \t\n]*(\#.*)?}gc;

      m{\G([0-9]+(?:\.[0-9]+)?)}gc   and return ('NUM', [$1, $beginline]);
      m{\G([A-Za-z][A-Za-z0-9_]*)}gc and return ('VAR', [$1, $beginline]);
      m{\G(.)}gc                     and do {
        my $token = exists $lexemename{$1}? $lexemename{$1} : $1;
        return ($token, [$token, $beginline]);
      };

      return('',undef);
    }
  }
} # closure

sub Run {
    my($self)=shift;
    my $yydebug = shift || 0;

    return $self->YYParse( 
      yylex => \&lexer, 
      yyerror => \&_Error,
      yydebug => $yydebug, # 0x1F
    );
}

sub uploadfile {
  my $file = shift;
  my $msg = shift;

  my $input = '';
  eval {
    $input = Parse::Eyapp::Base::slurp_file($file) 
  };
  if ($@) {
    print $msg;
    local $/ = undef;
    $input = <STDIN>;
  }
  return $input;
}

sub main {
  my $package = shift;

  my $debug = shift || 0;
  my $file = '';
  my $result = GetOptions (
    "debug!" => \$debug,  
    "file=s" => \$file,
  );

  $debug = 0x1F if $debug;
  $file = shift if !$file && @ARGV; 

  my $parser = $package->new();
  my $prompt = "Expressions. Press CTRL-D (Unix) or CTRL-Z (Windows) to finish:\n";
  $parser->{input} = uploadfile($file, $prompt);
  $parser->Run( $debug );
}

sub semantic_error {
  my ($parser, $msg) = @_;

  $parser->YYData->{ERRMSG} = $msg;
  $parser->YYError; 
}

1;
