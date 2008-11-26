package Math::Tail;
use warnings;
use strict;
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
  local $" = ', ';
  warn << "ERRMSG";

Syntax error near $what (lin num $token->[1]). 
Expected one of these terminals: @expected
ERRMSG
}

sub make_lexer {
  my $input = shift;
  my ($beginline, $lineno) = (1, 1);

  return sub {
    my $parser = shift;

    $beginline = $lineno;
    for ($$input) {    # contextualize
      m{\G[ \t]*(\#.*)?}gc;

      m{\G([0-9]+(?:\.[0-9]+)?)}gc   and return ('NUM', [$1, $beginline]);
      m{\G([A-Za-z][A-Za-z0-9_]*)}gc and return ('VAR', [$1, $beginline]);
      m{\G\n}gc                      and do { $lineno++; return ("\n", ["\n", $beginline]) };
      m{\G(.)}gc                     and return ($1,    [$1, $beginline]);

      return('',undef);
    }
  }
}

sub Run {
    my($self)=shift;
    my $input = shift or die "Run error: No input given\n";
    my $yydebug = shift || 0;

    return $self->YYParse( 
      yylex => make_lexer($input), 
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

  my $debug = 0;
  my $file = '';
  my $result = GetOptions (
    "debug!" => \$debug,  
    "file=s" => \$file,
  );

  $debug = 0x1F if $debug;
  $file = shift if !$file && @ARGV; 

  my $prompt = "Expressions. Press CTRL-D (Unix) or CTRL-Z (Windows) to finish:\n";
  my $input;
  $input = uploadfile($file, $prompt);

  my $parser = $package->new();
  $parser->Run( \$input, $debug );
}

sub semantic_error {
  my ($parser, $msg) = @_;

  $parser->YYData->{ERRMSG} = $msg;
  $parser->YYError; 
}

1;
