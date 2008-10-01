#!/usr/bin/perl -w
use strict;
use Data::Dumper;
use Parse::Eyapp;
our %s; # symbol table

my $ts = q{
  %token FLOAT INTEGER NAME

  %{
  our %s; 
  %}

  %metatree

  %%
  Dl: D 
    | Dl ';' D
  ;

  D : $T { $L->{t} = $T->{t} } $L 
  ;

  T : FLOAT    { $lhs->{t} = "FLOAT" } 
    | INTEGER  { $lhs->{t} = "INTEGER" } 
  ;

  L : $NAME 
        { $NAME->{t} = $lhs->{t}; $s{$NAME->{attr}} = $NAME } 
    | $NAME { $NAME->{t} = $lhs->{t}; $L->{t} = $lhs->{t} } ',' $L 
        { $s{$NAME->{attr}} = $NAME } 
  ;
  %%
};

sub Error { die "Error sintáctico\n"; }

{ # Closure of $input, %reserved_words and $validchars
  my $input = "";
  my %reserved_words = ();
  my $validchars = "";

  sub parametrize__scanner {
    $input = shift;
    %reserved_words = %{shift()};
    $validchars = shift;
  }

  sub scanner {
    $input =~ m{\G\s+}gc;                     # skip whites
    if ($input =~ m{\G([a-z_A_Z]\w*)\b}gc) {
      my $w = uc($1);                 # upper case the word
      return ($w, $w) if exists $reserved_words{$w};
      return ('NAME', $1);            # not a reserved word
    } 
    return ($1, $1) if ($input =~ m/\G([$validchars])/gc);
    die "Caracter invalido: $1\n" if ($input =~ m/\G(\S)/gc);
    return ('', undef); # end of file
  }
} # end closure

Parse::Eyapp->new_grammar(input=>$ts, classname=>'main', outputfile=>'Types.pm'); 
my $parser = main->new(yylex => \&scanner, yyerror => \&Error); # Create the parser

parametrize__scanner(
  "float x,y;\ninteger a,b\n", 
  { INTEGER => 'INTEGER', FLOAT => 'FLOAT'}, 
  ",;" 
);

my $t = $parser->YYParse() or die "Syntax Error analyzing input";

$t->translation_scheme;

$Data::Dumper::Indent = 1;
$Data::Dumper::Terse = 1;
$Data::Dumper::Deepcopy  = 1;
$Data::Dumper::Deparse = 1;
print Dumper($t);
print Dumper(\%s);

