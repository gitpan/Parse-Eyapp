package Tail;
use strict;
use warnings;

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

  sub lex {
    my $parser = shift;

    my $beginline = $parser->line();
    for (${$parser->input}) {    # contextualize
      m{\G([ \t\n]*)(?:\#.*)?}gc     and $parser->tokenline($1 =~ tr/\n//);

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

1;
