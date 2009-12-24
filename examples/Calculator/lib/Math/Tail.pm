package Math::Tail;
use warnings;
use strict;
use Getopt::Long;
use Pod::Usage;

sub lex {
  my $parser = shift;

  my $beginline = $parser->tokenline();
  for (${$parser->input}) {    # contextualize
    m{\G[ \t]*(\#.*)?}gc;

    m{\G([0-9]+(?:\.[0-9]+)?)}gc   and return ('NUM', [$1, $beginline]);
    m{\G([A-Za-z][A-Za-z0-9_]*)}gc and return ('VAR', [$1, $beginline]);
    m{\G\n}gc                      and do { 
                                        $parser->tokenline(1); 
                                        return ("\n", ["\n", $beginline]) 
                                   };
    m{\G(.)}gc                     and return ($1,    [$1, $beginline]);

    return('',undef);
  }
}

sub semantic_error {
  my ($parser, $msg) = @_;

  $parser->YYData->{ERRMSG} = $msg;
  $parser->YYError; 
}

1;

