package Tail;
use warnings;
use strict;

use base q{Parse::Eyapp::TailSupport};

sub lex {
    my($parser)=shift;

    for (${$parser->input}) {
        s/^[ \t\n]//;
        s/^(.)//s and return($1,$1);
        return ('', undef);
    }
}

1;
