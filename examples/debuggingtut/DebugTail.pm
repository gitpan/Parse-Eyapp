package DebugTail;
use strict;
use warnings;

use base q{Parse::Eyapp::TailSupport};

sub lex {
  my $self = shift;

  for (${$self->input()}) {
    s{^(\s+)}{} and $self->tokenline($1 =~ tr{\n}{});
    
    return ($1,$1) if s/^(.)//;
  }
  return ('',undef);
}

sub TERMINAL::info {
  $_[0]->{attr};
}

1;
