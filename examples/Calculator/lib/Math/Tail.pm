package Math::Tail;
use warnings;
use strict;

sub semantic_error {
  my ($parser, $msg) = @_;

  $parser->YYData->{ERRMSG} = $msg;
  $parser->YYError; 
}

1;

