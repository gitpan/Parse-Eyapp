#
# Module Parse::Eyapp.pm.
#
#
package Parse::Eyapp;
use 5.00600;
use strict;
BEGIN {
  unless (Parse::Eyapp::Driver->can('YYParse')) {
    our @ISA = qw(Parse::Eyapp::Output);
    require Parse::Eyapp::Output;
    # $VERSION is in Parse/Eyapp/Driver.pm
    our $VERSION = $Parse::Eyapp::Driver::VERSION;
  }
}

1;

__END__

