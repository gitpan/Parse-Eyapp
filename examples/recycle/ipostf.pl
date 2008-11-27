#!/usr/bin/perl 
use warnings;

package Actions;
use base NoacInh;

sub NUM {
  return $_[1];
}

sub PLUS {
  "$_[1] $_[3] +";
}

sub TIMES {
  "$_[1] $_[3] *";
}

my $parser = __PACKAGE__->new();
print "Write an expression: "; 
my $x;
{
  local $/ = undef;
  $x = <>;
}
my $t = $parser->Run($x);

print "$t\n";
