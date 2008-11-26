#!/usr/bin/perl 
use warnings;
use Noactions;

sub NUM::action {
  return $_[1];
}

sub PLUS::action {
  $_[1]+$_[3];
}

sub TIMES::action {
  $_[1]*$_[3];
}

my $parser = Noactions->new();
print "Write an expression: "; 
my $x;
{
  local $/ = undef;
  $x = <>;
}
my $t = $parser->Run($x);

print "$t\n";
