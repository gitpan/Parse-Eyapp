#!/usr/bin/perl -w
package CalcActions;
use strict;
use base qw{NoacInh};

sub NUM {
  return $_[1];
}

sub PLUS {
  $_[1]+$_[3];
}

sub TIMES {
  $_[1]*$_[3];
}

package PostActions;
use strict;
use base qw{NoacInh};

sub NUM {
  return $_[1];
}

sub PLUS {
  "$_[1] $_[3] +";
}

sub TIMES {
  "$_[1] $_[3] *";
}

package main;
use strict;

my $calcparser = CalcActions->new();
print "Write an expression: "; 
my $x = <STDIN>;
my $e = $calcparser->Run($x);

print "$e\n";

my $postparser = PostActions->new();
my $p = $postparser->Run($x);

print "$p\n";
