#!/usr/bin/perl -w
use strict;
use Calc;

my $parser = Calc->new();
$parser->YYData->{INPUT} = "a = 2*3\nb = (a+1):7\n";
my $t = $parser->Run;
print "$_\n" for @$t;
