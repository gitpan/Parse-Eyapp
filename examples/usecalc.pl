#!/usr/bin/perl -w
use strict;
use Calc;

my $parser = Calc->new();
my $input = <>;
my $t = $parser->Run(\$input);
print "========= Symbol Table ==============\n";
print "$_ = $t->{$_}\n" for sort keys %$t;
