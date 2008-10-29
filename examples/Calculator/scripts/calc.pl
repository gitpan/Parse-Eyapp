#!/usr/bin/perl -I../lib -w
use strict;
use Math::Calc;

my $debug = shift || 0;

print "Expression:\n";
my $input = <STDIN>;
my $parser = Math::Calc->new();
$parser->Run( \$input, $debug );
