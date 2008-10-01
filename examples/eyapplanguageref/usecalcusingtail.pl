#!/usr/bin/perl -w
use strict;
use CalcUsingTail;

print "Expression:\n";
my $input = <>;
my $parser = new CalcUsingTail();
$parser->Run( \$input );
