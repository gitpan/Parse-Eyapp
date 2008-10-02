#!/usr/bin/perl -w
use Lhs;
use Data::Dumper;

$parser = new Lhs();
my $tree = $parser->Run;
$Data::Dumper::Indent = 1;
if (defined($tree)) { print Dumper($tree); }
else { print "Error: invalid input\n"; }
