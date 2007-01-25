#!/usr/bin/perl -w
use strict;
use Rule3;
use Data::Dumper;

$Data::Dumper::Indent = 1;
my $parser = new Rule3();
$parser->Run;
