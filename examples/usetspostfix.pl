#!/usr/bin/perl -w
use strict;
use TSPostfix;
use Parse::Eyapp::Node;
use Parse::Eyapp::YATW;

my $parser = new TSPostfix();
$parser->Run;
