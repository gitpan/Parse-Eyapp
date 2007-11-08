#!/usr/bin/perl -w
use strict;
use Parse::Eyapp;
use RetUndef;
use Data::Dumper;

sub TERMINAL::info { $_[0]{attr} }

my $parser = RetUndef->new();
my $t = $parser->Run;
print $t->str,"\n";

$Data::Dumper::Indent = 1;
print Dumper($t);
