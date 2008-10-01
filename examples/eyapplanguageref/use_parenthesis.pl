#!/usr/bin/perl -w
use Parenthesis;
use Data::Dumper;

$Data::Dumper::Indent = 1;
$parser = Parenthesis->new();
print Dumper($parser->Run);
