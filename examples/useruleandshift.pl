#!/usr/bin/perl -w
use strict;
use Rule6;
use Shift;
{ no warnings; *TERMINAL::info = \&TERMINAL::attr; }

sub SHIFTLEFT::info { $_[0]{shift} }

$Data::Dumper::Indent = 1;
my $parser = new Rule6();
$parser->YYData->{INPUT} = <>;
my $t = $parser->Run;
print "***********\n",$t->str,"\n";
$t->s(@Shift::all);
print "***********\n",$t->str,"\n";
