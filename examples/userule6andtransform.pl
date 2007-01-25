#!/usr/bin/perl -w
use strict;
use Rule6;
use Parse::Eyapp::YATW;
use Data::Dumper;

$Data::Dumper::Indent = 1;
my $parser = new Rule6();
my $t = $parser->Run;
print Dumper($t);
print "\n***********\n";
my $q = Parse::Eyapp::YATW->new(
  PATTERN => sub { my $isterm = $_[0]->isa('TERMINAL'); 
                   print Dumper($_[0]) if $isterm; 
                   $isterm
                 }
);
$q->s($t);
my @m = $q->matches();
print Dumper($_) for @m;;
print "\n***********\n";
my $p = Parse::Eyapp::YATW->new(
  PATTERN => sub { my $isterm = $_[0]->isa('TERMINAL');
                  print "Token $_[0]->{token} has value $_[0]->{attr}\n" if  $isterm;
                  $isterm
                }
);
$p->s($t);
