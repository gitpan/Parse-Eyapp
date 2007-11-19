#!/usr/bin/perl -w
use strict;
use CalcSimple;
use Carp;

sub slurp_file {
  my $fn = shift;
  my $f;

  local $/ = undef;
  if (defined($fn)) {
    open $f, $fn 
  }
  else {
    $f = \*STDIN;
  }
  my $input = <$f>;
  return $input;
}

my $parser = CalcSimple->new();

my $input = slurp_file( shift() );
my ($r, $s) = @{$parser->Run($input)};

print "========= Results  ==============\n";
print "$_\n" for @$r;
print "========= Symbol Table ==============\n";
print "$_ = $s->{$_}\n" for sort keys %$s;
