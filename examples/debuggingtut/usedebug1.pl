#!/usr/bin/perl -w
use strict;
use Debug1;

sub slurp_file {
  my $fn = shift;
  my $f;

  local $/ = undef;
  if (defined($fn)) {
    open $f, $fn  or die "Can't find file $fn!\n";
  }
  else {
    $f = \*STDIN;
  }
  my $input = <$f>;
  return $input;
}

my $input = slurp_file( shift() );

my $parser = Debug1->new();

$parser->Run($input);

