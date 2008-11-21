#!/usr/bin/perl -w
# usetreebypass.pl prueba2.exp
use strict;
use DebugLookForward;

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

my $parser = DebugLookForward->new();

$parser->Run($input);

