#!/usr/bin/perl -w
use strict;
my $nt;

BEGIN { $nt = 10 }
use Test::More tests=>$nt;

SKIP: {
  skip "t/minusvoption/paulocustodio.eyp not found", $nt unless ($ENV{DEVELOPER} && ($ENV{DEVELOPER} eq 'casiano') && -r "t/minusvoption/paulocustodio.eyp" && -x "./eyapp");

  unlink 't/minusvoption/minusv.pl';

  # First without -v
  my $r = system(q{perl -I./lib/ eyapp -b '' -o t/minusvoption/minusv.pl t/minusvoption/paulocustodio.eyp});
  
  ok(!$r, "minus v compiled");

  ok(-s "t/minusvoption/minusv.pl", "modulino minusv.pl exists");

  ok(-x "t/minusvoption/minusv.pl", "modulino minusv.pl has execution permits");

  eval {
    $r = qx{t/minusvoption/minusv.pl 2>&1};
  };

  ok(!$@,'minusv.pl executed as standalone modulino');

  my $expected =  q{
  Statement 2: Syntax error at org. Expected ('nop').
  };
  $expected =~ s/\s+//g;
  $expected = quotemeta($expected);
  $expected = qr{$expected};

  $r =~ s/\s+//g;

  like($r, $expected,'expected error tokens without -v');

  # Now with -v

  unlink 't/minusvoption/minusv.pl';

  $r = system(q{perl -I./lib/ eyapp -v -b '' -o t/minusvoption/minusv.pl t/minusvoption/paulocustodio.eyp});
  
  ok(!$r, "minus v compiled");

  ok(-s "t/minusvoption/minusv.pl", "modulino minusv.pl exists");

  ok(-x "t/minusvoption/minusv.pl", "modulino minusv.pl has execution permits");

  eval {
    $r = qx{t/minusvoption/minusv.pl 2>&1};
  };

  ok(!$@,'minusv.pl executed as standalone modulino');

  $expected =  q{
  Statement 1: Syntax error at org. Expected ('nop', '\n').
  };
  $expected =~ s/\s+//g;
  $expected = quotemeta($expected);
  $expected = qr{$expected};

  $r =~ s/\s+//g;

  like($r, $expected,'expected error tokens without -v');

  unlink 't/minusvoption/minusv.pl';
}


