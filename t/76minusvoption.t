#!/usr/bin/perl -w
use strict;
my ($nt, $nt2);

BEGIN { 
$nt = 10; 
$nt2 = 50;
}
use Test::More tests=>$nt+3+2*$nt2;

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
  Statement 1: Syntax error at org. Expected ('', 'nop', '\n').
  };
  $expected =~ s/\s+//g;
  $expected = quotemeta($expected);
  $expected = qr{$expected};

  $r =~ s/\s+//g;

  like($r, $expected,'expected error tokens without -v');

  unlink 't/minusvoption/minusv.pl';
}

# Test YYExpected using the data generation program
SKIP: {
  skip "t/Generator.eyp not found", 3+2*$nt2 unless ($ENV{DEVELOPER} 
                                               && ($ENV{DEVELOPER} eq 'casiano') 
                                               && -r "t/Generator.eyp" 
                                               && -r "t/GenSupport.pm" 
                                               && -x "./eyapp");

  unlink 't/generator.pl';

  # First without -v
  my $r = system(q{perl -I./lib/ eyapp -C -o t/generator.pl t/Generator.eyp});
  
  ok(!$r, "minus v compiled");

  ok(-s "t/generator.pl", "modulino generator.pl exists");

  ok(-x "t/generator.pl", "modulino generator.pl has execution permits");

  for (1..$nt2) {
    eval {
      $r = qx{perl -It t/generator.pl 2>&1};
    };

    ok(!$@,'generator.pl executed as standalone modulino');

    my $expected =  qr{(?x)
    \A
    \s*\#\s*result:.+
    \s*
    ((\w+\s*=\s*[\w+*/;^()-]*)\s*;?\s*)+
    \Z
    };


    like($r, $expected,'arithmetic expressions generated');
  }

  unlink 't/generator.pl';
}



