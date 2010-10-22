#!/usr/bin/perl -w
use strict;
my ($nt, );

BEGIN { $nt = 10; 
}
use Test::More tests=> $nt;

# test "expects" method with PL-I if if=then then then=if example
SKIP: {
  skip "t/PL_I_conflictNested.eyp not found", $nt unless ($ENV{DEVELOPER} 
                                                        && -r "t/PL_I_conflictNested.eyp" 
                                                        && -r "t/Assign.eyp" 
                                                        && -x "./eyapp");

  unlink 't/pl1.pl';

  my $r = system(q{perl -I./lib/ eyapp  -Po t/Assign.pm t/Assign.eyp});
  ok(!$r, "Auxiliary grammar Assign.yp compiled with option -P");

  $r = system(q{perl -I./lib/ eyapp -C -o t/pl1.pl t/PL_I_conflictNested.eyp});
  ok(!$r, "PL-I subgrammar compiled");

  ok(-s "t/pl1.pl", "modulino pl1 exists");

  ok(-x "t/pl1.pl", "modulino has execution permits");

  eval {

    $r = qx{perl -Ilib -It t/pl1.pl -t -i -m 1 -c 'if if=then then then=if'};

  };

  ok(!$@,'t/PL_I_conflictNested.eyp executed as modulino');

  my $expected = q{
IF(
  EQ(
    ID[if],
    ID[then]
  ),
  ASSIGN(
    ID[then],
    ID[if]
  )
)
};
  $expected =~ s/\s+//g;
  $expected = quotemeta($expected);
  $expected = qr{$expected};

  $r =~ s/\s+//g;


  like($r, $expected,'AST for "if if=then then then=if"');

#########################2 tests############################################
  eval {

    $r = qx{perl -Ilib -It t/pl1.pl -t -i -m 1 -c 'if if=then then if=then'};

  };

  ok(!$@,'t/PL_I_conflictNested.eyp executed as modulino');

  $expected = q{
IF(
  EQ(
    ID[if],
    ID[then]
  ),
  ASSIGN(
    ID[if],
    ID[then]
  )
)
};
  $expected =~ s/\s+//g;
  $expected = quotemeta($expected);
  $expected = qr{$expected};

  $r =~ s/\s+//g;


  like($r, $expected,'AST for "if if=then then if=then"');
############################################################################

#########################2 tests############################################
  eval {

    $r = qx{perl -Ilib -It t/pl1.pl -t -i -m 1 -c 'if if=then then if if=then then if=then'};

  };

  ok(!$@,'t/PL_I_conflictNested.eyp executed as modulino');

  $expected = q{
IF(
  EQ(
    ID[if],
    ID[then]
  ),
  IF(
    EQ(
      ID[if],
      ID[then]
    ),
    ASSIGN(
      ID[if],
      ID[then]
    )
  )
)
};
  $expected =~ s/\s+//g;
  $expected = quotemeta($expected);
  $expected = qr{$expected};

  $r =~ s/\s+//g;


  like($r, $expected,'AST for "if if=then then if=then"');
############################################################################

  unlink 't/pl1.pl';
  unlink 't/Assign.pm';

}

