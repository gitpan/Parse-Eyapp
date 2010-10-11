#!/usr/bin/perl -w
use strict;
my ($nt, $nt2, $nt3);

BEGIN { $nt = 8; $nt2 = 7; $nt3 = 11;}
use Test::More tests=> $nt+$nt2+$nt3;

# test PPCR methodology with Pascal range versus enumerated conflict
SKIP: {
  skip "t/pascalnestedeyapp2.eyp not found", $nt unless ($ENV{DEVELOPER} 
                                                        && -r "t/pascalnestedeyapp2.eyp" 
                                                        #&& $^V ge v5.10.0
                                                        && -r "t/Range.eyp" 
                                                        && -x "./eyapp");

  unlink 't/ppcr.pl';

  my $r = system(q{perl -I./lib/ eyapp  -Po t/Range.pm t/Range.eyp});
  ok(!$r, "Auxiliary grammar Range.yp compiled with option -P");

  $r = system(q{perl -I./lib/ eyapp -TC -o t/ppcr.pl t/pascalnestedeyapp2.eyp});
  ok(!$r, "Pascal conflict grammar compiled");

  ok(-s "t/ppcr.pl", "modulino ppcr exists");

  ok(-x "t/ppcr.pl", "modulino has execution permits");

  eval {

    $r = qx{perl -Ilib -It t/ppcr.pl -t -i -m 1 -c 'type r = (x) .. (y); 4'};

  };

  ok(!$@,'t/pascalnestedeyapp2.eyp executed as modulino');

  my $expected = q{

typeDecl_is_type_ID_type_expr(
  TERMINAL[r],
  RANGE(
    ID(
      TERMINAL[x]
    ),
    ID(
      TERMINAL[y]
    )
  ),
  NUM(
    TERMINAL[4]
  )
)

};
  $expected =~ s/\s+//g;
  $expected = quotemeta($expected);
  $expected = qr{$expected};

  $r =~ s/\s+//g;


  like($r, $expected,'AST for "type r = (x) .. (y); 4"');

  ############################
  eval {

    $r = qx{perl -Ilib -It t/ppcr.pl -t -i -m 1 -c 'type r = (x,y,z); 8'};

  };

  ok(!$@,'t/pascalnestedeyapp2.eyp executed as modulino');

  $expected = q{

typeDecl_is_type_ID_type_expr(
  TERMINAL[r],
  ENUM(
    idList_is_idList_ID(
      idList_is_idList_ID(
        ID(
          TERMINAL[x]
        ),
        TERMINAL[y]
      ),
      TERMINAL[z]
    )
  ),
  NUM(
    TERMINAL[8]
  )
)

};
  $expected =~ s/\s+//g;
  $expected = quotemeta($expected);
  $expected = qr{$expected};

  $r =~ s/\s+//g;


  like($r, $expected,'AST for "type r = (x,y,z); 8"');

  unlink 't/ppcr.pl';
  unlink 't/Range.pm';

}

SKIP: {
  skip "t/noPackratSolvedExpRG2.eyp not found", $nt2 unless ($ENV{DEVELOPER} 
                                                        && -r "t/pascalnestedeyapp2.eyp" 
                                                        && -r "t/ExpList.eyp" 
                                                        #&& $^V ge v5.10.0
                                                        && -x "./eyapp");

  unlink 't/ppcr.pl';

  my $r = system(q{perl -I./lib/ eyapp  -Po t/ExpList.pm t/ExpList.eyp});
  ok(!$r, "Auxiliary grammar ExpList.yp compiled witn -P option");

  $r = system(q{perl -I./lib/ eyapp -TC -o t/ppcr.pl t/noPackratSolvedExpRG2.eyp 2> t/err});
  ok(!$r, "S->xSx|x grammar compiled");
  like(qx{cat t/err},qr{1 shift/reduce conflict},"number of conflicts eq 1");

  ok(-s "t/ppcr.pl", "modulino ppcr exists");

  ok(-x "t/ppcr.pl", "modulino has execution permits");

  eval {

    $r = qx{perl -Ilib -It t/ppcr.pl -t -i -m 1 -c '2-3 3*4 5+2 other things' 2>&1};

  };

  ok(!$@,'t/noPackratSolvedExpRG2.eyp executed as modulino');

  my $expected = q{
Number of x's = 3
nxr = 1 nxs = 1
Shifting
nxr = 1 nxs = 2
Reducing by :MIDx nxs = 2 nxr = 1

T_is_preproc_S_other_things(
  S_is_x_S_x(
    x_is_x_OP_NUM(
      x_is_NUM(
        TERMINAL[2]
      ),
      TERMINAL[-],
      TERMINAL[3]
    ),
    S_is_x(
      x_is_x_OP_NUM(
        x_is_NUM(
          TERMINAL[3]
        ),
        TERMINAL[*],
        TERMINAL[4]
      )
    ),
    x_is_x_OP_NUM(
      x_is_NUM(
        TERMINAL[5]
      ),
      TERMINAL[+],
      TERMINAL[2]
    )
  )
)

};
  $expected =~ s/\s+//g;
  $expected = quotemeta($expected);
  $expected = qr{$expected};

  $r =~ s/\s+//g;


  like($r, $expected,'AST for "2-3 3*4 5+2"');

  unlink 't/ppcr.pl';
  unlink 't/ExpList.pm';

}

# testing eyapp option -P

SKIP: {
  skip "t/Calc.eyp not found", $nt3 unless ($ENV{DEVELOPER} && -x "./eyapp"); 
  unlink 't/ppcr.pl';

  my $r = system(q{perl -I./lib/ eyapp -PTC -o t/ppcr.pl t/Calc.eyp 2> t/err});
  ok(!$r, "Calc.eyp  compiled with opt P");
  ok(-s 't/err' == 0, "no errors during compilation");

  ok(-s "t/ppcr.pl", "modulino ppcr exists");

  ok(-x "t/ppcr.pl", "modulino has execution permits");

  eval {

    $r = qx{perl -Ilib t/ppcr.pl -t -c 'a=2\@'};

  };

  ok(!$@,'t/Calc.eyp accepts strict prefix');

  my $expected = q{
$VAR1 = {
          'a' => bless( {
                          'children' => [
                                          bless( {
                                                   'children' => [],
                                                   'attr' => '2',
                                                   'token' => 'NUM'
                                                 }, 'TERMINAL' )
                                        ]
                        }, 'exp_is_NUM' )
        };

};
  $expected =~ s/\s+//g;
  $expected = quotemeta($expected);
  $expected = qr{$expected};

  $r =~ s/\s+//g;


  like($r, $expected,'AST for "a=2@"');

  unlink 't/err';
  unlink 't/ppcr.pl';
  unlink 't/ExpList.pm';

  #without -P option

  $r = system(q{perl -I./lib/ eyapp -TC -o t/ppcr.pl t/Calc.eyp 2> t/err});
  ok(!$r, "Calc.eyp  compiled without opt P");
  ok(-s 't/err' == 0, "no errors during compilation");

  ok(-s "t/ppcr.pl", "modulino ppcr exists");

  ok(-x "t/ppcr.pl", "modulino has execution permits");

  eval {

    $r = qx{perl -Ilib t/ppcr.pl -t -c 'a=2\@' 2>&1};

  };

  $expected = q{

Syntax error near input: '@' (lin num 1). 
Expected one of these terminals: -, , /, ^, *, +, 

};
  $expected =~ s/\s+//g;
  $expected = quotemeta($expected);
  $expected = qr{$expected};

  $r =~ s/\s+//g;


  like($r, $expected,'error as expected for "a=2@"');

  unlink 't/err';
  unlink 't/ppcr.pl';
  unlink 't/ExpList.pm';
}

