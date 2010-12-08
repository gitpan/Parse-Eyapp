#!/usr/bin/perl -w
use strict;
my ($nt, $nt2, $nt3, $nt4, $nt5, $nt6);

BEGIN { $nt = 8; $nt5 = 7; 
}
use Test::More tests=> $nt+$nt5;

# test -S option and PPCR methodology with Pascal range versus enumerated conflict
SKIP: {
  skip "t/pascalnestedeyapp3.eyp not found", $nt unless ($ENV{DEVELOPER} 
                                                        && -r "t/pascalnestedeyapp3.eyp" 
                                                        && -x "./eyapp");

  unlink 't/ppcr.pl';

  my $r = system(q{perl -I./lib/ eyapp  -P -S range t/pascalnestedeyapp3.eyp 2>&1});
  ok(!$r, "pascalnestedeyapp3.eyp compiled with options '-S range' and -P");

  $r = system(q{perl -I./lib/ eyapp -TC -o t/ppcr.pl t/pascalnestedeyapp3.eyp});
  ok(!$r, "Pascal conflict grammar compiled");

  ok(-s "t/ppcr.pl", "modulino ppcr exists");

  ok(-x "t/ppcr.pl", "modulino has execution permits");

  eval {

    $r = qx{perl -Ilib -It t/ppcr.pl -t -i -m 1 -c 'type r = (x) .. (y); 4'};

  };

  ok(!$@,'t/pascalnestedeyapp3.eyp executed as modulino');

  my $expected = q{

typeDecl_is_type_ID_type_expr(
  TERMINAL[r],
  RANGE(
    range_is_expr_expr(
      ID(
        TERMINAL[x]
      ),
      ID(
        TERMINAL[y]
      )
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

  ok(!$@,'t/pascalnestedeyapp3.eyp executed as modulino');

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
  unlink 'range.pm';
}

# testing PPCR and -S option with CplusplusNested2.eyp
# testing nested parsing (YYPreParse) when one token 
# has been read by the outer parser
SKIP: {
  skip "t/CplusplusNested2.eyp not found", $nt5 unless ($ENV{DEVELOPER} 
                                                        && -r "t/CplusplusNested2.eyp"
                                                        && -x "./eyapp");

  unlink 't/ppcr.pl';

  my $r = system(q{perl -I./lib/ eyapp  -PS decl t/CplusplusNested2.eyp});
  ok(!$r, "Auxiliary grammar decl.pm gnerated with '-PS decl' option");

  $r = system(q{perl -I./lib/ eyapp -C -o t/ppcr.pl t/CplusplusNested2.eyp 2> t/err});
  ok(!$r, "t/CplusplusNested2.eyp grammar compiled");
  like(qx{cat t/err},qr{^$},"no warning: %expect-rr 1");

  ok(-s "t/ppcr.pl", "modulino ppcr exists");

  ok(-x "t/ppcr.pl", "modulino has execution permits");

  eval {

    $r = qx{perl -Ilib -It t/ppcr.pl -t -i -c 'int (x) + 2; int (z) = 4;' 2>&1};

  };

  ok(!$@,'t/CplusplusNested2.eyp executed as modulino');

  my $expected = q{
PROG(PROG(EMPTY,EXP(TYPECAST(TERMINAL[int],ID[x]),NUM[2])),DECL(TERMINAL[int],ID[z],NUM[4]))
};
  $expected =~ s/\s+//g;
  $expected = quotemeta($expected);
  $expected = qr{$expected};

  $r =~ s/\s+//g;


  like($r, $expected,'AST for "int (x) + 2; int (z) = 4;"');

  unlink 't/ppcr.pl';
  unlink 'decl.pm';

}

