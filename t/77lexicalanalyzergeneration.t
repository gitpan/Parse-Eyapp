#!/usr/bin/perl -w
use strict;
my $nt;
my $skips;

BEGIN { $nt = 5; $skips = 4; }
use Test::More tests=> $skips*$nt+1+7+9+9;

SKIP: {
  skip "t/numlist.eyp not found", $nt unless ($ENV{DEVELOPER} && -r "t/numlist.eyp" && -x "./eyapp");

  unlink 't/numlist.pl';

  my $r = system(q{perl -I./lib/ eyapp -TC -s -o t/numlist.pl t/numlist.eyp});
  
  ok(!$r, "standalone option");

  ok(-s "t/numlist.pl", "modulino standalone exists");

  ok(-x "t/numlist.pl", "modulino standalone has execution permits");

  local $ENV{PERL5LIB};
  my $eyapppath = shift @INC; # Supress ~/LEyapp/lib from search path
  eval {

    $r = qx{t/numlist.pl -t -i -c '4 a b'};

  };

  ok(!$@,'t/numlist.eyp executed as standalone modulino');

  my $expected = q{
A_is_A_B(A_is_A_B(A_is_B(B_is_NUM(TERMINAL[4])),B_is_a(TERMINAL[a])),B_is_ID(TERMINAL[b]))
};
  $expected =~ s/\s+//g;
  $expected = quotemeta($expected);
  $expected = qr{$expected};

  $r =~ s/\s+//g;


  like($r, $expected,'AST for "4 a b"');

  unlink 't/numlist.pl';

}

SKIP: {
  skip "t/simplewithwhites.eyp not found", $nt unless ($ENV{DEVELOPER} && -r "t/simplewithwhites.eyp" && -r "t/inputfor77" && -x "./eyapp");

  unlink 't/simplewithwhites.pl';

  my $r = system(q{perl -I./lib/ eyapp -TC -s -o t/simplewithwhites.pl t/simplewithwhites.eyp});
  
  ok(!$r, "standalone option");

  ok(-s "t/simplewithwhites.pl", "modulino standalone exists");

  ok(-x "t/simplewithwhites.pl", "modulino standalone has execution permits");

  local $ENV{PERL5LIB};
  my $eyapppath = shift @INC; # Supress ~/LEyapp/lib from search path
  eval {

    $r = qx{t/simplewithwhites.pl -t -i -f t/inputfor77};

  };

  ok(!$@,'t/simplewithwhites.eyp executed as standalone modulino');

  my $expected = q{
A_is_A_d(A_is_a(TERMINAL[a]),TERMINAL[d])
};
  $expected =~ s/\s+//g;
  $expected = quotemeta($expected);
  $expected = qr{$expected};

  $r =~ s/\s+//g;


  like($r, $expected,'AST for file "t/inputfor77"');

  unlink 't/simplewithwhites.pl';

}

SKIP: {
  skip "t/tokensemdef.eyp not found", $nt unless ($ENV{DEVELOPER} && -r "t/tokensemdef.eyp" && -r "t/input2for77" && -x "./eyapp");

  unlink 't/tokensemdef.pl';

  my $r = system(q{perl -I./lib/ eyapp -TC -s -o t/tokensemdef.pl t/tokensemdef.eyp});
  
  ok(!$r, "standalone option");

  ok(-s "t/tokensemdef.pl", "modulino standalone exists");

  ok(-x "t/tokensemdef.pl", "modulino standalone has execution permits");

  local $ENV{PERL5LIB};
  my $eyapppath = shift @INC; # Supress ~/LEyapp/lib from search path
  eval {

    $r = qx{t/tokensemdef.pl -t -i -f t/input2for77};

  };

  ok(!$@,'t/tokensemdef.eyp executed as standalone modulino');

  my $expected = q{
A_is_A_B(A_is_A_B(A_is_B(B_is_NUM(TERMINAL[4])),B_is_a(TERMINAL[a])),B_is_ID(TERMINAL[b]))
};
  $expected =~ s/\s+//g;
  $expected = quotemeta($expected);
  $expected = qr{$expected};

  $r =~ s/\s+//g;


  like($r, $expected,'AST for file "t/input2for77"');

  unlink 't/tokensemdef.pl';

}

SKIP: {
  skip "t/quotemeta.eyp not found", $nt unless ($ENV{DEVELOPER} && -r "t/quotemeta.eyp" && -x "./eyapp");

  unlink 't/quotemeta.pl';

  my $r = system(q{perl -I./lib/ eyapp -TC -s -o t/quotemeta.pl t/quotemeta.eyp});
  
  ok(!$r, "standalone option for quotemeta.eyp");

  ok(-s "t/quotemeta.pl", "modulino standalone exists");

  ok(-x "t/quotemeta.pl", "modulino standalone has execution permits");

  local $ENV{PERL5LIB};
  my $eyapppath = shift @INC; # Supress ~/LEyapp/lib from search path
  eval {

    $r = qx{t/quotemeta.pl -t -i -m 1 -c '43 + - * []'};

  };

  ok(!$@,'t/quotemeta.eyp executed as standalone modulino');

  my $expected = q{

s_is_s(
  s_is_s(
    s_is_s(
      s_is_s(
        s_is_NUM(
          TERMINAL
        ),
        TERMINAL[+]
      ),
      TERMINAL[-]
    ),
    TERMINAL[*]
  )
)

};
  $expected =~ s/\s+//g;
  $expected = quotemeta($expected);
  $expected = qr{$expected};

  $r =~ s/\s+//g;


  like($r, $expected,'AST for file "43 + - * []"');

  unlink 't/quotemeta.pl';

}

SKIP: {
  skip "t/quotemeta2.eyp not found", 1 unless ($ENV{DEVELOPER} && -r "t/quotemeta2.eyp" && -r "t/input2for77" && -x "./eyapp");

  unlink 't/quotemeta2.pl';

  system(q{perl -I./lib/ eyapp -TC -s -o t/quotemeta2.pl t/quotemeta2.eyp 2> t/err});
  
  my $r = qx{cat t/err};

  my $expected = q{
*Error* Unexpected input: '=', at line 1 at file t/quotemeta2.eyp
*Fatal* Errors detected: No output, at eof at file t/quotemeta2.eyp
};
  $expected =~ s/\s+//g;
  $expected = quotemeta($expected);
  $expected = qr{$expected};

  $r =~ s/\s+//g;

  like($r, $expected,q{Error for %semantic token '+' = /(\+)/});

  unlink 't/err';

}

SKIP: {
  skip "t/dummytoken.eyp not found", 7 unless ($ENV{DEVELOPER} && -r "t/dummytoken.eyp" && -x "./eyapp");

  unlink 't/dummytoken.pl';

  my $r = system(q{perl -I./lib/ eyapp -C -s -o t/dummytoken.pl t/dummytoken.eyp 2> t/err});
  
  ok(!$r, "standalone option");
  
  $r = qx{cat t/err};

  is($r, '', q{no errors during compilation '%dummy token'});

  unlink 't/err';

  ok(-s "t/dummytoken.pl", "modulino standalone exists");

  ok(-x "t/dummytoken.pl", "modulino standalone has execution permits");

  local $ENV{PERL5LIB};
  my $eyapppath = shift @INC; # Supress ~/LEyapp/lib from search path
  eval {

    $r = qx{t/dummytoken.pl -t -i -m 1 -c 'if e then if e then o else o'};

  };

  ok(!$@,'t/dummytoken.eyp executed as standalone modulino');

  my $expected = q{

IFTHEN(
  TERMINAL[e],
  IFTHENELSE(
    TERMINAL[e],
    TERMINAL[o],
    TERMINAL[o]
  )
)

};
  $expected =~ s/\s+//g;
  $expected = quotemeta($expected);
  $expected = qr{$expected};

  $r =~ s/\s+//g;


  like($r, $expected,'AST for "if e then if e then o else o"');

  eval {

    $r = qx{t/dummytoken.pl -t -i -m 1 -c 'TUTU' 2>&1};

  };

  $expected = q{

Syntax error near 'TUTU'. 
Expected terminal: 'end of input'
There were 1 errors during parsing

};
  $expected =~ s/\s+//g;
  $expected = quotemeta($expected);
  $expected = qr{$expected};

  $r =~ s/\s+//g;


  like($r, $expected,'dummy token produces error');

  unlink 't/dummytoken.pl';

}

SKIP: {
  skip "t/PL_I_conflictWithLexical.eyp not found", 9 unless ($ENV{DEVELOPER} && -r "t/PL_I_conflictWithLexical.eyp" && -x "./eyapp");

  unlink 't/PL_I_conflictWithLexical.pl';

  my $r = system(q{perl -I./lib/ eyapp -C -s -o t/PL_I_conflictWithLexical.pl t/PL_I_conflictWithLexical.eyp});
  
  ok(!$r, "standalone option");

  ok(-s "t/PL_I_conflictWithLexical.pl", "modulino standalone exists");

  ok(-x "t/PL_I_conflictWithLexical.pl", "modulino standalone has execution permits");

  local $ENV{PERL5LIB};
  my $eyapppath = shift @INC; # Supress ~/LEyapp/lib from search path
  eval {

    $r = qx{t/PL_I_conflictWithLexical.pl -t -i -c 'if if=then then then=if'};

  };

  ok(!$@,'t/PL_I_conflictWithLexical.eyp executed as standalone modulino');

  my $expected = q{
IF(TERMINAL[if],EQ(ID[if],ID[then]),TERMINAL[then],ASSIGN(ID[then],ID[if]))
};
  $expected =~ s/\s+//g;
  $expected = quotemeta($expected);
  $expected = qr{$expected};

  $r =~ s/\s+//g;


  like($r, $expected,'AST for "if if=then then then=if"');

  eval {

    $r = qx{t/PL_I_conflictWithLexical.pl -t -i -c 'if then=if then if=then'};

  };

  ok(!$@,'t/PL_I_conflictWithLexical.eyp executed as standalone modulino');

  $expected = q{
IF(TERMINAL[if],EQ(ID[then],ID[if]),TERMINAL[then],ASSIGN(ID[if],ID[then]))
};
  $expected =~ s/\s+//g;
  $expected = quotemeta($expected);
  $expected = qr{$expected};

  $r =~ s/\s+//g;


  like($r, $expected,'AST for "if then=if then if=then"');

  eval {

    $r = qx{t/PL_I_conflictWithLexical.pl -t -i -c 'if then=if then if a=b then c=d'};

  };

  ok(!$@,'t/PL_I_conflictWithLexical.eyp executed as standalone modulino');

  $expected = q{
IF(TERMINAL[if],EQ(ID[then],ID[if]),TERMINAL[then],
   IF(TERMINAL[if],EQ(ID[a],ID[b]),TERMINAL[then],ASSIGN(ID[c],ID[d]))
};
  $expected =~ s/\s+//g;
  $expected = quotemeta($expected);
  $expected = qr{$expected};

  $r =~ s/\s+//g;


  like($r, $expected,'AST for "if then=if then if a=b then c=d"');

  unlink 't/PL_I_conflictWithLexical.pl';

}

SKIP: {
  skip "t/PL_I_conflictContextualTokens.eyp not found", 9 unless ($ENV{DEVELOPER} && -r "t/PL_I_conflictContextualTokens.eyp" && -x "./eyapp");

  unlink 't/PL_I_conflictContextualTokens.pl';

  my $r = system(q{perl -I./lib/ eyapp -C -s -o t/PL_I_conflictContextualTokens.pl t/PL_I_conflictContextualTokens.eyp});
  
  ok(!$r, "standalone option");

  ok(-s "t/PL_I_conflictContextualTokens.pl", "modulino standalone exists");

  ok(-x "t/PL_I_conflictContextualTokens.pl", "modulino standalone has execution permits");

  local $ENV{PERL5LIB};
  my $eyapppath = shift @INC; # Supress ~/LEyapp/lib from search path
  eval {

    $r = qx{t/PL_I_conflictContextualTokens.pl -t -i -c 'if if=then then then=if'};

  };

  ok(!$@,'t/PL_I_conflictContextualTokens.eyp executed as standalone modulino');

  my $expected = q{
IF(TERMINAL[if],EQ(ID[if],ID[then]),TERMINAL[then],ASSIGN(ID[then],ID[if]))
};
  $expected =~ s/\s+//g;
  $expected = quotemeta($expected);
  $expected = qr{$expected};

  $r =~ s/\s+//g;


  like($r, $expected,'AST for "if if=then then then=if"');

  eval {

    $r = qx{t/PL_I_conflictContextualTokens.pl -t -i -c 'if then=if then if=then'};

  };

  ok(!$@,'t/PL_I_conflictContextualTokens.eyp executed as standalone modulino');

  $expected = q{
IF(TERMINAL[if],EQ(ID[then],ID[if]),TERMINAL[then],ASSIGN(ID[if],ID[then]))
};
  $expected =~ s/\s+//g;
  $expected = quotemeta($expected);
  $expected = qr{$expected};

  $r =~ s/\s+//g;


  like($r, $expected,'AST for "if then=if then if=then"');

  eval {

    $r = qx{t/PL_I_conflictContextualTokens.pl -t -i -c 'if then=if then if a=b then c=d'};

  };

  ok(!$@,'t/PL_I_conflictContextualTokens.eyp executed as standalone modulino');

  $expected = q{
IF(TERMINAL[if],EQ(ID[then],ID[if]),TERMINAL[then],
   IF(TERMINAL[if],EQ(ID[a],ID[b]),TERMINAL[then],ASSIGN(ID[c],ID[d]))
};
  $expected =~ s/\s+//g;
  $expected = quotemeta($expected);
  $expected = qr{$expected};

  $r =~ s/\s+//g;


  like($r, $expected,'AST for "if then=if then if a=b then c=d"');

  unlink 't/PL_I_conflictContextualTokens.pl';

}
