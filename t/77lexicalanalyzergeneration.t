#!/usr/bin/perl -w
use strict;
my $nt;
my $skips;

BEGIN { $nt = 5; $skips = 4; }
use Test::More tests=> $skips*$nt+1;

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
