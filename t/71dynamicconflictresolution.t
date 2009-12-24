#!/usr/bin/perl -w
use strict;
my $nt;

BEGIN { $nt = 9 }
use Test::More tests=>$nt;
#use_ok qw(Parse::Eyapp) or exit;

SKIP: {
  skip "t/dynamicresolution/pascalenumeratedvsrangesolvedviadyn.eyp not found", $nt unless ($ENV{DEVELOPER} && ($ENV{DEVELOPER} eq 'casiano') && -r "t/dynamicresolution/pascalenumeratedvsrangesolvedviadyn.eyp" && -x "./eyapp");

  unlink 't/Calc.pm';

  my $r = system(q{perl -I./lib/ eyapp -b '' -s -o t/dynamicresolution/persvd.pl t/dynamicresolution/pascalenumeratedvsrangesolvedviadyn.eyp});
  
  ok(!$r, "standalone option");

  ok(-s "t/dynamicresolution/persvd.pl", "modulino standalone exists");

  ok(-x "t/dynamicresolution/persvd.pl", "modulino standalone has execution permits");

  local $ENV{PERL5LIB};
  my $eyapppath = shift @INC; # Supress ~/LEyapp/lib from search path
  eval {

    $r = qx{t/dynamicresolution/persvd.pl -t -c 'type r = (x+2)*3 ..  y/2 ;'};
  };

  ok(!$@,'pascalenumeratedvsrangesolvedviadyn executed as standalone modulino');

  my $expected = qr{type_decl_is_TYPE_ID_type\(\s+TERMINAL,\s+TERMINAL,\s+RANGE\(\s+expr_is_expr_TIMES_expr\(\s+expr_is_LP_expr_RP\(\s+expr_is_expr_PLUS_expr\(\s+ID\(\s+TERMINAL\s+\),\s+expr_is_NUM\(\s+TERMINAL\s+\)\s+\)\s+\),\s+expr_is_NUM\(\s+TERMINAL\s+\)\s+\),\s+TERMINAL,\s+expr_is_expr_DIV_expr\(\s+ID\(\s+TERMINAL\s+\),\s+expr_is_NUM\(\s+TERMINAL\s+\)\s+\)\s+\)\s+\)\s+};

  like($r, $expected,'AST for type r = (x+2)*3 ..  y/2 ;');

  eval {
    $r = qx{t/dynamicresolution/persvd.pl -t -c 'type e = (x, y, z);'};
  };

  ok(!$@,'pascalenumeratedvsrangesolvedviadyn executed as standalone modulino');

  $expected = qr{type_decl_is_TYPE_ID_type\(\s+TERMINAL,\s+TERMINAL,\s+ENUM\(\s+id_list_is_id_list_COMMA_ID\(\s+id_list_is_id_list_COMMA_ID\(\s+ID\(\s+TERMINAL\s+\),\s+TERMINAL\s+\),\s+TERMINAL\s+\)\s+\)\s+\)};

  like($r, $expected,'AST for type e = (x, y, z);');

  eval {
    $r = qx{t/dynamicresolution/persvd.pl -t -c 'type e = (x);'};
  };

  ok(!$@,'pascalenumeratedvsrangesolvedviadyn executed as standalone modulino');

  $expected = qr{type_decl_is_TYPE_ID_type\(\s+TERMINAL,\s+TERMINAL,\s+ENUM\(\s+ID\(\s+TERMINAL\s+\)\s+\)\s+\)};

  like($r, $expected,'AST for type e = (x);');

  unlink 't/dynamicresolution/persvd.pl';

}

