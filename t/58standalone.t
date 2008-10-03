#!/usr/bin/perl -w
use strict;
use Test::More tests=>5;
#use_ok qw(Parse::Eyapp) or exit;

SKIP: {
  skip "Calc.eyp not found", 5 unless -r "t/Calc.eyp";
  unlink 't/Calc.pm';
  my $r = system('eyapp -s t/Calc.eyp');
  # With attributes
  ok(!$r, "standalone option");
  ok(-s "t/Calc.pm", ".pm generated with standalone");
  eval {
    require "t/Calc.pm";
  };
  ok(!$@, "standalone generated module loaded");

  my $warning = '';
  local $SIG{__WARN__} = sub { $warning = shift };
  eval {
    use_ok qw{Parse::Eyapp};
  };
  ok(!$warning, "Parse::Eyapp loaded on top of standalone without warnings");
}
