#!/usr/bin/perl -w
use strict;
use Test::More tests=>8;
#use_ok qw(Parse::Eyapp) or exit;

SKIP: {
  skip "Calc.eyp not found", 8 unless -r "t/Calc.eyp";

  unlink 't/Calc.pm';

  my $r = system('eyapp -s t/Calc.eyp');
  
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

  my $parser = Calc->new();
  my $input = "a = 3*2\nb = 4*a\nc = a*b\n";
  my $t = $parser->Run(\$input);
  my %r = ( a => 6, b => 24, c => 144);
  is($t->{$_}, $r{$_}, "Using calc: $_ is $r{$_}") for (qw{a b c})
}
