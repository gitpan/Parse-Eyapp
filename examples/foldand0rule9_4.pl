#!/usr/bin/perl -w
# File: foldand0rule9_4.pl. Compile it with 
#          eyapp -m 'Calc' Rule9.yp; treereg -o T.pm -p 'R::' -m T Transform4
use strict;
use Calc;
use T;

sub R::TERMINAL::info { $_[0]{attr} }
my $parser = new Calc(yyprefix => "R::");
my $t = $parser->YYParse( yylex => \&Calc::Lexer, yyerror => \&Calc::Error);
print "\n***** Before ******\n";
print $t->str."\n";
$t->s(@T::all);
print "\n***** After ******\n";
print $t->str."\n";
