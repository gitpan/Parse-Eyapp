#!/usr/bin/perl -w
use strict;
# compile it with eyapp Rule9
use Rule9;
use Data::Dumper;
# compile it with: 
#          treereg -p 'Rule9::' Transform4
use Transform4;

$Data::Dumper::Indent = 1;
my $parser = new Rule9(yyprefix => "Rule9::");
my $t = $parser->YYParse( yylex => \&Rule9::Lexer, yyerror => \&Rule9::Error, 
		    #yydebug =>0xFF
		  );
print "\n***** Before ******\n";
print Dumper($t);
$t->s(@Transform4::all);
print "\n***** After ******\n";
print Dumper($t);
