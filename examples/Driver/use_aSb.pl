#!/usr/bin/perl -w
use strict;
use aSb;

my $p = aSb->new;
$p->slurp_file('','Try aaabbb: ', "\n");
$p->YYParse( yydebug => 0x1F );
