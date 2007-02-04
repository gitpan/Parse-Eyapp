#!/usr/bin/perl -w
use strict;
use Calc;

my $parser = Calc->new();
my $input = <<'EOI';
a = 2*3
d = 5/(a-6)
b = (a+1)/7
c=a*3+4)-5
a = a+1
EOI
my $t = $parser->Run(\$input);
print "========= Symbol Table ==============\n";
print "$_ = $t->{$_}\n" for sort keys %$t;
