#!/usr/bin/perl 
use warnings;
use PostfixWithActions;

my $debug = shift || 0;
my $pparser = PostfixWithActions->new();
print "Write an expression: "; 
my $x = <STDIN>;

# First, translate to postfix ...
$pparser->Run($debug, $x);

# Restore input for the parser (the lexical
# analyzer was a destructive one)
$pparser->input(\$x);

# And then selectively substitute 
# some semantic actions
# to obtain an infix calculator ...

my %s;
$pparser->YYSetaction(
  ASSIGN => sub { $s{$_[1]} = $_[3] },
  PLUS   => sub { $_[1] + $_[3] },
  TIMES  => sub { $_[1] * $_[3] },
  DIV    => sub { $_[1] / $_[3] },
  NEG    => sub { -$_[2] },
);

$pparser->Run($debug);

