#!/usr/bin/perl 
use warnings;
use PostfixWithActions;

my $debug = shift || 0;
my $pparser = PostfixWithActions->new();
print "Write an expression: "; 
my $x = <STDIN>;

# First, translate to postfix ...
$pparser->Run($debug, $x);

exit(1) if $pparser->YYNberr;

# And then selectively substitute 
# some semantic actions
# to obtain an infix calculator ...

my %s;
$pparser->YYSetaction(
  'OP:ASSIGN'   => sub { $s{$_[1]} = $_[3] },
  'OP:PLUS'     => sub { $_[1] + $_[3] },
  'OP:TIMES'    => sub { $_[1] * $_[3] },
  'OP:DIV'      => sub { $_[1] / $_[3] },
  'OP:NEG'      => sub { -$_[2] },
);

$pparser->Run($debug, $x);

# Let su reuse the grammar a third time.
# Now we use it to generate the AST
$pparser->YYSetaction(
  'EXP'           => sub { $_[1] }, # bypass 
  'OPERAND:NUM'   => \&Parse::Eyapp::Driver::YYBuildAST,
  'OPERAND:VAR'   => \&Parse::Eyapp::Driver::YYBuildAST,
  'OP:ASSIGN'     => \&Parse::Eyapp::Driver::YYBuildAST,
  'OP:PLUS'       => \&Parse::Eyapp::Driver::YYBuildAST,
  'OP:TIMES'      => \&Parse::Eyapp::Driver::YYBuildAST,
  'OP:DIV'        => \&Parse::Eyapp::Driver::YYBuildAST,
  'OP:NEG'        => \&Parse::Eyapp::Driver::YYBuildAST,
);

*TERMINAL::info = sub { $_[0]{attr} };
my $t = $pparser->Run($debug, $x);
print $t->str."\n";

