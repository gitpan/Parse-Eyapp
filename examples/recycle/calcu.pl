#!/usr/bin/perl 
use warnings;
use Noactions;

sub Calc::NUM::action {
  return $_[1];
}

sub Calc::PLUS::action {
  $_[1]+$_[3];
}

sub Calc::TIMES::action {
  $_[1]*$_[3];
}

my $debug = shift || 0;
my $parser = Noactions->new(yyprefix => 'Calc::');
print "Write an expression: "; 
my $x;
{
  local $/ = undef;
  $x = <STDIN>;
}
my $t = $parser->Run($x, $debug);

print "$t\n";
