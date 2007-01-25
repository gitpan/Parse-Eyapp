#!/usr/bin/perl -w
use strict;
use Data::Dumper;
use Parse::Eyapp;
use IO::Interactive qw(is_interactive);

my $translationscheme = q{
%{
# head code is available at tree construction time 
use Data::Dumper;

our %sym; # symbol table
%}

%metatree

%right   '='
%left   '-' '+'
%left   '*' '/'

%%
line:       %name EXP  
              exp <+ ';'> /* Expressions separated by semicolons */ 
	        { $lhs->{n} = [ map { $_->{n}} $_[1]->Children() ]; }
;

exp:    
            %name PLUS  
              exp.left '+'  exp.right 
	        { $lhs->{n} = $left->{n} + $right->{n} }
        |   %name MINUS
	      exp.left '-' exp.right    
	        { $lhs->{n} = $left->{n} - $right->{n} }
        |   %name TIMES 
              exp.left '*' exp.right  
	        { $lhs->{n} = $left->{n} * $right->{n} }
        |   %name DIV 
              exp.left '/' exp.right  
	        { $lhs->{n} = $left->{n} / $right->{n} }
        |   %name NUM   $NUM          
	        { $lhs->{n} = $NUM->{attr} }
        |   '(' $exp ')'  %begin { $exp }       
        |   %name VAR
	      $VAR                 
	        { $lhs->{n} = $sym{$VAR->{attr}}->{n} }
        |   %name ASSIGN
	      $VAR '=' $exp         
	        { $lhs->{n} = $sym{$VAR->{attr}}->{n} = $exp->{n} }

;

%%
# tail code is available at tree construction time 
sub _Error {
  my($token)=$_[0]->YYCurval;
  my($what)= $token ? "input: '$token'" : "end of input";
  
  die "Syntax error near $what.\n";
}

sub _Lexer {
    my($parser)=shift;

    for ($parser->YYData->{INPUT}) {
        $_ or  return('',undef);

        s/^\s*//;
        s/^([0-9]+(?:\.[0-9]+)?)// and return('NUM',$1);
        s/^([A-Za-z][A-Za-z0-9_]*)// and return('VAR',$1);
        s/^(.)// and return($1,$1);
        s/^\s*//;
    }
}

sub Run {
    my($self)=shift;
    return $self->YYParse( yylex => \&_Lexer, yyerror => \&_Error );
}
}; # end translation scheme

$Data::Dumper::Indent = 1;
$Data::Dumper::Terse = 1;
$Data::Dumper::Deepcopy  = 1;
my $p = Parse::Eyapp->new_grammar(
  input=>$translationscheme,
  classname=>'main',
  firstline => 6,
  outputfile => 'main.pm');
die $p->qtables() if $p->Warnings;
my $parser = main->new();
print "Write a sequence of arithmetic expressions: " if is_interactive();
$parser->YYData->{INPUT} = <>;
my $t = $parser->Run() or die "Syntax Error analyzing input";
$t->translation_scheme;
my $treestring = Dumper($t);
our %sym;
my $symboltable = Dumper(\%sym);
print <<"EOR";
***********Tree*************
$treestring
******Symbol table**********
$symboltable
************Result**********
@{$t->{n}}

EOR
