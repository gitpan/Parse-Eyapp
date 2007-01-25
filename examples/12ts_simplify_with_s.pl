#!/usr/bin/perl -w
# Test YATW s
use strict;
use Parse::Eyapp;
use Parse::Eyapp::Treeregexp;

my $translationscheme = q{
  %{
  # head code is available at tree construction time 
  %}

  %defaultaction  { $lhs->{n} = $_[1]->{n} }
  %metatree

  %left   '-' '+'
  %left   '*' 
  %left   NEG

  %%
  line:       %name EXP  
                exp  
  ;

  exp:    
              %name PLUS  
                exp.left '+'  exp.right 
            { $lhs->{n} .= $left->{n} + $right->{n} }
          |   %name TIMES 
                exp.left '*' exp.right  
            { $lhs->{n} = $left->{n} * $right->{n} }
          |   %name NUM   $NUM          
            { $lhs->{n} = $NUM->{attr} }
          |   '(' $exp ')'  %begin { $exp }       
          |   %name MINUS
          exp.left '-' exp.right    
            { $lhs->{n} = $left->{n} - $right->{n} }

          |   %name UMINUS 
          '-' $exp %prec NEG        
            { $lhs->{n} = -$exp->{n} }
  ;

  %%
  # tail code is available at tree construction time 
  sub _Error { die "Syntax error.\n"; }

  sub _Lexer {
      my($parser)=shift;

      $parser->YYData->{INPUT} or  return('',undef);

      $parser->YYData->{INPUT}=~s/^\s*//;

      for ($parser->YYData->{INPUT}) {
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

sub is_code {
  my $self = shift; # tree

  # $_[0] is the father, $_[1] the index
  if ((ref($self) eq 'CODE')) {
    splice(@{$_[0]->{children}}, $_[1], 1);
    return 1;
  }
  return 0;
}

Parse::Eyapp->new_grammar(
  input=>$translationscheme,
  classname=>'Calc', 
  firstline =>7,
); 
my $parser = Calc->new();                # Create the parser

$parser->YYData->{INPUT} = "2*-3\n";  print "2*-3\n"; # Set the input
my $t = $parser->Run;                    # Parse it
print $t->str."\n";
my $p = Parse::Eyapp::YATW->new(PATTERN => \&is_code);
$p->s($t);
{ no warnings; # make attr info available only for this display
  local *TERMINAL::info = sub { $_[0]{attr} };
  print $t->str."\n";
}
