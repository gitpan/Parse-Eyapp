#!/usr/bin/perl -w
use strict;
use  Parse::Eyapp;
use  Parse::Eyapp::Treeregexp;

sub TERMINAL::info { $_[0]{attr} }
my $translationscheme = q{
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
    |   '(' $exp ')'  
           %begin { $exp }       
    |   %name MINUS
        exp.left '-' exp.right    
          { $lhs->{n} = $left->{n} - $right->{n} }

    |   %name UMINUS 
        '-' $exp %prec NEG        
          { $lhs->{n} = -$exp->{n} }
  ;

  %%
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

sub not_useful {
  my $self = shift; # node
  my $pat = $_[2];  # get the YATW object

  (ref($self) eq 'CODE') or ((ref($self) eq 'TERMINAL') and ($self->{token} eq $self->{attr}))
    or do { return 0 };
  $pat->delete();
  return 1;
}

Parse::Eyapp->new_grammar(
  input=>$translationscheme,
  classname=>'Calc', 
  firstline =>7,
); 
my $parser = Calc->new();                # Create the parser

$parser->YYData->{INPUT} = "2*3\n"; print $parser->YYData->{INPUT}; 
my $t = $parser->Run;                    # Parse it
print $t->str."\n";                      # Show the tree
my $p = Parse::Eyapp::YATW->new(PATTERN => \&not_useful);
$p->s($t);
print $t->str."\n";                      # Show the tree

