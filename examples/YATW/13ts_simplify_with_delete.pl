#!/usr/bin/perl -w
use strict;
use  Parse::Eyapp;
use  Parse::Eyapp::Treeregexp;

sub TERMINAL::info { $_[0]{attr} }
my $translationscheme = q{
  %lexer {
      s/^\s*//;

      s/^([0-9]+(?:\.[0-9]+)?)// and return('NUM',$1);
      s/^([A-Za-z][A-Za-z0-9_]*)// and return('VAR',$1);
      s/^(.)// and return($1,$1);
  }

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


$parser->slurp_file('', "Give an expression (like -2*3): ","\n");
print ${$parser->input}; # Set the input

my $t = $parser->YYParse;                    # Parse it

exit(1) if $parser->YYNberr > 0;

print $t->str."\n";                      # Show the tree

my $p = Parse::Eyapp::YATW->new(PATTERN => \&not_useful);
$p->s($t);
print $t->str."\n";                      # Show the tree

