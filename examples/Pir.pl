#!/usr/bin/perl -w
use strict;
use Parse::Eyapp;
use Parse::Eyapp::Treeregexp;
use List::Util qw(reduce);

my $input =<< "EOI";
b = 5;
a = b+2;
a = 2*(a+b)*(2-4/2);
print a;
d = (a = a+1)*4-b;
c = a*b+d;
print c;
print d
EOI

sub NUM::info { $_[0]{attr} }
{ no warnings; *VAR::info = \&NUM::info; }

my $grammar = q{
  %right  '='     
  %left   '-' '+' 
  %left   '*' '/' 
  %left   NEG    
  %semantic token PRINT
  %tree bypass   

  %%
  line: 
    sts <%name EXPRESION_LIST + ';'>  
      { $_[1] } 
  ;
  sts: 
      %name PRINT 
      PRINT leftvalue
    | exp { $_[1] }
  ;
  exp:
      %name NUM  NUM            
    | %name VAR   VAR         
    | %name ASSIGN leftvalue '=' exp 
    | %name PLUS exp '+' exp    
    | %name MINUS exp '-' exp 
    | %name TIMES  exp '*' exp 
    | %name DIV     exp '/' exp 
    | %no bypass NEG 
      '-' $exp %prec NEG 
    |   '(' exp ')'  
  ;
  leftvalue : %name VAR VAR
  ;
  %%
  my $lineno = 1;

  sub Err {
    my $parser = shift;

    my($token)=$parser->YYCurval;
    my($what)= $token ? "input: '$token'" 
                      : "end of input";
    my @expected = $parser->YYExpect();
    local $" = ', ';
    die << "ERRMSG";
Syntax error near $what (line number $lineno). 
Expected one of these terminals: @expected
ERRMSG
  }

  sub Lex {
    my($parser)=shift; # The parser object

    for ($parser->YYData->{INPUT}) { # Topicalize
      m{\G[ \t]*}gc;
      m{\G\n}gc                      
        and $lineno++;
      m{\G([0-9]+(?:\.[0-9]+)?)}gc   
        and return('NUM',$1);
      m{\Gprint}gc                   
        and return('PRINT', 'PRINT');
      m{\G([A-Za-z][A-Za-z0-9_]*)}gc 
        and return('VAR',$1);
      m{\G(.)}gc                     
        and return($1,$1);
      return('',undef);
    }
  }
}; # end grammar

my $transformations = q{
  { #  Example of support code
    use List::Util qw(reduce);
    my %Op = (PLUS=>'+', MINUS => '-', 
              TIMES=>'*', DIV => '/');
  }
  algebra = fold wxz zxw neg;

  fold: /TIMES|PLUS|DIV|MINUS/:bin(NUM, NUM) 
    => { 
      my $op = $Op{ref($bin)};
      $NUM[0]->{attr} = 
        eval  "$NUM[0]->{attr} $op $NUM[1]->{attr}";
      $_[0] = $NUM[0]; 
    }
  zxw: TIMES(NUM, .) and { $NUM->{attr} == 0 }
    => { $_[0] = $NUM }
  wxz: TIMES(., NUM) and { $NUM->{attr} == 0 } 
    => { $_[0] = $NUM }
  neg: NEG(NUM) 
    => { $NUM->{attr} = -$NUM->{attr}; 
         $_[0] = $NUM }

  reg_assign: .  
    => { 
      if (ref($W) =~ /VAR|NUM/) {
        $W->{reg} = $W->{attr};
        return 1;
      }
      if (ref($W) =~ /ASSIGN/) {
        $W->{reg} = $W->child(0)->{attr};
        return 1;
      }
      $_[0]->{reg} = new_N_register(); 
    }


  translation = t_num t_var t_op t_neg 
               t_assign t_list t_print;

  t_num: NUM => { $_[0]->{trans} = $_[0]->{attr}; }
  { our %s; }
  t_var: VAR => {
      $s{$_[0]->{attr}} = "num";
      $_[0]->{trans} = $_[0]->{attr};
    }
  t_op:  /TIMES|PLUS|DIV|MINUS/:bin($left, $right) 
    => {
      my $op = $Op{ref($bin)};
      $bin->{trans} = "$bin->{reg} = $left->{reg} "
                     ."$op $right->{reg}"; 
    }
  t_neg: NEG($exp) => {
    $NEG->{trans} = "$NEG->{reg} = - $exp->{reg}";
  }

  t_assign: ASSIGN($var, $exp) => { 
    $s{$var->{attr}} = "num";
    $ASSIGN->{trans} = "$var->{reg} = $exp->{reg}" 
  }

  { my $cr = '\\n'; }
  t_print: PRINT(., $var)
    => {
      $s{$var->{attr}} = "num";
      $PRINT->{trans} =<<"EOP";
print "$var->{attr} = "
print $var->{attr}
print "$cr"
EOP
    }

  t_list: EXPRESION_LIST(@S) 
    => {
      $EXPRESION_LIST->{trans} = "";
      my @trans = map { translate($_) } @S;
      $EXPRESION_LIST->{trans} = 
        reduce { "$a\n$b" } @trans if @trans;
    }

};

{ my $num = 1; # closure
  sub new_N_register {
    return '$N'.$num++;
  }
}

sub translate {
  my $t = shift;

  my $trans = "";
  for ($t->children) {
    (ref($_) =~ m{\bNUM\b|\bVAR\b|\bTERMINAL\b}) 
      or $trans .= translate($_)."\n" 
  }
  $trans .= $t->{trans} ;
}

sub build_dec {
  our %s;
  my $dec = "";
  if (%s) {
    my @vars = sort keys %s;
    my $last = pop @vars;
    $dec .= "$_, " for @vars;
    $dec .= $last;
  }
  return $dec;
}

################# main ######################
Parse::Eyapp->new_grammar( 
  input=>$grammar,    
  classname=>'T2P', 
  firstline=>7     
); 
my $parser = T2P->new(); 
$parser->YYData->{INPUT} = $input;
my $t = $parser->YYParse( 
  yylex => \&T2P::Lex, yyerror => \&T2P::Err);
#print "\n************\n".$t->str."\n************\n";

my $p = Parse::Eyapp::Treeregexp->new( 
           STRING => $transformations
        )->generate(); 

# Machine independent optimizations
$t->s(our @algebra);  

# Address Assignment 
our $reg_assign;
$reg_assign->s($t);

# Translate to PARROT
$t->bud(our @translation);
#print $t->str,"\n";

# Peephole Optimization
$t->{trans} =~ 
  s{(\$N\d+) = (.*\n)\s*([a-zA-Z]\w*) = \1\n}
   {$3 = $2}g;

# Indent
$t->{trans} =~ s/^/\t/gm;
# Prefix with variable declarations
my $dec = build_dec();

# Output the code
print << "TRANSLATION";
.sub 'main' :main
\t.local num $dec
$t->{trans}
.end
TRANSLATION
#print Dumper($t);
