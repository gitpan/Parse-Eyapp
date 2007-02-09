#!/usr/bin/perl -w
use strict;
use Parse::Eyapp;
use Parse::Eyapp::Treeregexp;
use List::Util qw(reduce);

my $input =<< "EOI";
b = 5;
a = 2;
a = 2*(a+b)*(2-4/2);
print a;
d = (a = a+1)-b;
c = a*b;
print c;
print d
EOI

sub NUM::info { $_[0]{attr} }
{ no warnings; *VAR::info = \&NUM::info; }

my $grammar = q{
  %right  '='     # Lowest precedence
  %left   '-' '+' # + and - have more precedence than = Disambiguate a-b-c as (a-b)-c
  %left   '*' '/' # * and / have more precedence than + Disambiguate a/b/c as (a/b)/c
  %left   NEG     # Disambiguate -a-b as (-a)-b and not as -(a-b)
	%semantic token PRINT
  %tree bypass    # Let us build an abstract syntax tree ...

  %%
  line: sts <%name EXPRESION_LIST + ';'>  { $_[1] } /* list of expressions separated by ';' */
  ;

  /* The %name directive defines the name of the class to which the node being built belongs */
	sts: 
			%name PRINT 
			PRINT leftvalue
		| exp { $_[1] }
	;
  exp:
      %name NUM  NUM            | %name VAR   VAR         
    | %name ASSIGN leftvalue '=' exp 
    | %name PLUS exp '+' exp    | %name MINUS exp '-' exp | %name TIMES  exp '*' exp 
    | %name DIV     exp '/' exp 
    | %no bypass UMINUS 
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
    my($what)= $token ? "input: '$token'" : "end of input";
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
      m{\G\n}gc                      and $lineno++;
      m{\G([0-9]+(?:\.[0-9]+)?)}gc   and return('NUM',$1);
      m{\Gprint}gc                   and return('PRINT', 'PRINT');
      m{\G([A-Za-z][A-Za-z0-9_]*)}gc and return('VAR',$1);
      m{\G(.)}gc                     and return($1,$1);
      return('',undef);
    }
  }
}; # end grammar

my $transformations = q{
  { #  Example of support code
    use List::Util qw(reduce);
    my %Op = (PLUS=>'+', MINUS => '-', TIMES=>'*', DIV => '/');
  }
  algebraic_transformations = constantfold whatever_times_zero zero_times_whatever uminus;

  constantfold: /TIMES|PLUS|DIV|MINUS/:bin(NUM, NUM) 
    => { 
      my $op = $Op{ref($bin)};
      $NUM[0]->{attr} = eval  "$NUM[0]->{attr} $op $NUM[1]->{attr}";
      $_[0] = $NUM[0]; 
    }
  zero_times_whatever: TIMES(NUM, .) and { $NUM->{attr} == 0 } => { $_[0] = $NUM }
  whatever_times_zero: TIMES(., NUM) and { $NUM->{attr} == 0 } => { $_[0] = $NUM }
  uminus: UMINUS(NUM) => { $NUM->{attr} = -$NUM->{attr}; $_[0] = $NUM }

  name_it: .  
    => { 
      if (ref($W) =~ /VAR|NUM/) {
        $W->{name} = $W->{attr};
        return 1;
      }
      if (ref($W) =~ /ASSIGN/) {
        $W->{name} = $W->child(0)->{attr};
        return 1;
      }
      $_[0]->{name} = new_N_register(); 
    }

  translation = trans_num trans_var trans_op trans_uminus trans_assign trans_list trans_print;

  trans_num: NUM
    => {
      $_[0]->{trans} = $_[0]->{attr};
    }

  { 
    our %s; # Symbol table
  }
  trans_var: VAR
    => {
      $s{$_[0]->{attr}} = "num";
      $_[0]->{trans} = $_[0]->{attr};
    }

  trans_op:  /TIMES|PLUS|DIV|MINUS/:bin($left, $right) 
    => {
      my $op = $Op{ref($bin)};
      $bin->{trans} = "$bin->{name} = $left->{name} $op $right->{name}"; 
    }

  trans_uminus: UMINUS($exp)
    => {
      $UMINUS->{trans} = "$UMINUS->{name} = - $exp->{name}";
    }

  trans_assign: ASSIGN($var, $exp)
    => { 
      $ASSIGN->{trans} =  "$var->{name} = $exp->{name}" 
    }

	trans_print: PRINT(., $var)
	  => {
      $s{$var->{attr}} = "num";
		  $PRINT->{trans} =<<"EOP";
print "$var->{attr} = "
print $var->{attr}
print "\\\\n"
EOP
		}

  trans_list: EXPRESION_LIST(@S) 
    => {
      $EXPRESION_LIST->{trans} = "";
      my @trans = map { translate($_) } @S;
      $EXPRESION_LIST->{trans} = reduce { "$a\n$b" } @trans if @trans;
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
    $trans .= translate($_)."\n" unless ref($_) =~ m{\bNUM\b|\bVAR\b|\bTERMINAL\b};
  }
  $trans .= $t->{trans} ;
}

################# main ######################
Parse::Eyapp->new_grammar( # Create the parser package/class
  input=>$grammar,    
  classname=>'T2P', # The name of the package containing the parser
  firstline=>7       # String $grammar starts at line 7 (for error diagnostics)
); 
my $parser = T2P->new();                # Create a parser
$parser->YYData->{INPUT} = $input;
my $t = $parser->YYParse( yylex => \&T2P::Lex, yyerror => \&T2P::Err, );

print "\n************\n".$t->str."\n************\n";

my $p = Parse::Eyapp::Treeregexp->new( STRING => $transformations);
$p->generate(); # Create the tranformations

# Do machine independent optimizations
$t->s(our @algebraic_transformations);  

# Assign PARROT virtual registers to expressions
our $name_it;
$name_it->s($t);

# Translate to PARROT
$t->bud(our @translation);

#print $t->str,"\n";

# Prefix with variable declarations
$t->{trans} =~ s/^/\t/gm;
my $declarations = "";
our %s;
if (%s) {
	my @vars = sort keys %s;
	my $last = pop @vars;
	$declarations .= "$_, " for @vars;
	$declarations .= $last;
}

# Output the code
print << "TRANSLATION";
.sub 'main' :main
\t.local num $declarations
$t->{trans}
.end
TRANSLATION
#print Dumper($t);

