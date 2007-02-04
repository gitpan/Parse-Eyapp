#
# Module Parse::Eyapp.pm.
#
#
package Parse::Eyapp;

use strict;
our @ISA = qw(Parse::Eyapp::Output);
use Parse::Eyapp::Output;

# $VERSION is in Parse/Eyapp/Driver.pm
our $VERSION = $Parse::Eyapp::Driver::VERSION;

1;

__END__

=head1 NAME
 
Parse::Eyapp  - Extensions for Parse::Yapp
 
 
=head1 VERSION
 
1.06555

=head1 SYNOPSIS
 
 use strict;
 use Parse::Eyapp;
 use Parse::Eyapp::Treeregexp;

 sub TERMINAL::info {
   $_[0]{attr}
 }

 my $grammar = q{
   %right  '='     # Lowest precedence
   %left   '-' '+' # + and - have more precedence than = Disambiguate a-b-c as (a-b)-c
   %left   '*' '/' # * and / have more precedence than + Disambiguate a/b/c as (a/b)/c
   %left   NEG     # Disambiguate -a-b as (-a)-b and not as -(a-b)
   %tree           # Let us build an abstract syntax tree ...

   %%
   line: exp <%name EXPRESION_LIST + ';'>  { $_[1] } /* list of expressions separated by ';' */
   ;

   /* The %name directive defines the name of the class to which the node being built belongs */
   exp:
       %name NUM  NUM            | %name VAR   VAR         | %name ASSIGN VAR '=' exp 
     | %name PLUS exp '+' exp    | %name MINUS exp '-' exp | %name TIMES  exp '*' exp 
     | %name DIV     exp '/' exp | %name UMINUS '-' exp %prec NEG 
     |   '(' exp ')'  { $_[2] }  /* Let us simplify a bit the tree */
   ;

   %%
   sub _Error { die "Syntax error near ".($_[0]->YYCurval?$_[0]->YYCurval:"end of file")."\n" }

   sub _Lexer {
     my($parser)=shift; # The parser object

     for ($parser->YYData->{INPUT}) {
       s/^\s+//;
       $_ eq '' and return('',undef);
       s/^([0-9]+(?:\.[0-9]+)?)// and return('NUM',$1);
       s/^([A-Za-z][A-Za-z0-9_]*)// and return('VAR',$1);
       s/^(.)//s and return($1,$1);
     }
   }

   sub Run {
       my($self)=shift;
       $self->YYParse( yylex => \&_Lexer, yyerror => \&_Error, );
   }
 }; # end grammar

 our (@all, $uminus);

 Parse::Eyapp->new_grammar( # Create the parser package/class
   input=>$grammar,    
   classname=>'Calc', # The name of the package containing the parser
   firstline=>7       # String $grammar starts at line 7 (for error diagnostics)
 ); 
 my $parser = Calc->new();                # Create a parser
 $parser->YYData->{INPUT} = "2*-3+b*0;--2\n"; # Set the input
 my $t = $parser->Run;                    # Parse it!
 local $Parse::Eyapp::Node::INDENT=2;
 print "Syntax Tree:",$t->str;

 # Let us transform the tree. Define the tree-regular expressions ..
 my $p = Parse::Eyapp::Treeregexp->new( STRING => q{
     { #  Example of support code
       my %Op = (PLUS=>'+', MINUS => '-', TIMES=>'*', DIV => '/');
     }
     constantfold: /TIMES|PLUS|DIV|MINUS/:bin(NUM($x), NUM($y)) 
       => { 
         my $op = $Op{ref($bin)};
         $x->{attr} = eval  "$x->{attr} $op $y->{attr}";
         $_[0] = $NUM[0]; 
       }
     uminus: UMINUS(NUM($x)) => { $x->{attr} = -$x->{attr}; $_[0] = $NUM }
     zero_times_whatever: TIMES(NUM($x), .) and { $x->{attr} == 0 } => { $_[0] = $NUM }
     whatever_times_zero: TIMES(., NUM($x)) and { $x->{attr} == 0 } => { $_[0] = $NUM }
   },
   OUTPUTFILE=> 'main.pm'
 );
 $p->generate(); # Create the tranformations

 $t->s($uminus); # Transform UMINUS nodes
 $t->s(@all);    # constant folding and mult. by zero

 local $Parse::Eyapp::Node::INDENT=0;
 print "\nSyntax Tree after transformations:\n",$t->str,"\n";
  
=head1 Introduction

Parse::Eyapp (Extended yapp) is a collection of modules
that extends Francois Desarmenien Parse::Yapp 1.05.
Eyapp extends yacc/yapp syntax with 
functionalities line named attributes,
EBNF-like expressions, modifiable default action,
automatic syntax tree building,
semi-automatic abstract syntax tree building,
translation schemes, tree regular expressions,
tree transformations, scope analysis support,
directed acyclic graphs and a few more.

=head1 The Eyapp Language

=head2 Eyapp Grammar 

This section describes the syntax of the Eyapp language using its own notation.
The grammar is compatible with yacc and yapp grammars.
Semicolons have been omitted to save space.
Between C-like comments you can find an (informal) 
explanation of the language 
associated with the token.


  eyapp: head body tail ;
  symbol: LITERAL  /* A string literal like 'hello' */
      |   ident   
  ident:  IDENT  /* IDENT is [A-Za-z_][A-Za-z0-9_]* */ 
  head: headsec '%%'
  headsec:  decl *
  decl:  '\n'      
      |   SEMANTIC typedecl symlist '\n'  /* SEMANTIC  is %semantic\s+token      */
      |   SYNTACTIC typedecl symlist '\n' /* SYNTACTIC is %syntactic\s+token     */
      |   TOKEN typedecl symlist '\n'     /* TOKEN     is %token                 */
      |   ASSOC typedecl symlist '\n'     /* ASSOC     is %(left|right|nonassoc) */
      |   START ident '\n'                /* START     is %start                 */
      |   HEADCODE '\n'                   /* HEADCODE  is %{ Perl code ... %}    */
      |   UNION CODE '\n'                 /* UNION CODE  see yacc/bison          */
      |   DEFAULTACTION CODE '\n'         /* DEFAULTACTION is %defaultaction     */
      |   TREE treeclauses? '\n'          /* TREE      is %tree                  */
      |   METATREE '\n'                   /* METATREE  is %metatree              */
      |   TYPE typedecl identlist '\n'    /* TYPE      is %type                  */
      |   EXPECT NUMBER '\n'              /* EXPECT    is %expect                */
                                          /* NUMBER    is \d+                    */
  typedecl:   /* empty */
      |       '<' IDENT '>'
  treeclauses: BYPASS ALIAS? | ALIAS BYPASS?
  symlist:    symbol + 
  identlist:  ident +
  body: rules * '%%'
  rules: IDENT ':' rhss ';'  
  rhss: rule <+ '|'>  
  rule:   optname rhs (prec epscode)?
  rhs:  rhseltwithid *
  rhseltwithid : 
        rhselt '.' IDENT 
      | '$' rhselt  
      | rhselt
  rhselt:     symbol    
      | code    
      | '(' optname rhs ')' 
      | rhselt STAR               /* STAR   is (%name\s*([A-Za-z_]\w*)\s*)?\*  */
      | rhselt '<' STAR symbol '>' 
      | rhselt OPTION             /* OPTION is (%name\s*([A-Za-z_]\w*)\s*)?\?  */
      | rhselt '<' PLUS symbol '>'
      | rhselt PLUS               /* PLUS   is (%name\s*([A-Za-z_]\w*)\s*)?\+  */
  optname: (NAME IDENT)?          /* NAME is %name */
         | NOBYPASS IDENT         /* NOBYPASS is %no\s+bypass */
  prec: PREC symbol               /* PREC is %prec */
  epscode:  code ?   
  code:   
      CODE           /* CODE     is { Perl code ... }         */
    | BEGINCODE      /* BEGINCODE is %begin { Perl code ... } */
  tail:  TAILCODE ?  /* TAILCODE is { Perl code ... } */

The semantic of C<Eyapp> agrees with the semantic of C<yacc> and C<yapp>
for all the common constructions. For an introduction to the extensions
see L<eyapptut>. 

If you are already familiar with L<Parse::Yapp>, yacc or L<Parse::RecDescent> you'll have no
problem understanding the former description. 

=head2 The head section

An Eyapp program has three parts: 

                                 eyapp: head body tail ;

Each part is separated by the symbol C<%%>:

                                 head: headsec '%%'

The head section contains a list of declarations 

                                 headsec:  decl *

There are different kinds of declarations. This reference does not 
fully describes those declarations that are shared with L<yacc> and 
L<yapp>. 
In this and the incoming sections we will describe the basics
of the Eyapp language using the file C<examples/Calc.eyp> 
that accompanies this distribution. This file implements a trivial 
calculator. Here is the header section:

 pl@nereida:~/src/perl/YappWithDefaultAction/examples$ sed -ne '1,11p' Calc.eyp | cat -n
  1  # examples/Calc.eyp
  2  %right  '='
  3  %left   '-' '+'
  4  %left   '*' '/'
  5  %left   NEG
  6  %right  '^'
  7  %{
  8  my %s; # symbol table
  9  %}
 10
 11  %%

Lines 2-5 declare several tokens. The iusual way to declare
tokens is through the C<%token> directive. The declarations 
C<%nonassco>, C<%left> and C<%right> 
not only declare the tokens but also associate a I<priority> with them.  
Tokens declared in the same line have the same precedence. 
Tokens declared with these directives in lines below have more
precedence than thos declared above. Thus, in the example
we are saying that C<"+"> and C<"-"> have the same precedence
but higher precedence than C<"=">. The final effect of C<"-">
having greater precedence than C<"="> will be that an
expression like:

                        a = 4 - 5

will be interpreted as
 
                        a = (4 -5)

and not as

                        (a = 4) - 5

The use of the C<%left> indicates that - in case of ambiguity 
and a match between precedences - the parser must build the tree corresponding
to a left parenthesization. Thus, the expression

                         4 - 5 - 9

will be interpreted as

                         (4 - 5) - 9

Perl code surrounded by C<%{> and C<%}>
can be inserted in this section. Such code will be inserted in the module
generated by C<eyapp> near the beginning. Therefore, declarations like the
one of the calculator symbol table C<%s>

  7  %{
  8  my %s; # symbol table
  9  %}

will be visible from almost any point in the parser.


=head2 The Body

The body section contains the production rules describing the grammar:

  body:   rules * '%%'
  rules:  IDENT ':' rhss ';'  
  rhss:   (optname rhs (prec epscode)?) <+ '|'>  

The body of our calculator example is:

 pl@nereida:~/src/perl/YappWithDefaultAction/examples$ sed -ne '12,48p' Calc.eyp | cat -n
  1  start:
  2      input { \%s }
  3  ;
  4
  5  input: line *
  6  ;
  7
  8  line:
  9    '\n'         { undef }
 10    | exp '\n'   { print "$_[1]\n" if defined($_[1]); $_[1] }
 11    | error  '\n'
 12        {
 13          $_[0]->YYErrok;
 14          undef
 15        }
 16  ;
 17
 18  exp:
 19      NUM
 20    | VAR                 { $s{$_[1]} }
 21    | VAR '=' exp         { $s{$_[1]} = $_[3] }
 22    | exp '+' exp         { $_[1] + $_[3] }
 23    | exp '-' exp         { $_[1] - $_[3] }
 24    | exp '*' exp         { $_[1] * $_[3] }
 25    | exp '/' exp
 26      {
 27         $_[3] and return($_[1] / $_[3]);
 28         $_[0]->YYData->{ERRMSG} = "Illegal division by zero.\n";
 29         $_[0]->YYError; # Pretend that a syntactic error ocurred: _Error will be called
 30         undef
 31      }
 32    | '-' exp %prec NEG   { -$_[2] }
 33    | exp '^' exp         { $_[1] ** $_[3] }
 34    | '(' exp ')'         { $_[2] }
 35  ;
 36
 37  %%


This example does not uses any of the Eyapp extensions (with the exception of the 
I<star list> at line 5). Please, see the L<Parse::Yapp> pages and elsewhere documentation
on L<yacc> and L<bison> for more information.

=head2 The Tail

The tail section contains Perl code. Usually the lexical analyzer and the
Error management subrotuines go here:

 pl@nereida:~/src/perl/YappWithDefaultAction/examples$ sed -ne '48,$p' Calc.eyp | cat -n
  1  %%
  2
  3  my $lineno = 1;
  4
  5  sub _Error {
  6    my $parser = shift;
  7
  8      exists $parser->YYData->{ERRMSG}
  9    and do {
 10        print $parser->YYData->{ERRMSG};
 11        delete $parser->YYData->{ERRMSG};
 12        return;
 13    };
 14    my($token)=$parser->YYCurval;
 15    my($what)= $token ? "input: '$token'" : "end of input";
 16    my @expected = $parser->YYExpect();
 17    local $" = ', ';
 18    print << "ERRMSG";
 19
 20  Syntax error near $what (lin num $lineno).
 21  Expected one of these terminals: @expected
 22  ERRMSG
 23  }
 24
 25  sub echo {
 26    my $parser = shift;
 27
 28    print $_[1] unless $parser->YYRecovering;
 29    return @_;
 30  }
 31
 32  sub make_lexer {
 33    my $input = shift;
 34
 35    return sub {
 36      my $parser = shift;
 37
 38      for ($$input) {
 39        m{\G[ \t]*}gc;
 40        m{\G([0-9]+(?:\.[0-9]+)?)}gc   and return $parser->echo('NUM',$1);
 41        m{\G([A-Za-z][A-Za-z0-9_]*)}gc and return $parser->echo('VAR',$1);
 42        m{\G\n}gc                      and do { $lineno++; return $parser->echo("\n", "\n") };
 43        m{\G(.)}gc                     and return $parser->echo($1,$1);
 44
 45        return('',undef);
 46      }
 47    }
 48  }
 49
 50  sub Run {
 51      my($self)=shift;
 52      my $input = shift or die "No input given\n";
 53
 54      return $self->YYParse( yylex => make_lexer($input), yyerror => \&_Error,
 55        #yydebug =>0x1F
 56      );
 57  }

This example does not uses any of the Eyapp extensions. 
Please, see the L<Parse::Yapp> pages and elsewhere documentation
on L<yacc> and L<bison> for more information.

=head2 Using an Eyapp Program

The following is an example of program that uses the calculator expalined 
in the two rpevious sections:

 pl@nereida:~/src/perl/YappWithDefaultAction/examples$ cat -n usecalc.pl
  1  #!/usr/bin/perl -w
  2  use strict;
  3  use Calc;
  4
  5  my $parser = Calc->new();
  6  my $input = <<'EOI';
  7  a = 2*3
  8  d = 5/(a-6)
  9  b = (a+1)/7
 10  c=a*3+4)-5
 11  a = a+1
 12  EOI
 13  my $t = $parser->Run(\$input);
 14  print "========= Symbol Table ==============\n";
 15  print "$_ = $t->{$_}\n" for sort keys %$t;

The output for this program is:

 pl@nereida:~/src/perl/YappWithDefaultAction/examples$ usecalc.pl
 a=2*3
 6
 d=5/(a-6)
 Illegal division by zero.
 b=(a+1)/7
 1
 c=a*3+4)
 Syntax error near input: ')' (lin num 4).
 Expected one of these terminals: -, /, ^, *, +,

 a=a+1
 7
 ========= Symbol Table ==============
 a = 7
 b = 1
 c = 22

=head2 Lists and Optionals

=over

=item *
In Eyapp the C<+> operator indicates one or more repetitions of the element
to the left of C<+>, thus a rule like:

                        decls:  decl +

is the same as:

                        decls:  decls decl 
                             |  decl

An additional  symbol may be included  to indicate lists of elements 
separated by such symbol. Thus

                       rhss: rule <+ '|'>  

is equivalent to:

                       rhss: rhss '|' rule 
                           | rule

=item *
The operators C<*> and C<?> have their usual meaning: 0 or more for
C<*> and optionality for C<?>. Is legal to parenthesize 
a C<rhs> expression as in:

                       optname: (NAME IDENT)?

=back
 
=head2 Names for attributes

Attributes can be referenced by meaningful names instead
of the classic error-prone positional approach using the I<dot notation>
like in:

              exp : exp.left '-' exp.right  { $left - $right }

By qualifying the first appearance of the syntactic variable C<exp>
with the notation C<exp.left> we can later refer inside the actions
to the associated attribute using the lexical variable
C<$left>. Thus the former code is equivalent to:
 
                 exp '-' exp
                  { 
                    my $lhs = shift; 
                    my ($left, $right) = @_[1, 3];
                    $lhs->{n} = $left->{n} - $right->{n} 
                  }


The I<dolar notation> C<$A> can be used as an abbreviation
of C<A.A>. For example, the code:

                $VAR '=' $exp
                  { $lhs->{n} = $sym{$VAR->{attr}}->{n} = $exp->{n} }

is equivalent to:

                  VAR '=' exp
                  { 
                    my $lhs = shift;
                    my ($VAR, $exp) = @_[1, 3];
                    $lhs->{n} = $sym{$VAR->{attr}}->{n} = $exp->{n} 
                  }

=head2 Default actions

When no action is specified both C<yapp> and C<eyapp>
implicitly insert the semantic action C<{ $_[1] }>. 
In C<Parse::Eyapp> you can modify such behavior using the C<%defaultaction { Perl code }>
directive. The C<{ Perl code }> clause that follows the C<%defaultaction>
directive is
executed when reducing by any production for which no explicit
action was specified.

See an example that translates an infix expression
like C<a=b*-3> into a postfix expression like C<a b 3 NEG * = >:

 # File Postfix.eyp (See the examples/ directory)
 %right  '='
 %left   '-' '+'
 %left   '*' '/'
 %left   NEG

 %defaultaction { return  "$left $right $op"; }

 %%
 line: $exp  { print "$exp\n" }
 ;

 exp:        $NUM  { $NUM }
         |   $VAR  { $VAR }
         |   VAR.left '='.op exp.right
         |   exp.left '+'.op exp.right
         |   exp.left '-'.op exp.right
         |   exp.left '*'.op exp.right
         |   exp.left '/'.op exp.right
         |   '-' $exp %prec NEG { "$exp NEG" }
         |   '(' $exp ')' { $exp }
 ;

 %%

 # Support subroutines as in the Synopsis example
 ...

The file containing the C<Eyapp> program must be compiled with C<eyapp>:

 nereida:~/src/perl/YappWithDefaultAction/examples> eyapp Postfix.eyp

Next, you have to write a client program:

 nereida:~/src/perl/YappWithDefaultAction/examples> cat -n usepostfix.pl
      1  #!/usr/bin/perl -w
      2  use strict;
      3  use Postfix;
      4
      5  my $parser = new Postfix();
      6  $parser->Run;

Now we can run the client program:

 nereida:~/src/perl/YappWithDefaultAction/examples> usepostfix.pl
 Write an expression: -(2*a-b*-3)
 2 a * b 3 NEG * - NEG

=head2 Abstract Syntax Trees : C<%tree> and C<%name>

C<Parse::Eyapp> facilitates the construction of concrete syntax trees and 
abstract syntax trees (abbreviated AST from now on) through the C<%tree>
directive. 
Nodes in the AST are blessed in the production
C<name>. 
By default the name of a production is the concatenation
of the left hand side and the production number. The production number
is the ordinal number of the production as they appear in the associated 
C<.output> file (see option C<-v> of L<eyapp>). For example,
given the grammar:

 pl@nereida:~/src/perl/YappWithDefaultAction/examples$ sed -ne '9,28p' treewithoutnames.pl
 my $grammar = q{
   %right  '='     # Lowest precedence
   %left   '-' '+' # + and - have more precedence than = Disambiguate a-b-c as (a-b)-c
   %left   '*' '/' # * and / have more precedence than + Disambiguate a/b/c as (a/b)/c
   %left   NEG     # Disambiguate -a-b as (-a)-b and not as -(a-b)
   %tree           # Let us build an abstract syntax tree ...

   %%
   line: exp <+ ';'>  { $_[1] } /* list of expressions separated by ';' */
   ;

   exp:
        NUM           |   VAR       | VAR '=' exp
     | exp '+' exp    | exp '-' exp |  exp '*' exp
     | exp '/' exp
     | '-' exp %prec NEG
     |   '(' exp ')'  { $_[2] }
   ;


The tree produced by the parser when feed with input C<a=2*b>
is:

 _PLUS_LIST(exp_6(TERMINAL[a],exp_9(exp_4(TERMINAL[2]),exp_5(TERMINAL[b]))))


If we want to see the correspondence between names and rules we can generate and
check the corresponding file C<.output>:

 pl@nereida:~/src/perl/YappWithDefaultAction/examples$ sed -ne '28,42p' treewithoutnames.output
 Rules:
 ------
 0:      $start -> line $end
 1:      PLUS-1 -> PLUS-1 ';' exp
 2:      PLUS-1 -> exp
 3:      line -> PLUS-1
 4:      exp -> NUM
 5:      exp -> VAR
 6:      exp -> VAR '=' exp
 7:      exp -> exp '+' exp
 8:      exp -> exp '-' exp
 9:      exp -> exp '*' exp
 10:     exp -> exp '/' exp
 11:     exp -> '-' exp
 12:     exp -> '(' exp ')'

We can see now that the node C<exp_9> corresponds to the rule C<exp -E<gt> exp '*' exp>.
Observe also that the Eyapp rule:

                                line: exp <+ ';'>
actually produces the rules:

                        1:      PLUS-1 -> PLUS-1 ';' exp
                        2:      PLUS-1 -> exp

and that the name of the class associated with the non empty list is C<_PLUS_LIST>.

A production rule can be 
I<named> using the C<%name IDENTIFIER> directive. 
For each production rule a 
namespace/package is created. I<The> C<IDENTIFIER>
I<is the name of the associated package>.
Therefore, by modifying the former grammar with 
additional C<%name> directives:

 pl@nereida:~/src/perl/YappWithDefaultAction/examples$ sed -ne '8,26p' treewithnames.pl
 my $grammar = q{
   %right  '='     # Lowest precedence
   %left   '-' '+' # + and - have more precedence than = Disambiguate a-b-c as (a-b)-c
   %left   '*' '/' # * and / have more precedence than + Disambiguate a/b/c as (a/b)/c
   %left   NEG     # Disambiguate -a-b as (-a)-b and not as -(a-b)
   %tree           # Let us build an abstract syntax tree ...

   %%
   line: exp <%name EXPS + ';'>  { $_[1] } /* list of expressions separated by ';' */
   ;

   exp:
       %name NUM    NUM           | %name VAR   VAR         | %name ASSIGN VAR '=' exp
     | %name PLUS   exp '+' exp   | %name MINUS exp '-' exp | %name TIMES  exp '*' exp
     | %name DIV    exp '/' exp
     | %name UMINUS '-' exp %prec NEG
     |   '(' exp ')'  { $_[2] }
   ;


we are explictly naming the productions. Thus, all the node instances 
corresponding to the 
production C<exp: VAR '=' exp> will belong to the class C<ASSIGN>. Now 
the tree for C<a=2*b> becomes:

          EXPS(ASSIGN(TERMINAL[a],TIMES(NUM(TERMINAL[2]),VAR(TERMINAL[b]))))

Observe how the list has been named C<EXPS>. The C<%name> directive prefixes the 
list operator (C<[+*?]>).

=head3 About the Encapsulation of Nodes

There is no encapsulation of nodes. The user/client 
knows that they are hashes that can be decorated with new keys/attributes.
All nodes in the AST created by C<%tree> are C<Parse::Eyapp::Node> nodes.
The only reserved field is C<children> which is a reference to the
array of children. You can always create a C<Node> class 
I<by hand> by inheriting from C<Parse::Eyapp::Node>. See 
section L<Separated Compilation with eyapp and treereg> for an example.

=head3 TERMINAL Nodes

Nodes named C<TERMINAL> are built from the
tokens provided by the lexical analyzer. 
C<Parse::Eyapp> follows the same protocol
than L<Parse::Yapp> for communication between the parser and the lexer:
A couple C<($token, $attribute)> is returned by the lexer.
These values are stored under the keys C<token> and C<attr>.
C<TERMINAL> nodes as all C<Parse::Eyapp::Node> nodes
also have the attribute C<children> but is - almost always - empty.


=head3 Explicit Actions Inside C<%tree>

Explicit actions can be specified by the programmer like in this line
from the L</SYNOPSIS> example: 

      |   '(' exp ')'  { $_[2] }  /* Let us simplify a bit the tree */

Explicit actions receive as arguments the references to the children nodes already 
built. The programmer can influence the shape of the tree by inserting
these explicit actions. In this example the programmer has decided to simplify the 
syntax tree: the nodes associated with the parenthesis are 
discarded and the reference to the subtree containing the proper
expression is returned. Such manoeuvre is called I<bypassing>.
See section L<The  bypass clause and the %no bypass directive>
to know more about I<automatic bypassing>

=head3 Explicitly Building Nodes With C<YYBuildAST> 

Sometimes the best time to decorate a node with some
attributes is just after being built.
In such cases the programmer can take I<manual control>
building the node with C<YYBuildAST> to 
inmediately proceed to decorate it.

The following example illustrates the situation:

 Variable:
     %name  VARARRAY
     $ID ('[' binary ']') <%name INDEXSPEC +>
       {
         my $self = shift;
         my $node =  $self->YYBuildAST(@_);
         $node->{line} = $ID->[1];
         return $node;
       }

This production rule defines the expression to access an array element 
as an identifier followed by
a non empty list of binary expressions C< Variable: ID ('[' binary ']')+>. 
Furthermore, the node corresponding
to the list of indices has been named C<INDEXSPEC>. 

When no explicit action is
inserted a binary node will be built having as first child the node
corresponding to the identifier C<$ID> and as second child the reference 
to the list of binary expressions. The children corresponding to
C<'['> and C<']'> are discarded since they are -by default- I<syntactic tokens>
(see section L<Syntactic and Semantic tokens>).
However, the programmer wants to decorate
the node being built with a C<line> attribute holding the line number in the source
code where the identifier being used appears. The call to the C<Parse::Eyapp::Driver>
method C<YYBuildAST> does the job of building the node. After
that the node can be decorated and returned. 

Actually, the C<%tree> directive is semantically equivalent to:

                %default action { goto &Parse::Eyapp::Driver::YYBuildAST }

=head3 Returning non References Under C<%tree>

When a I<explicit user action returns s.t. that is not a reference
no node will be inserted>. This fact can be used to supress nodes
in the AST being built. See the following example (file C<examples/returnnonode.yp>):

 nereida:~/src/perl/YappWithDefaultAction/examples> sed -ne '1,11p' returnnonode.yp | cat -n
  1  %tree
  2  %semantic token 'a' 'b'
  3  %%
  4  S:  /* empty */
  5      | S A
  6      | S B
  7  ;
  8  A : 'a'
  9  ;
 10  B : 'b' { }
 11  ;

since the action at line 10 returns C<undef>
the C<B : 'b'> subtree will not be inserted in the AST:

 nereida:~/src/perl/YappWithDefaultAction/examples> usereturnnonode.pl
 ababa
 S_2(S_3(S_2(S_3(S_2(S_1,A_4(TERMINAL[a]))),A_4(TERMINAL[a]))),A_4(TERMINAL[a]))

Observe the absence of C<B>s and C<'b'>s.

=head3 Intermediate actions and C<%tree>

Intermediate actions can be used to change the shape of the AST (prune it,
decorate it, etc.) but the value returned by them is ignored. The grammar 
below has two intermediate actions. They modify the attributes of the
node to its left and return a reference C<$f> to such node (lines 5 and 6):

 nereida:~/src/perl/YappWithDefaultAction/examples> \
          sed -ne '1,10p' intermediateactiontree.yp | cat -n
  1  %semantic token 'a' 'b'
  2  %tree bypass
  3  %%
  4  S:    /* empty */
  5      | S A.f { $f->{attr} = "A"; $f; } A
  6      | S B.f { $f->{attr} = "B"; $f; } B
  7  ;
  8  A : %name A 'a'
  9  ;
 10  B : %name B 'b'

See the client program running:

 nereida:~/src/perl/YappWithDefaultAction/examples> cat -n useintermediateactiontree.pl
  1  #!/usr/bin/perl -w
  2  use strict;
  3  use Parse::Eyapp;
  4  use intermediateactiontree;
  5
  6  { no warnings;
  7  *A::info = *B::info = sub { $_[0]{attr} };
  8  }
  9
 10  my $parser = intermediateactiontree->new();
 11  my $t = $parser->Run;
 12  print $t->str,"\n";
 nereida:~/src/perl/YappWithDefaultAction/examples> useintermediateactiontree.pl
 aabbaa
 S_2(S_4(S_2(S_1,A[A],A[a]),B[B],B[b]),A[A],A[a])

The 
attributes 
of left C<A>s 
have been effectively changed by the intermediate actions
from C<'a'> to C<'A'>.
However no further children have been inserted.

=head3 Syntactic and Semantic tokens

C<Parse::Eyapp> diferences between C<syntactic tokens>
and C<semantic tokens>. By default all tokens
declared using string notation (i.e. between quotes
like C<'+'>, C<'='>)
are considered I<syntactic tokens>. Tokens declared by an identifier
(like C<NUM> or C<VAR>) are by default considered
I<semantic tokens>. B<Syntactic tokens do not yield to nodes in the
syntactic tree>. Thus, the first print in the former L</SYNOPSIS> example:

              $parser->YYData->{INPUT} = "2*-3+b*0;--2\n"; 
              my $t = $parser->Run;                    
              local $Parse::Eyapp::Node::INDENT=2;
              print "Syntax Tree:",$t->str;


gives as result the following output:

 nereida:~/src/perl/YappWithDefaultAction/examples> synopsis.pl
 Syntax Tree:
 EXPRESION_LIST(
   PLUS(
     TIMES(
       NUM(
         TERMINAL[2]
       ),
       UMINUS(
         NUM(
           TERMINAL[3]
         )
       ) # UMINUS
     ) # TIMES,
     TIMES(
       VAR(
         TERMINAL[b]
       ),
       NUM(
         TERMINAL[0]
       )
     ) # TIMES
   ) # PLUS,
   UMINUS(
     UMINUS(
       NUM(
         TERMINAL[2]
       )
     ) # UMINUS
   ) # UMINUS
 ) # EXPRESION_LIST

C<TERMINAL> nodes corresponding to tokens that were defined by strings like
C<'='>, C<'-'>, C<'+'>, C<'/'>, C<'*'>, C<'('> and C<')'>  do not 
appear in the tree.  C<TERMINAL> nodes corresponding to tokens that were defined
using an identifer, like C<NUM> or C<VAR> are, by default,  I<semantic tokens>
and appear in the AST.


=head3 Changing the Status of a Token 

The new token declaration directives C<%syntactic token> and
C<%semantic token> can change the status of a token.
For example (file C<15treewithsyntactictoken.pl> in the C<examples/> directory), 
given the grammar:

   %syntactic token b
   %semantic token 'a' 'c'
   %tree

   %%

   S: %name ABC
        A B C
    | %name BC
        B C
   ;

   A: %name A
        'a'
   ;

   B: %name B
        b
   ;

   C: %name C
       'c'
   ;
   %%

the tree build for input C<abc> will be 
C<ABC(A(TERMINAL[a]),B,C(TERMINAL[c]))>.

=head3 Saving the Information of Syntactic Tokens in their Father

The reason for the adjective C<%syntactic> applied to a token is to 
state that the token influences the shape of the syntax tree
but carries no other information. When the syntax tree is built
the node corresponding to the token is discarded.

Sometimes the difference between syntactic and semantic 
tokens is blurred. For example the line number associated
with an instance of the syntactic token C<'+'> can be used later
-say during type checking- to emit a more accurate error
diagnostic. But if the node was discarded the information
about that line number is no longer available.
When building the syntax tree C<Parse::Eyapp> (namely
the method C<Parse::Eyapp::YYBuildAST>) checks 
if the method C<TERMINAL::save_attributes> exists and if so
it will be called when dealing with a I<syntactic token>. 
The method receives as argument - additionally
to the reference to the attribute of the token as it
is returned by the lexer - a reference
to the node associated with the left hand side of the
production. Here is an example (file C<examples/Types.eyp>)
of use:

              sub TERMINAL::save_attributes {
                # $_[0] is a syntactic terminal
                # $_[1] is the father.
                push @{$_[1]->{lines}}, $_[0]->[1]; # save the line number
              }


=head3 The  C<bypass> clause and the C<%no bypass> directive

The shape of the tree can be also modified using some C<%tree> clauses
as C<%tree bypass> which will produce an automatic I<bypass> of any
node with only one child at tree-construction-time. 

A I<bypass operation> consists in I<returning the only child 
of the node being visited to the father of the node and re-typing (re-blessing)
the node in the name of the production> (if a name was provided). 

A node may have only one child at tree-construction-time for one of
two reasons. 

=over

=item *
The first occurs when the right hand side of the production
was already unary like in:

                           exp:
                               %name NUM  NUM 

Here - if the C<bypass> clause is used - 
the C<NUM> node will be bypassed and the child C<TERMINAL> built
from the information provided by the lexical analyzer will be renamed/reblessed 
as C<NUM>.
  
=item *
Another reason for a node to be I<bypassed> is  the fact that though the right
hand side of the production may have more than one symbol, 
only one of them is not a syntactic token
like in:

                           exp: '(' exp ')'

=back

A consequence of the global scope application of C<%tree bypass>
is that undesired bypasses may occur like in

                           exp : %name UMINUS
                                 '-' $exp %prec NEG

though the right hand side has two symbols, token C<'-'> is
a syntactic token and therefore only C<exp> is left. The I<bypass>
operation will be applied when building this node.
This I<bypass> can be avoided applying the C<no bypass ID> directive to the corresponding 
production:

                           exp : %no bypass UMINUS
                                 '-' $exp %prec NEG

The following example (file C<examples/bypass.pl>) 
is the equivalent of the L</SYNOPSIS> example
but using the C<bypass> clause instead:

 use Parse::Eyapp;
 use Parse::Eyapp::Treeregexp;

 sub TERMINAL::info { $_[0]{attr} }
 { no warnings; *VAR::info = *NUM::info = \&TERMINAL::info; }

 my $grammar = q{
   %right  '='     # Lowest precedence
   %left   '-' '+' 
   %left   '*' '/' 
   %left   NEG     # Disambiguate -a-b as (-a)-b and not as -(a-b)
   %tree bypass    # Let us build an abstract syntax tree ...

   %%
   line: exp <%name EXPRESION_LIST + ';'>  { $_[1] } 
   ;

   exp:
       %name NUM  NUM            | %name VAR   VAR         | %name ASSIGN VAR '=' exp
     | %name PLUS exp '+' exp    | %name MINUS exp '-' exp | %name TIMES  exp '*' exp
     | %name DIV     exp '/' exp
     | %no bypass UMINUS
       '-' $exp %prec NEG
     |   '(' exp ')'
   ;

   %%
   # sub _Error, _Lexer and Run like in the synopsis example
   # ...
 }; # end grammar

 our (@all, $uminus);

 Parse::Eyapp->new_grammar( # Create the parser package/class
   input=>$grammar,
   classname=>'Calc', # The name of the package containing the parser
   firstline=>7       # String $grammar starts at line 7 (for error diagnostics)
 );
 my $parser = Calc->new();                # Create a parser
 $parser->YYData->{INPUT} = "a=2*-3+b*0\n"; # Set the input
 my $t = $parser->Run;                    # Parse it!

 print "\n************\n".$t->str."\n************\n";

 # Let us transform the tree. Define the tree-regular expressions ..
 my $p = Parse::Eyapp::Treeregexp->new( STRING => q{
   { #  Example of support code
     my %Op = (PLUS=>'+', MINUS => '-', TIMES=>'*', DIV => '/');
   }
   constantfold: /TIMES|PLUS|DIV|MINUS/:bin(NUM, NUM)
     => {
       my $op = $Op{ref($_[0])};
       $NUM[0]->{attr} = eval  "$NUM[0]->{attr} $op $NUM[1]->{attr}";
       $_[0] = $NUM[0];
     }
   zero_times_whatever: TIMES(NUM, .) and { $NUM->{attr} == 0 } => { $_[0] = $NUM }
   whatever_times_zero: TIMES(., NUM) and { $NUM->{attr} == 0 } => { $_[0] = $NUM }
   uminus: UMINUS(NUM) => { $NUM->{attr} = -$NUM->{attr}; $_[0] = $NUM }
   },
   OUTPUTFILE=> 'main.pm'
 );
 $p->generate(); # Create the tranformations

 $t->s(@all);    # constant folding and mult. by zero

 print $t->str,"\n";

when running this example with input C<"a=2*-3+b*0\n">
we obtain the following output:

 nereida:~/src/perl/YappWithDefaultAction/examples> bypass.pl

 ************
 EXPRESION_LIST(ASSIGN(TERMINAL[a],PLUS(TIMES(NUM[2],UMINUS(NUM[3])),TIMES(VAR[b],NUM[0]))))
 ************
 EXPRESION_LIST(ASSIGN(TERMINAL[a],NUM[-6]))

As you can see the trees are more compact when using the C<bypass> directive.


=head3 The C<alias> clause of the C<%tree> directive

Access to children in L<Parse::Eyapp> is made through the C<child> and C<children>
methods.
There are occasions however where access by name to the children may be preferable.
The use of the C<alias> clause with the C<%tree> directive creates accessors
to the children with names specified by the programmer. The I<dot and dolar notations>
are used for this. When dealing with a production like:
  
                       A: 
                          %name A_Node
                          Node B.bum N.pum $Chip

methods C<bum>, C<pum> and C<Chip> will be created for the class C<A_Node>.
Those methods wil provide access to the respective child (first, second and third in
the example). The methods are build at compile-time and therefore later 
transformations of the AST modifying the order of the children may 
invalidate the use of these getter-setters.

As an example, the CPAN module L<Language::AttributeGrammar> provides
AST decorators from an attribute grammar specification of the AST.
To work  L<Language::AttributeGrammar> requires named access to the children
of the AST nodes. Follows an example (file C<examples/CalcwithAttributeGrammar.pl>)
of a small calculator:

 use Parse::Eyapp;
 use Language::AttributeGrammar;

 my $grammar = q{
 ... # priority declarations. Like in previous examples
 %tree bypass alias

 %%
 line: $exp  { $_[1] }
 ;

 exp:
     %name NUM
           $NUM
         | %name VAR
           $VAR
     ............ # as in the bypass example
 }; # end grammar

 Parse::Eyapp->new_grammar(
   input=>$grammar, classname=>'Rule6', firstline =>7,
 );
 my $parser = Rule6->new();
 $parser->YYData->{INPUT} = "a = -(2*3+5-1)\n";
 my $t = $parser->Run;
 my $attgram = new Language::AttributeGrammar <<'EOG';
 # Compute the expression
 NUM:    $/.val = { $<attr> }
 TIMES:  $/.val = { $<left>.val * $<right>.val }
 PLUS:   $/.val = { $<left>.val + $<right>.val }
 MINUS:  $/.val = { $<left>.val - $<right>.val }
 UMINUS: $/.val = { -$<exp>.val }
 ASSIGN: $/.val = { $<exp>.val }
 EOG

 my $res = $attgram->apply($t, 'val');

=head2 Translation Schemes and the C<%metatree> directive

Eyapp allows through the C<%metatree> directive
the creation of I<Translation Schemes> as described in the L<Dragon's book|/REFERENCES>.
Instead of executing the semantic actions associated with the productions,
the syntax tree is built. Semantic actions aren't executed. Instead they are 
inserted as nodes of the syntax tree. The main difference with ordinary nodes
being that the attribute of such a C<CODE> node is a reference to the anonymous 
subroutine representing the semantic action.
The tree is later traversed in depth-first order using the C<$t-E<gt>translation_scheme>
method: each time a C<CODE> node
is visited  the action is executed.

The following example parses a tiny subset of a typical
I<typed language> and decorates the syntax tree with a new 
attribute C<t> holding the type of each declared variable:

 use strict; # File examples/trans_scheme_simple_decls4.pl
 use Data::Dumper;
 use Parse::Eyapp;
 our %s; # symbol table

 my $ts = q{ 
   %token FLOAT INTEGER NAME

   %{
   our %s;
   %}

   %metatree

   %%
   Dl:  D <* ';'>
   ;

   D : $T { $L->{t} = $T->{t} } $L
   ;

   T : FLOAT    { $lhs->{t} = "FLOAT" }
     | INTEGER  { $lhs->{t} = "INTEGER" }
   ;

   L : $NAME
         { $NAME->{t} = $lhs->{t}; $s{$NAME->{attr}} = $NAME }
     | $NAME { $NAME->{t} = $lhs->{t}; $L->{t} = $lhs->{t} } ',' $L
         { $s{$NAME->{attr}} = $NAME }
   ;
   %%
 }; # end $ts

 sub Error { die "Error sint�ctico\n"; }

 { # Closure of $input, %reserved_words and $validchars
   my $input = "";
   my %reserved_words = ();
   my $validchars = "";

   sub parametrize__scanner {
     $input = shift;
     %reserved_words = %{shift()};
     $validchars = shift;
   }

   sub scanner {
     $input =~ m{\G\s+}gc;                     # skip whites
     if ($input =~ m{\G([a-z_A_Z]\w*)\b}gc) {
       my $w = uc($1);                 # upper case the word
       return ($w, $w) if exists $reserved_words{$w};
       return ('NAME', $1);            # not a reserved word
     }
     return ($1, $1) if ($input =~ m/\G([$validchars])/gc);
     die "Not valid token: $1\n" if ($input =~ m/\G(\S)/gc);
     return ('', undef); # end of file
   }
 } # end closure

 Parse::Eyapp->new_grammar(input=>$ts,classname=>'main',outputfile=>'Types.pm');
 my $parser = main->new(yylex => \&scanner, yyerror => \&Error); 

 parametrize__scanner(
   "float x,y;\ninteger a,b\n",
   { INTEGER => 'INTEGER', FLOAT => 'FLOAT'},
   ",;"
 );

 my $t = $parser->YYParse() or die "Syntax Error analyzing input";

 $t->translation_scheme;

 $Data::Dumper::Indent = 1;
 $Data::Dumper::Terse = 1;
 $Data::Dumper::Deepcopy  = 1;
 $Data::Dumper::Deparse = 1;
 print Dumper($t);
 print Dumper(\%s);

Inside a Translation Scheme the lexical variable C<$lhs> refers to the attribute
of the father.

=head3 Execution Stages of a Translation Scheme

The execution of a Translation Scheme can be divided in the following stages:

=over

=item 1. During the first stage the grammar is analyzed and the parser is built:

 Parse::Eyapp->new_grammar(input=>$ts,classname=>'main',outputfile=>'Types.pm');

This stage is called I<Class Construction Time>

=item 2. A parser conforming to the generated grammar is built

  my $parser = main->new(yylex => \&scanner, yyerror => \&Error);

This stage is called  I<Parser Construction Time>

=item 3. The next phase is I<Tree construction time>. The input is set
and the tree is built:

 parametrize__scanner(
    "float x,y;\ninteger a,b\n",
    { INTEGER => 'INTEGER', FLOAT => 'FLOAT'},
    ",;"
  );

  my $t = $parser->YYParse() or die "Syntax Error analyzing input";

=item 4. The last stage is I<Execution Time>. The tree is traversed in depth first
order and the C<CODE> nodes are executed. 

                           $t->translation_scheme;

=back

This combination of bottom-up parsing with depth first traversing
leads to a semantic behavior similar to recursive top-down parsers
but with two advantages:

=over

=item * The grammar can be left-recursive

=item * At the time of executing the action the syntax tree is already built, therefore we can refer
to nodes on the right side of the action like in:

                      D : $T { $L->{t} = $T->{t} } $L

=back

=head3 The C<%begin> directive

The C<%begin { code }> directive  can be used when
building a translation scheme, i.e. when under the 
control of the C<%metatree> directive.
It indicates that such C<{ code }> will be executed at I<tree
construction time>. Therefore the code receives as arguments
the references to the nodes of the branch than is being built.
Usually I<begin code> assist in the construction of the tree.
Line 39 of the following code shows an example.
The action C<{ $exp }> simplifies the syntax tree
bypassing the parenthesis node. The example also illustrates
the combined use of default actions and 
translation schemes.

 nereida:~/src/perl/YappWithDefaultAction/examples> \
                cat -n trans_scheme_default_action.pl
   1  #!/usr/bin/perl -w
   2  use strict;
   3  use Data::Dumper;
   4  use Parse::Eyapp;
   5  use IO::Interactive qw(is_interactive);
   6
   7  my $translationscheme = q{
   8  %{
   9  # head code is available at tree construction time
  10  use Data::Dumper;
  11  our %sym; # symbol table
  12  %}
  13
  14  %defaultaction { $lhs->{n} = eval " $left->{n} $_[2]->{attr} $right->{n} " }
  15
  16  %metatree
  17
  18  %right   '='
  19  %left   '-' '+'
  20  %left   '*' '/'
  21
  22  %%
  23  line:       %name EXP
  24                exp <+ ';'> /* Expressions separated by semicolons */
  25                  { $lhs->{n} = $_[1]->Last_child->{n} }
  26  ;
  27
  28  exp:
  29              %name PLUS
  30                exp.left '+' exp.right
  31          |   %name MINUS
  32                exp.left '-' exp.right
  33          |   %name TIMES
  34                exp.left '*' exp.right
  35          |   %name DIV
  36                exp.left '/' exp.right
  37          |   %name NUM   $NUM
  38                  { $lhs->{n} = $NUM->{attr} }
  39          |   '(' $exp ')'  %begin { $exp }        # Bypass the node
  40          |   %name VAR
  41                $VAR
  42                  { $lhs->{n} = $sym{$VAR->{attr}}->{n} }
  43          |   %name ASSIGN
  44                $VAR '=' $exp
  45                  { $lhs->{n} = $sym{$VAR->{attr}}->{n} = $exp->{n} }
  46
  47  ;
  48
  49  %%
  50  # tail code is available at tree construction time
  51  sub _Error {
  52    die "Syntax error.\n";
  53  }
  54
  55  sub _Lexer {
  56      my($parser)=shift;
  57
  58      for ($parser->YYData->{INPUT}) {
  59          defined($_) or  return('',undef);
  60
  61          s/^\s*//;
  62          s/^([0-9]+(?:\.[0-9]+)?)// and return('NUM',$1);
  63          s/^([A-Za-z][A-Za-z0-9_]*)// and return('VAR',$1);
  64          s/^(.)// and return($1,$1);
  65          s/^\s*//;
  66      }
  67  }
  68
  69  sub Run {
  70      my($self)=shift;
  71      return $self->YYParse( yylex => \&_Lexer, yyerror => \&_Error );
  72  }
  73  }; # end translation scheme
  74
  75  $Data::Dumper::Indent = 1;
  76  $Data::Dumper::Terse = 1;
  77  $Data::Dumper::Deepcopy  = 1;
  78  my $p = Parse::Eyapp->new_grammar(
  79    input=>$translationscheme,
  80    classname=>'main',
  81    firstline => 6,
  82    outputfile => 'main.pm');
  83  die $p->qtables() if $p->Warnings;
  84  my $parser = main->new();
  85  print "Write a sequence of arithmetic expressions: " if is_interactive();
  86  $parser->YYData->{INPUT} = <>;
  87  my $t = $parser->Run() or die "Syntax Error analyzing input";
  88  $t->translation_scheme;
  89  my $treestring = Dumper($t);
  90  our %sym;
  91  my $symboltable = Dumper(\%sym);
  92  print <<"EOR";
  93  ***********Tree*************
  94  $treestring
  95  ******Symbol table**********
  96  $symboltable
  97  ************Result**********
  98  $t->{n}
  99
 100  EOR


=head1 The Treeregexp Language

A Treeregexp program is made of the repetition of three kind of 
primitives: The treeregexp transformations, auxiliar Perl code 
and Transformation Families.

  treeregexplist:  treeregexp* 

  treeregexp: 
      IDENT ':' treereg ('=>' CODE)?  # Treeregexp 
    | CODE                            # Auxiliar code
    | IDENT '=' IDENT + ';'           # Transformation families

Treeregexp themselves follow the rule:

                  IDENT ':' treereg ('=>' CODE)?

Several instances of this rule can be seen in the example in
the L</SYNOPSIS> section.
The identifier C<IDENT> gives the name to the rule.
At the time of this writing (2006) there are the following kinds
of treeregexes:

  treereg: 
        /* tree patterns with children */
      IDENT '(' childlist ')' ('and' CODE)? 
    | REGEXP (':' IDENT)? '(' childlist ')' ('and' CODE)? 
    | SCALAR '(' childlist ')' ('and' CODE)?  
    | '.' '(' childlist ')' ('and' CODE)? 
          /* leaf tree patterns */
    | IDENT ('and' CODE)? 
    | REGEXP (':' IDENT)? ('and' CODE)? 
    | '.' ('and' CODE)? 
    | SCALAR ('and' CODE)? 
    | ARRAY 
    | '*' 

=head2 Treeregexp rules

When seen a rule like

    zero_times: TIMES(NUM($x), ., .) and { $x->{attr} == 0 } => { $_[0] = $NUM }

The Treeregexp translator creates a C<Parse::Eyapp:YATW> object
that can be later referenced in the user code by the package variable
C<$zero_times>.

=head3 The treeregexp

The first part of the rule C<TIMES(NUM($x), ., .)>
indicates that for a matching to succeed the node being
visited must be of C<type> C<TIMES>, have a left child
of  C<type> C<NUM> and two more children.

If the first part succeeded then the following part 
takes the control to see if the I<semantic conditions>
are satisfied.

=head3 Semantic condition

The second part is optional and must be prefixed by the reserved word C<and>
followed by a Perl code manifesting the semantic conditions that must be hold
by the node to succeed. Thus, in the example:

  zero_times: TIMES(NUM($x), ., .) and { $x->{attr} == 0 } => { $_[0] = $NUM }

the semantic condition C<$x-E<gt>{attr} == 0> states that the
value of the number stored in the C<TERMINAL> node referenced
by C<$x> must be zero.

=head3 Referencing the matching nodes

The node  being visited can be referenced/modified
inside the semantic actions using C<$_[0]>. 

The Treeregexp
translator automatically creates a set of lexical variables
for us. The scope of these variables is limited to the
semantic condition and the transformation code. 

Thus, in the example

  zero_times: TIMES(NUM($x), ., .) and { $x->{attr} == 0 } => { $_[0] = $NUM }

the node being visited C<$_[0]>
can be also referenced using the lexical variable
C<$TIMES> which is created by he Treeregexp compiler.
In the same way a reference to the left child C<NUM> will be stored
in the lexical variable C<$NUM> and a
reference to the child of C<$NUM> will be stored in C<$x>.
The semantic condition states that the attribute
of the node associated with C<$x> must be zero.

When the same type of node appears several times inside 
the treeregexp part the associated lexical variable is 
declared by the Treeregexp compiler as an array.
This is the case in the C<constantfold> transformation
in the L</SYNOPSIS> example, where there are two nodes of type C<NUM>:

  constantfold: /TIMES|PLUS|DIV|MINUS/(NUM($x), ., NUM($y))
     => {
    $x->{attr} = eval  "$x->{attr} $W->{attr} $y->{attr}";
    $_[0] = $NUM[0];
  }

Thus variable  C<$NUM[0]> references the node that matches the 
first C<NUM> term in the formula and C<$NUM[1]> the one
that matches the second.

=head3 Transformation code

The third part of the rule is also optional and comes prefixed by
the big arrow C<=E<gt>>. The Perl code in this section usually 
transforms the matching tree.
To achieve the modification of the tree, the Treeregexp programmer
B<must use C<$_[0]>> and not the lexical variables provided by the translator.
Remember that in Perl C<$_[0]> is an alias of the actual parameter.
The C<constantfold> example above B<will not work> if we rewrite the code C<$_[0] = $NUM[0]> as

                            { $TIMES = $NUM }

=head2 Regexp Treeregexes

The previous C<constantfold> example used a classic Perl linear regexp
to explicit that the root node of the matching subtree must match the Perl regexp.
The general syntax for C<REGEXP> treeregexes patterns is:

      treereg: REGEXP (':' IDENT)? '(' childlist ')' ('and' CODE)? 

The C<REGEXP> must be specified between slashes (other delimiters
as C<{}> are not accepted).
It is legal to specify options after the second slash (like C<e>, C<i>, etc.).

The operation of string oriented regexps is slightly modified
when they are used inside a treeregexp:
B<by default the option> 
C<x> 
B<will be assumed>.
The treeregexp compiler will automatically insert it.
Use the new option C<X> (upper case X) if you want to supress such behavior.
B<There is no need also to insert> C<\b> 
B<word anchors> to delimit identifiers:
all the identifiers in a regexp treeregexp are automatically
surrounded by C<\b>. Use the option C<B> (upper case B)
to supress this behavior.

The optional identifier after the C<REGEXP> indicates the name of the lexical variable
that will be held a reference to the node whose type matches C<REGEXP>.
Variable C<$W> (or C<@W> if there are more than one REGEXP and or dot treeregexes)
will be used instead if no identifier is specified.


=head2 Scalar Treeregexes

A scalar treeregxp is defined writing a Perl scalar inside the treeregexp, like C<$x>
in C<NUM($x)>. A scalar treeregxp immediately matches any node that exists
and stores a reference to such node inside the Perl lexical scalar variable.
The scope of the variable is limited to the semantic parts of the Treeregexp.
Is illegal to use C<$W> or C<$W_#num> as variable names for scalar treeregexes.


=head2 Dot Treeregexes

A dot matches any node. It can be seen as an abbreviation for
scalar treeregexes. The reference to the matching node
is stored in the lexical variable C<$W>. 
The variable C<@W> will be used instead
if there are more than one REGEXP and or dot treeregexes

=head2 Array Treeregexp Expressions

The Treeregexp language permits expressions like:

                   A(@a,B($x),@c)

After the matching variable C<@A> contains the shortest prefix
of C<$A-E<gt>children> that does not match C<B($x)>.
The variable C<@c> contains the remaining sufix of
 C<$A-E<gt>children>. 

The following example uses 
array treereg expressions to move the assignment C<b = 5>
out of the C<while> loop:

  ..  ......................................................................
  93  my $program = "a =1000; c = 1; while (a) { c = c*a; b = 5; a = a-1 }\n";
  94  $parser->YYData->{INPUT} = $program;
  95  my $t = $parser->Run;
  96  my @output = split /\n/, $t->str;
  97
  98  my $p = Parse::Eyapp::Treeregexp->new( STRING => q{
  99    moveinvariant: BLOCK(
 100                     @prests,
 101                     WHILE(VAR($b), BLOCK(@a, ASSIGN($x, NUM($e)), @c)),
 102                     @possts
 103                   )
 104      => {
 105           my $assign = $ASSIGN;
 106           $BLOCK[1]->delete($ASSIGN);
 107           $BLOCK[0]->insert_before($WHILE, $assign);
 108         }
 109    },
 110    FIRSTLINE => 99,
 111  );
 112  $p->generate();

=head2 Star Treeregexp 

Deprecated. Don't use it. Is still there but not to endure.

=head2 Transformation Families

Transformations created by C<Parse::Eyapp::Treeregexp> can be grouped in 
families. That is the function of the rule:

                    treeregexp: IDENT '=' IDENT + ';' 


The next example (file C<examples/TSwithtreetransformations3.eyp>)
defines the family 

     algebraic_transformations = constantfold zero_times times_zero comasocfold;

Follows the code:

     my $transform = Parse::Eyapp::Treeregexp->new( STRING => q{

      uminus: UMINUS(., NUM($x), .) => { $x->{attr} = -$x->{attr}; $_[0] = $NUM }
      constantfold: /TIMES|PLUS|DIV|MINUS/:bin(NUM($z), ., NUM($y))
         => {
        $z->{attr} = eval  "$z->{attr} $W->{attr} $y->{attr}";
        $_[0] = $NUM[0];
      }
      commutative_add: PLUS($x, ., $y, .)
        => { my $t = $x; $_[0]->child(0, $y); $_[0]->child(2, $t)}
      comasocfold: TIMES(DIV(NUM($x), ., $b), ., NUM($y))
         => {
        $x->{attr} = $x->{attr} * $y->{attr};
        $_[0] = $DIV;
      }
      zero_times: TIMES(NUM($x), ., .) and { $x->{attr} == 0 } => { $_[0] = $NUM }
      times_zero: TIMES(., ., NUM($x)) and { $x->{attr} == 0 } => { $_[0] = $NUM }
      algebraic_transformations = constantfold zero_times times_zero comasocfold;
    },
    );

    $transform->generate();
    our ($uminus);
    $uminus->s($tree);
 

The transformations belonging to a family are usually applied 
toghether:

                $tree->s(@algebraic_transformations);


=head2 Code Support

In between Treeregexp rules and family assignments the programmer can insert 
Perl code between curly brackets. That code usually gives support to
the semantic conditions and transformations inside the rules.
See for example test 14 in the C<t/> directory of the Parse::Eyapp distribution.

  {
    sub not_semantic {
      my $self = shift;
      return  1 if $self->{token} eq $self->{attr};
      return 0;
    }
  }

  delete_tokens : TERMINAL and { not_semantic($TERMINAL) } 
                           => { $delete_tokens->delete() }

=head1 Compiling with C<eyapp>

=head2 Separated Compilation with C<eyapp> and C<treereg>

A Treeregexp program can be isolated in a file
an compiled with the program C<treereg>.
The default extension is C<.trg>.
See the following example:

 nereida:~/src/perl/YappWithDefaultAction/examples> cat -n Shift.trg
  1  # File: Shift.trg
  2  {
  3    sub log2 {
  4      my $n = shift;
  5      return log($n)/log(2);
  6    }
  7
  8    my $power;
  9  }
 10  mult2shift: TIMES($e, NUM($m)) and { $power = log2($m->{attr}); (1 << $power) == $m->{attr} }
 11    => {
 12      $_[0]->delete(1);
 13      $_[0]->{shift} = $power;
 14      $_[0]->type('SHIFTLEFT');
 15    }


Note that auxiliary support code can be inserted at any point
between transformations (lines 2-6). The code will be inserted (without 
the defining curly brackets) at that point. Note also
that the lexical variable C<$power> is visible
inside the definition of the C<mult2shift> transformation.

A treeregexp like C<$e> matches any node. A reference to the node
is saved in the lexical variable C<$e>. The scope of the variable
C<$e> is the current tree transformation, i.e. C<mult2shift>.
Such kind of treeregexps are called B<scalar treeregexp>s.

The call to the C<delete> method at line 12 deletes 
the second child of the node being visited (i.e. C<NUM($m)>).

The call to C<type> at line 14 retypes the node
as a C<SHIFTLEFT> node.

The program is compiled using the script C<treereg>:

 pl@nereida:~/src/perl/YappWithDefaultAction/examples$ eyapp Rule6
 pl@nereida:~/src/perl/YappWithDefaultAction/examples$ treereg Shift
 pl@nereida:~/src/perl/YappWithDefaultAction/examples$ ls -ltr | tail -2
 -rw-rw----  1 pl users   5960 2007-01-30 09:09 Rule6.pm
 -rw-rw----  1 pl users   1424 2007-01-30 09:09 Shift.pm

The Grammar C<Rule6.yp> is similar to the one in the L</SYNOPSIS>
section. Module C<Rule6.pm> contains the parser.
The module C<Shift.pm> contains the code implementing
the tree transformations.

The client program follows:

 nereida:~/src/perl/YappWithDefaultAction/examples> cat -n useruleandshift.pl
  1  #!/usr/bin/perl -w
  2  use strict;
  3  use Rule6;
  4  use Shift;
  5  { no warnings; *TERMINAL::info = \&TERMINAL::attr; }
  6
  7  push @SHIFTLEFT::ISA, 'Parse::Eyapp::Node';
  8  sub SHIFTLEFT::info { $_[0]{shift} }
  9
 10  my $parser = new Rule6();
 11  $parser->YYData->{INPUT} = <>;
 12  my $t = $parser->Run;
 13  print "***********\n",$t->str,"\n";
 14  $t->s(@Shift::all);
 15  print "***********\n",$t->str,"\n";

Lines 5 and 8 provide the node classes C<TERMINAL> and C<SHIFTLEFT> of C<info>
methods to be used by C<str> (lines 13 and 14). I<To make> C<SHIFTLEFT> I<a 
node class it has to inherit from> C<Parse::Eyapp::Node> (line 7).

Multiplications by a power of two are substituted by the corresponding shifts:

 nereida:~/src/perl/YappWithDefaultAction/examples> useruleandshift.pl
 a=b*8
 ***********
 ASSIGN(TERMINAL[a],TIMES(VAR(TERMINAL[b]),NUM(TERMINAL[8])))
 ***********
 ASSIGN(TERMINAL[a],SHIFTLEFT[3](VAR(TERMINAL[b])))

=head2 Compiling: More Options

See files C<Rule9.yp>, C<Transform4.trg> and C<foldand0rule9_4.pl> 
in the examples directory for a more detailed vision of this example. 
File C<Rule9.yp> is very much like the grammar
in the L</SYNOPSIS> example.
To compile the grammar C<Rule9.yp> and the treeregexp
file C<Transform4.trg> use the commands:
  
                eyapp -m 'Calc' Rule9.yp

That will produce a file C<Calc.pm> containing a package C<Calc>
that implements the LALR parser. 
Then the command:

                treereg -o T.pm -p 'R::' -m T Transform4

produces a file C<T.pm> containing a package C<T> that implements
the tree transformation program. The C<-p> option announces that
node classes are prefixed by C<'R::'>.

With such parameters the client program uses the generated modules as 
follows:

 nereida:~/src/perl/YappWithDefaultAction/examples> cat -n foldand0rule9_4.pl
  1  #!/usr/bin/perl -w
  2  # File: foldand0rule9_4.pl. Compile it with
  3  #          eyapp -m 'Calc' Rule9.yp; treereg -o T.pm -p 'R::' -m T Transform4
  4  use strict;
  5  use Calc;
  6  use T;
  7
  8  sub R::TERMINAL::info { $_[0]{attr} }
  9  my $parser = new Calc(yyprefix => "R::");
 10  my $t = $parser->YYParse( yylex => \&Calc::Lexer, yyerror => \&Calc::Error);
 11  print "\n***** Before ******\n";
 12  print $t->str."\n";
 13  $t->s(@T::all);
 14  print "\n***** After ******\n";
 15  print $t->str."\n";

running the program produces the following output:

 nereida:~/src/perl/YappWithDefaultAction/examples> foldand0rule9_4.pl
 2*3

 ***** Before ******
 R::TIMES(R::NUM(R::TERMINAL[2]),R::TERMINAL[*],R::NUM(R::TERMINAL[3]))

 ***** After ******
 R::NUM(R::TERMINAL[6])

=head1 C<Parse::Eyapp> Methods

A C<Parse::Eyapp> object holds the information 
about the C<Eyapp> input grammar: parsing tables,
conflicts, semantic actions, etc.


=head2 Parse::Eyapp-E<gt>new_grammar

To translate an Eyapp grammar you must use
either the L<eyapp> script or call the class constructor 
C<new_grammar>.
The C<Parse::Eyapp> method C<Parse::Eyapp-E<gt>new_grammar(input=E<gt>$grammar)> 
creates a package containing the code that implements a LALR parser
for the input grammar:

    my $p = Parse::Eyapp->new_grammar(
      input=>$translationscheme,
      classname=>'Grammar',
      firstline => 6,
      outputfile => 'main');
    die $p->Warnings if $p->Warnings;
    my $new_parser_for_grammar = Grammar->new();

The method returns a C<Parse::Eyapp> object.

You can check the object to see if there were
problems during the construction of the parser
for your grammar:

                die $p->qtables() if $p->Warnings;

The method C<Warnings> returns
the warnings produced during the parsing. The absence of warnings indicates
the correctness of the input program.

The call to C<Parse::Eyapp-E<gt>new_grammar> generates
a class/package containing the parser for
your input grammar. Such package lives in the namespace 
determined by the C<classname> argument
of C<new_grammar>. To create a parser for the 
grammar you call the constructor C<new> of
the just created class:

    my $new_parser_for_grammar = Grammar->new();

The meaning of the arguments of  C<Parse::Eyapp-E<gt>new_grammar> 
is:

=over 

=item -   input 

The string containing the input

=item -   classname 

The name of the package that will held the code for the LALR parser.
The package of the caller will be used as default if none is specified.

=item -   firstline 

For error diagnostics. The line where the definition of the Eyapp
grammar starts.

=item -   linenumbers 

Include/not include  C<# line directives> in the generated code

=item -   outputfile 

If defined the generated code fill be dumped in the specified filename (with extension .pm)
and the LALR information ambigueties and conflicts) in the specified filename 
with extension .output.

=back

=head2 $eyapp->qtables

Returns a string containing information
on warnings, ambiguities, conflicts, rules and the generated DFA tables.
Is the same information in C<file.output> when using the command 
C<eyapp -v file.eyp>.

  my $p = Parse::Eyapp->new_grammar(
    input=>$eyappprogram,
    classname=>'SimpleC',
    outputfile => 'SimpleC.pm',
    firstline=>12,
  );

  print $p->qtables() if $p->Warnings;

=head2 $eyapp->outputtables

It receives two arguments

  $eyapp->outputtables($path, $base)

Similar to C<qtables> but prints 
the information on warnings, conflicts and rules
to the specified C<$path/$file>.

=head2 $eyapp->Warnings

Returns the warnings resulting from compiling the grammar:

  my $p = Parse::Eyapp->new_grammar(
    input=>$translationscheme,
    classname=>'main',
    firstline => 6,
    outputfile => 'main'
  );
  die $p->Warnings if $p->Warnings;

Returns the empty string if there were no conflicts.

=head2 $eyapp->ShowDfa

Returns a string with the information about the LALR generated
DFA.


=head2 $eyapp->Summary

Returns a string with summary information about the compilation
of the grammar. No arguments.

=head2 $eyapp->Conflicts

Returns a string with summary information about the conflicts
that arised when compiling the grammar. No arguments.

=head2 $eyapp->DfaTable

Returns a string with the parsing tables

=head1 Methods Available in the Generated C<Class>

This section describes the methods and objects belonging
to the class generated either using L<eyapp> 
or L<new_grammar|/Parse::Eyapp-E<gt>new_grammar>. In the incoming paragraphs
we will assume that C<Class> was the 
value selected for the C<classname> argument
when C<Parse::Eyapp-E<gt>new_grammar> was called.
Objects belonging to  C<Class> are the actual parsers for the 
input grammar.

=head2 Class->new

The method C<Class-E<gt>new> returns a new LALR parser object.
Here C<Class> stands for the name of the class containing the parser.
See an example of call:

  my $parser = main->new(yyprefix => 'Parse::Eyapp::Node::',
                         yylex    => \&main::_Lexer,
                         yyerror  => \&main::_Error,
                         yydebug => 0x1F,
  );

The meaning of the arguments used in the example are as follows:

=over

=item - yyprefix

Used with C<%tree> or C<%metatree>. 
When used, the type names of the nodes of the syntax tree will
be build prefixing the value associated to C<yyprefix> to the name of the production
rule. The name of the production rule is either explicitly given through a %name
directive or the concatenation of the left hand side of the rule with the
ordinal of the right hand side of the production. 
See section L</Compiling with eyapp> for an example.

=item - yylex 

Reference to the lexer subroutine

=item - yyerror

Reference to the error subroutine. The error subroutine receives
as first argument the reference to the C<Class> parser object.
This way it can take advantage of methods like C<YYCurval>
and L<YYExpect|/$parser-E<gt>YYExpect> (see below):

  sub _Error {
    my($token)=$_[0]->YYCurval;
    my($what)= $token ? "input: '$token'" : "end of input";
    my @expected = $_[0]->YYExpect();

    local $" = ', ';
    die "Syntax error near $what. Expected one of these tokens: @expected\n";
  }

=item - yydebug

Controls the level of debugging. Must be a number.

=back

The package produced from the grammar has several methods.

The parser object has the following methods that work at parsing time
exactly as in L<Parse::Yapp>. These methods can be found
in the module Parse::Eyapp::Driver. 
Assume you have in C<$parser> the reference
to your parser object:

=head2  
$parser->YYParse()

It very much works C<Parse::Yapp::YYParse> and as yacc/bison C<yyparse>.
It accepts almost the same arguments as C<Class-E<gt>new> with the exception
of C<yyprefix> which can be used only with C<new>.

=head2 Debugging the Grammar

The integer parameter C<yydebug> of C<new> and C<YYParse>
controls the level of debugging. Different levels of 
verbosity can be obtained by setting the bits of this
argument. It works as follows:

     /============================================================\
     | Bit Value  | Outputs                                       |
     |------------+-----------------------------------------------|
     |  0x01      |  Token reading (useful for Lexer debugging)   |
     |------------+-----------------------------------------------|
     |  0x02      |  States information                           |
     |------------+-----------------------------------------------|
     |  0x04      |  Driver actions (shifts, reduces, accept...)  |
     |------------+-----------------------------------------------|
     |  0x08      |  Parse Stack dump                             |
     |------------+-----------------------------------------------|
     |  0x10      |  Error Recovery tracing                       |
     \============================================================/

As an example consider the grammar (file C<PlusList1.yp> in C<examples/>)

  %%
  S:      'c'+  { print "S -> 'c'+\n" }
  ;
  %%

When the parser is called with C<yydebug> activated:

  $self->YYParse(yylex => \&_Lexer, yyerror => \&_Error , yydebug => 0x1F);

the output reports about the parser activities:

  > use_pluslist1.pl
  ----------------------------------------
  In state 0:
  Stack:[0]
  c
  Need token. Got >c<
  Shift and go to state 2.
  ----------------------------------------
  In state 2:
  Stack:[0,2]
  Don't need token.
  Reduce using rule 2 (PLUS-1,1): Back to state 0, then go to state 3.
  ----------------------------------------
  In state 3:
  Stack:[0,3]
  Need token. Got ><
  Reduce using rule 3 (S,1): S -> 'c'+
  Back to state 0, then go to state 1.
  ----------------------------------------
  In state 1:
  Stack:[0,1]
  Shift and go to state 4.
  ----------------------------------------
  In state 4:
  Stack:[0,1,4]
  Don't need token.
  Accept.


=head2 
$parser->YYErrok 

Works as yacc/bison C<yyerrok>. 
Modifies the error status
so that subsequent 
error messages will be emitted.

=head2 
	 $parser->YYError 

Works as yacc/bison C<YYERROR>.
Pretends that a syntax error has been detected.

=head2 $parser->YYNberr

The current number of errors

=head2 
	 $parser->YYAccept 

Works as yacc/bison C<YYACCEPT>.
The parser finishes returning 
the current semantic value to indicate success.


=head2 
	 $parser->YYAbort 

Works as yacc/bison C<YYABORT>. 
The parser finishes returning 
C<undef> to indicate failure.

=head2 
  $parser->YYRecovering 

Works as yacc/bison C<YYRECOVERING>.
Returns C<TRUE> if the parser is recovering from a syntax error.

=head2 
    $parser->YYCurtok 

Gives the current token

=head2 
    $parser->YYCurval 

Gives the attribute associated with the current token

=head2 
    $parser->YYExpect  

Returns the list of tokens the parser 
expected when the failure occurred

 pl@nereida:~/src/perl/YappWithDefaultAction/examples$ \
                            sed -ne '26,33p' Postfix.eyp
 sub _Error {
   my($token)=$_[0]->YYCurval;
   my($what)= $token ? "input: '$token'" : "end of input";
   my @expected = $_[0]->YYExpect();

   local $" = ', ';
   die "Syntax error near $what. Expected one of these tokens: @expected\n";
 }


=head2 
    $parser->YYLexer 

Returns a reference to the lexical analyzer

=head2 
    $parser->YYLhs 

Returns the identifier of the left hand side of the current production (the one
that is being used for reduction/antiderivation. An example 
of use can be found in C<examples/Lhs1.yp>:

  %defaultaction { print $_[0]->YYLhs,"\n" }

=head2  $parser->YYRuleindex

Returns the index of the production rule, counting the super rule as rule 0.
To know the numbers have a look at  the C<.output> file.
To get a C<.output> file use the option C<-v> of C<eyapp> or the C<outputfile>
parameter when using method C<new_grammar> (see the documentation for L<eyapp>). 

=head2  $parser->YYRightside

Returns an array of strings describing the right hand side of the rule 

=head2  $parser->YYIsterm

Returns TRUE  if the symbol given as argument is a terminal. Example:

  DB<0> x $self->YYIsterm('exp')
 0  ''
  DB<1> x $self->YYIsterm('*')
 0  1


An example of combined 
use of C<YYRightside>, C<YYRuleindex>, C<YYLhs> and C<YYIsterm>
can be found C<examples/Rule3.yp>:

 nereida:~/src/perl/YappWithDefaultAction/examples> sed -n -e '4,22p' Rule3.yp | cat -n
  1  sub build_node {
  2    my $self = shift;
  3    my @children = @_;
  4    my @right = $self->YYRightside();
  5    my $var = $self->YYLhs;
  6    my $rule = $self->YYRuleindex();
  7
  8    for(my $i = 0; $i < @right; $i++) {
  9      $_ = $right[$i];
 10      if ($self->YYIsterm($_)) {
 11        $children[$i] = bless { token => $_, attr => $children[$i] },
 12                                            __PACKAGE__.'::TERMINAL';
 13      }
 14    }
 15    bless {
 16            children => \@children,
 17            info => "$var -> @right"
 18          }, __PACKAGE__."::${var}_$rule"
 19  }

when executed an output similar to this is produced:

 nereida:~/src/perl/YappWithDefaultAction/examples> userule3.pl
 2*3
 $VAR1 = bless( {
   'info' => 'exp -> exp * exp',
   'children' => [
     bless( {
       'info' => 'exp -> NUM',
       'children' => [ bless( { 'attr' => '2', 'token' => 'NUM' }, 'Rule3::TERMINAL' ) ]
     }, 'Rule3::exp_6' ),
     bless( { 'attr' => '*', 'token' => '*' }, 'Rule3::TERMINAL' ),
     bless( {
       'info' => 'exp -> NUM',
       'children' => [ bless( { 'attr' => '3', 'token' => 'NUM' }, 'Rule3::TERMINAL' )
       ]
     }, 'Rule3::exp_6' )
   ]
 }, 'Rule3::exp_11' );


=head2  $parser->YYIssemantic

Returns TRUE if the terminal is I<semantic>. I<Semantics token> can be declared
using the directive C<%semantic token>. The opposite of a I<Semantic token>
is a I<Syntactic token>. I<Syntactic tokens> can be declared
using the directive  C<%syntactic token>. 

When using the C<%tree> directive all the nodes corresponding to syntactic
tokens are pruned from the tree. Under this directive
tokens in the text delimited by simple quotes (like C<'+'>)
are, by default, considered syntactic tokens. 

When using the C<%metatree> directive all the tokens 
are considered, by default, I<semantic tokens>.
Thus, no nodes will be - by default- pruned when construction
the code augmented tree. The exception are string tokens
used as separators in the definition of
lists,  like in C<S E<lt>* ';'E<gt>>. If you want the separating string token
to appear include an explicit semantic declaration for it (example C<%semantic token ';'>).

=head2  $parser->YYName

Returns the name of the current rule (The production whose reduction
gave place to the execution of the current semantic action).

  DB<12> x $self->YYName
 0  'exp_11'

=head2  $parser->YYPrefix

Return and/or sets the C<yyprefix> attribute. This a string that will be concatenated
as a prefix to any C<Parse::Eyapp::Node> nodes in the syntax tree.

=head2 $parser->YYBypass

Returns TRUE if running under the C<%tree bypass> clause

=head2 $parser->YYBypassrule

Returns TRUE if the production being
used for reduction was marked to be bypassed.

=head2 $parser->YYFirstline

First line of the input string describing the grammar

=head2 Parse::Eyapp::Driver::BeANode

Is not a method.
Receives as input a C<Class> name. 
Introduces C<Parse::Eyapp::Node> as an ancestor class
of C<Class>. To work correctly, objects belonging to 
C<Class> must be hashes
with a C<children> key whose value must be a reference
to the array of children. The children must be also
C<Parse::Eyapp::Node> nodes.
Actually you can circumvent this call by directly introducing
C<Parse::Eyapp::Node> in the ancestors of C<Class>:

         push @{$class."::ISA"}, "Parse::Eyapp::Node" 

=head2 $parser->YYBuildAST

Sometimes the best time to decorate a node with some attributes is just
after being built. In such cases the programmer can take manual control
building the node with C<YYBuildAST> to inmediately proceed to decorate it.

The following example from C<examples/Types.eyp>
illustrates the idea:

 Variable:
     %name  VARARRAY
     $ID ('[' binary ']') <%name INDEXSPEC +>
       {
         my $self = shift;
         my $node =  $self->YYBuildAST(@_);
         $node->{line} = $ID->[1];
         return $node;
       }

Actually, the C<%tree> directive is semantically equivalent to:

  %default action { goto &Parse::Eyapp::Driver::YYBuildAST }

=head2 $parser->YYBuildTS

Similar to C<$parser-E<gt>YYBuildAST> but builds nodes for translation schemes.


=head1 Parse::Eyapp::Parse objects

The parser for the C<Eyapp> language
was written and generated
using C<Parse::Eyapp> and the C<eyapp> compiler (actually
the first version 
was bootstrapped using the L<yapp|Parse::Yapp> compiler).
The Eyapp program parsing the C<Eyapp> language
is in the file C<Parse/Eyapp/Parse.yp> 
in the C<Parse::Eyapp> distribution.
(See also section L<The Eyapp Language>)
Therefore C<Parse::Eyapp::Parse> 
objects have all the methods mentioned 
in the section "L</Methods Available in the Generated C<Class>>".
A C<Parse::Eyapp::Parse> is nothing but a particular kind of C<Parse::Eyapp>
parser: I<the one that parses> C<Eyapp> I<grammars>.


=head1 Parse::Eyapp::Node Methods

The C<Parse::Eyapp::Node> objects represent the nodes of the syntax
tree. 
All the node classes build by C<%tree> and C<%metatree> directives
inherit from C<Parse::Eyapp::Node> and consequently have
acces to the methods provided in such module. 

=head2  Parse::Eyapp::Node->new

Nodes are usually created using the C<%tree> or C<%metatree>
C<Parse::Eyapp> directives. The C<Parse::Eyapp::Node> constructor C<new>
offers an alternative way to create forests.

This class method can be used to build multiple nodes on a row.
It receives a string describing the tree and optionally a
reference to a subroutine. Such subroutine (called the attribute
handler) is in charge to initialize
the attributes of the just created nodes.
The attribute handler is called with the array of references to the
nodes as they appear in the string from left to right.

C<Parse::Eyapp::Node-E<gt>new> returns an array of pointers to the nodes created
as they appear in the input string from left to right.
In scalar context returns a pointer to the first of these trees.

The following example (see file C<examples/28foldwithnewwithvars.pl>) of
a treeregexp transformation creates a new C<NUM(TERMINAL)> node
using C<Parse::Eyapp::Node-E<gt>new>:

 my $p = Parse::Eyapp::Treeregexp->new( STRING => q{
   {
     my %Op = (PLUS=>'+', MINUS => '-', TIMES=>'*', DIV => '/');
   }
   constantfold: /TIMES|PLUS|MINUS|DIV/(NUM($x), NUM($y))
      => {
     my $op = $Op{ref($_[0])};

     my $res = Parse::Eyapp::Node->new(
       q{NUM(TERMINAL)},
       sub {
         my ($NUM, $TERMINAL) = @_;
         $TERMINAL->{attr} = eval "$x->{attr} $op $y->{attr}";
         $TERMINAL->{token} = 'NUM';
       },
     );
     $_[0] = $res;
   }
   },
 );

The string can describe more than one tree like in:

   my @t = Parse::Eyapp::Node->new(
            'A(C,D) E(F)', sub { my $i = 0; $_->{n} = $i++ for @_ });

The following trees will be built:

        bless( { 'n' => 0,
          'children' => [
            bless( { 'n' => 1, 'children' => [] }, 'C' ),
            bless( { 'n' => 2, 'children' => [] }, 'D' )
          ]
        }, 'A' );
        bless( { 'n' => 3,
          'children' => [
            bless( { 'n' => 4, 'children' => [] }, 'F' )
          ]
        }, 'E' );

and C<@t> will contain 5 references to the corresponding subtrees 
A(C,D), C, D, E(F) and F.

=head2 Directed Acyclic Graphs with C<Parse::Eyapp::Node-E<gt>hnew>

C<Parse::Eyapp> provides the method C<Parse::Eyapp::Node-E<gt>hnew>
to build I<Directed Acyclic Graphs> (DAGs) instead of trees. They are built using 
I<hashed consing>, i.e. I<memoizing> the creation of nodes. 
It works very much like C<Parse::Eyapp::Node-E<gt>new>
but if one of the implied trees was previously built, C<hnew> 
returns a reference to the existing one.
See the following debugger session where several DAGs describing
I<type expressions> are built:

  DB<2> x $a = Parse::Eyapp::Node->hnew('F(X_3(A_3(A_5(INT)), CHAR, A_5(INT)),CHAR)')
 0  F=HASH(0x85f6a20)
    'children' => ARRAY(0x85e92e4)
    |- 0  X_3=HASH(0x83f55fc)
    |     'children' => ARRAY(0x83f5608)
    |     |- 0  A_3=HASH(0x85a0488)
    |     |     'children' => ARRAY(0x859fad4)
    |     |        0  A_5=HASH(0x85e5d3c)
    |     |           'children' => ARRAY(0x83f4120)
    |     |              0  INT=HASH(0x83f5200)
    |     |                 'children' => ARRAY(0x852ccb4)
    |     |                      empty array
    |     |- 1  CHAR=HASH(0x8513564)
    |     |     'children' => ARRAY(0x852cad4)
    |     |          empty array
    |     `- 2  A_5=HASH(0x85e5d3c)
    |           -> REUSED_ADDRESS
    `- 1  CHAR=HASH(0x8513564)
          -> REUSED_ADDRESS
  DB<3> x $a->str
 0  'F(X_3(A_3(A_5(INT)),CHAR,A_5(INT)),CHAR)'

The second occurrence of C<A_5(INT)> is labelled C<REUSED_ADDRESS>. The
same occurs with the second instance  of C<CHAR>. C<Parse::Eyapp::Node-E<gt>hnew>
can be more convenient than C<new> 
when dealing with optimizations like I<common subexpressions>
or during I<type checking>. 
See file C<examples/Types.eyp> for a more comprehensive example.


=head2  $node->type 

Returns the type of the node.
It can be called as a sub when C<$node> is not
a C<Parse::Eyapp::Node> like this:

                     Parse::Eyapp::Node::type($scalar)

This is the case when visiting C<CODE> nodes.

The following session with the debugger illustrates how it works:

  > perl -MParse::Eyapp::Node -de0
  DB<1> @t = Parse::Eyapp::Node->new("A(B,C)") # Creates a tree
  DB<2> x map { $_->type } @t # Get the types of the three nodes
  0  'A'
  1  'B'
  2  'C'
  DB<3> x Parse::Eyapp::Node::type(sub {})
  0  'CODE'
  DB<4> x Parse::Eyapp::Node::type("hola")
  0  'Parse::Eyapp::Node::STRING'
  DB<5> x Parse::Eyapp::Node::type({ a=> 1})
  0  'HASH'
  DB<6> x Parse::Eyapp::Node::type([ a, 1 ])
  0  'ARRAY'

As it is shown in the example it can be called as a subroutine with 
a (CODE/HASH/ARRAY) reference or an ordinary scalar.

The words HASH, CODE, ARRAY and STRING are reserved for 
ordinary Perl references. Avoid naming a node with one of those words.

=head2  $node->child

Setter-getter to modify a specific child of a node.
It is called like:

                   $node->child($i)

Returns the child with index $i. Returns C<undef> if the child does not exists.
It has two obligatory parameters: the node (since it is a method)
and the index of the child. Sets teh new value if called

                    $node->child($i, $tree)

The method will croak if the obligatory parameters are not provided.
Follows an example of use inside a Treereg program (see
file C<examples/TSwithtreetransformations2.eyp>) that swaps
the children of a C<PLUS> node:

  my $transform = Parse::Eyapp::Treeregexp->new( STRING => q{
     commutative_add: PLUS($x, ., $y, .) # 1st . = '+' 2nd . = CODE
       => { my $t = $x; $_[0]->child(0, $y); $_[0]->child(2, $t)}
  }

=head2 Child Access Through C<%tree alias> 

Remember that when the C<Eyapp> program runs 
under the C<%tree alias> directive 
The I<dot and dollar notations> can be used 
to generate named getter-setters to access the children:

  %tree bypass alias
  .... 
  %%
  exp: %name PLUS
         exp.left '+' exp.right
  ....
  %%
  .... # and later
  print $exp->left->str;

Here methods with names C<left> and C<right> will be created
to access the corresponding children associated with the 
two instances of C<exp> in the right hand side of
the production rule.

=head2  $node->children

Returns the array of children of the node. When the tree is a
translation scheme the CODE references are also included.
See C<examples/TSPostfix3.eyp> for an example of use
inside a Translation Scheme:

 pl@nereida:~/src/perl/YappWithDefaultAction/examples$\
                       sed -ne '31,34p' TSPostfix3.eyp
 line: %name PROG
        exp <%name EXP + ';'>
          { @{$lhs->{t}} = map { $_->{t}} ($_[1]->children()); }

The tree in a Translation Scheme contains the references to
the C<CODE> implementing the semantic actions.
For example,  the syntax tree built by 
the parser for the input C<a=-b*3> in C<TSPostfix3.eyp> is:

 PROG(EXP(
     ASSIGN(
       TERMINAL[a],
       TERMINAL[=],
       TIMES(
         NEG(TERMINAL[-], VAR(TERMINAL[b], CODE), CODE),
         TERMINAL[*],
         NUM(TERMINAL[3], CODE),
         CODE
       ) # TIMES,
       CODE
     ) # ASSIGN
   ) # EXP,
   CODE
 ) # PROG

C<$node-E<gt>children> can also be used as a setter.

=head2  $node->Children

Returns the array of children of the node.
When dealing with a translation scheme,
the  $node->Children method (first in uppercase) returns the non
CODE children of the node.

=head2  $node->last_child

Return the last child of the node. When dealing with translation
schemes, the last can be a C<CODE> node.

=head2  $node->Last_child

The C<$node-E<gt>Last_child> method returns the last non CODE child of the node.
See an example:

  line:       %name EXP
                exp <+ ';'> /* Expressions separated by semicolons */
            { $lhs->{n} = $_[1]->Last_child->{n} }
  ;

=head2 $node->descendant

The C< descendant> method 
returns the descendant of a node given its I<coordinates>. 
The coordinates of a node C<$s> relative to a tree C<$t>
to which it belongs is a string of numbers
separated by dots like  C<".1.3.2"> which
denotes the I<child path> from C<$t> to C<$s>, i.e.
C<$s == $t-E<gt>child(1)-E<gt>child(3)-E<gt>child(2)>.

See a session
with the debugger:

   DB<7> x $t->child(0)->child(0)->child(1)->child(0)->child(2)->child(1)->str
 0  '
 BLOCK[8:4:test]^{0}(
   CONTINUE[10,10]
 )
   DB<8> x $t->descendant('.0.0.1.0.2.1')->str
 0  '
 BLOCK[8:4:test]^{0}(
   CONTINUE[10,10]

=head2 $node->str

The C<str> method returns a string representation of the tree. 
The I<str> method traverses the syntax tree dumping the type
of the node being visited in a string. If the node being visited
has a method C<info> it will
be executed and its result inserted between C<$DELIMITER>s
into the string. Thus, in the L</SYNOPSIS>
example, by adding the C<info> method to the class C<TERMINAL>:

 sub TERMINAL::info {
   $_[0]{attr}
 }

we achieve the insertion of attributes in the string being built 
by C<str>.

The existence of some methods (like C<footnote>) and
the values of some package variables
influence the behavior of C<str>. Among the most
important are:

  @PREFIXES = qw(Parse::Eyapp::Node::);                                 # Prefixes to supress 
  $INDENT = 0; # 0 = compact, 1 = indent, 2 = indent and include Types in closing parenthesis
  $STRSEP = ',';                                # Separator between nodes, by default a comma
  $DELIMITER = '[';                         # The string returned by C<info> will be enclosed 
  $FOOTNOTE_HEADER = "\n---------------------------\n"; 
  $FOOTNOTE_SEP = ")\n"; 
  $FOOTNOTE_LEFT = '^{';                               # Left delimiter for a footnote number
  $FOOTNOTE_RIGHT = '}';                              # Right delimiter for a footnote number
  $LINESEP = 4;                             # When indent=2 the enclosing parenthesis will be
                                            # commented if more than $LINESEP apart

The following list defines the C<$DELIMITER>s you can choose for 
attribute representation:

          '[' => ']', '{' => '}', '(' => ')', '<' => '>'

If the node being visited has a method  C<footnote>, the string
returned by the method will be concatenated at the end of the 
string as a footnote. The variables C<$FOOTNOTE_LEFT> and
C<$FOOTNOTE_RIGHT> govern the displaying of footnote numbers.

Follows an example of output using C<footnotes>. 

 nereida:~/doc/casiano/PLBOOK/PLBOOK/code/Simple-Types/script> \
                                          usetypes.pl prueba24.c
 PROGRAM^{0}(FUNCTION[f]^{1}(RETURNINT(TIMES(INUM(TERMINAL[2:2]),VAR(TERMINAL[a:2])))))
 ---------------------------
 0)
 Types:
 $VAR1 = {
   'CHAR' => bless( {
     'children' => []
   }, 'CHAR' ),
   'VOID' => bless( {
     'children' => []
   }, 'VOID' ),
   'INT' => bless( {
     'children' => []
   }, 'INT' ),
   'F(X_1(INT),INT)' => bless( {
     'children' => [
       bless( {
         'children' => [
           $VAR1->{'INT'}
         ]
       }, 'X_1' ),
       $VAR1->{'INT'}
     ]
   }, 'F' )
 };
 Symbol Table:
 $VAR1 = {
   'f' => {
     'type' => 'F(X_1(INT),INT)',
     'line' => 1
   }
 };

 ---------------------------
 1)
 $VAR1 = {
   'a' => {
     'type' => 'INT',
     'param' => 1,
     'line' => 1
   }
 };

The first footnote was due to a call to C<PROGRAM:footnote>.
The C<footnote> method for the C<PROGRAM> node was defined as:

 nereida:~/doc/casiano/PLBOOK/PLBOOK/code/Simple-Types/lib/Simple> \
                             sed -n -e '691,696p' Types.eyp | cat -n
     1  sub PROGRAM::footnote {
     2    return "Types:\n"
     3           .Dumper($_[0]->{types}).
     4           "Symbol Table:\n"
     5           .Dumper($_[0]->{symboltable})
     6  }

The second footnote was produced by the existence of a
C<FUNCTION::footnote> method:

 nereida:~/doc/casiano/PLBOOK/PLBOOK/code/Simple-Types/lib/Simple> \
                            sed -n -e '702,704p' Types.eyp | cat -n
 1  sub FUNCTION::footnote {
 2    return Dumper($_[0]->{symboltable})
 3  }


The source program for the example was:

     1  int f(int a) {
     2    return 2*a;
     3  }


=head2  $node->delete

The C<$node-E<gt>delete($child)> method is used to delete the specified child of C<$node>.
The child to delete can be specified using the index or a
reference. It returns the deleted child.

Throws an exception if the object can't do C<children> or has no C<children>.
See also the L<delete|/$yatw-E<gt>delete> method of treeregexes 
(C<Parse::Eyapp:YATW> objects)
to delete the node being visited.

The following example moves out of a loop an assignment statement
assuming is an invariant of the loop. To do it, it uses
the C<delete> and C<insert_before> methods:

  nereida:~/src/perl/YappWithDefaultAction/examples> \
              sed -ne '98,113p' moveinvariantoutofloopcomplexformula.pl
  my $p = Parse::Eyapp::Treeregexp->new( STRING => q{
    moveinvariant: BLOCK(
                     @prests,
                     WHILE(VAR($b), BLOCK(@a, ASSIGN($x, NUM($e)), @c)),
                     @possts
                   )
      => {
           my $assign = $ASSIGN;
           $BLOCK[1]->delete($ASSIGN);
           $BLOCK[0]->insert_before($WHILE, $assign);
         }
    },
    FIRSTLINE => 99,
  );
  $p->generate();
  $moveinvariant->s($t);

The example below deletes CODE nodes
from the tree build for a translation scheme:

  my $transform = Parse::Eyapp::Treeregexp->new( 
    STRING=>q{
      delete_code: CODE => { Parse::Eyapp::Node::delete($CODE) }
    },
  )

Observe how delete is called as a subroutine.

=head2  $node->unshift($newchild)

Inserts C<$newchild> at the beginning of the list of children of C<$node>.
See also the L<unshift|/$yatw-E<gt>unshift> method 
for C<Parse::Eyapp:YATW> treeregexp transformation objects

=head2  $node->push($newchild)

Inserts C<$newchild> at the end of the list of children of C<$node>.

=head2  $node->insert_before($position, $new_child)

Inserts C<$newchild> before C<$position> in the list of children of C<$node>.
Variable C<$position> can be an index or a reference.

The method throws an exception if C<$position> is an index
and is not in range. Also if C<$node> has no children.

The method throws a warning if C<$position> is a reference and does not define
an actual child. In such case C<$new_child> is not inserted.

See also the L<insert_before|/$yatw-E<gt>insert_before> 
method for C<Parse::Eyapp:YATW> treeregexp transformation objects


=head2  $node->insert_after($position, $new_child)

Inserts C<$newchild> after C<$position> in the list of children of C<$node>.
Variable C<$position> can be an index or a reference.

The method throws an exception if C<$position> is an index and is not
in the range of C<$node->children>.

The method throws a warning if C<$position> is a reference and does not exists
in the list of children. In such case C<$new_child> is not inserted.

=head2  $node->translation_scheme

Traverses $node. Each time a CODE node is visited the subroutine referenced
is called with arguments the node and its children. Usually the code will decorate
the nodes with new attributes or will update existing ones. Obviously this method
does nothing for an ordinary AST. It is used after compiling
an Eyapp program that makes use of the C<%metatree> directive.

=head2 $node->bud

Bottom-up decorator. The tree is traversed bottom-up. The set of
transformations is applied to each node in the order
supplied by the user. I<As soon as one succeeds
no more transformations are applied>.
For an example  see the files C<examples/Types.eyp> 
and C<examples/Trans.trg>.  The code below 
shows an extract of the type-checking phase of a toy-example compiler: 

  nereida:~/src/perl/YappWithDefaultAction/examples> \
                          sed -ne '600,611p' Types.eyp
   my @typecheck = (
     our $inum,
     our $charconstant,
     our $bin,
     our $arrays,
     our $assign,
     our $control,
     our $functioncall,
     our $statements,
   );

   $t->bud(@typecheck);

As an example of the appearance of the treeregexp transformations
involved in the former call, see the code of the C<$control> 
treeregexp transformation:

  nereida:~/src/perl/YappWithDefaultAction/examples> \
                          sed -ne '183,192p' Trans.trg
  control: /IF|IFELSE|WHILE/:con($bool)
    => {
      $bool = char2int($con, 0) if $bool->{t} == $CHAR;
        type_error("Condition must have integer type!", $bool->line)
      unless $bool->{t} == $INT;

      $con->{t} = $VOID;

      return 1;
    }

=head1 Transformations Objects: Parse::Eyapp:YATW  Methods

Parse::Eyapp:YATW objects represent tree transformations.
They carry the information of what nodes match and how to modify
them.


=head2  Parse::Eyapp::Node->new

Builds a treeregexp transformation object.
Though usually you build a transformation by means of Treeregexp programs
you can directly invoke the method to build a tree transformation.
A transformation object can be built from a function 
that conforms to the YATW tree transformation call protocol
(see the section L</The YATW Tree Transformation Call Protocol>).
Follows an example (file C<examples/12ts_simplify_with_s.pl>):

 nereida:~/src/perl/YappWithDefaultAction/examples> \
        sed -ne '68,$p' 12ts_simplify_with_s.pl | cat -n
  1  sub is_code {
  2    my $self = shift; # tree
  3
  4    # After the shift $_[0] is the father, $_[1] the index
  5    if ((ref($self) eq 'CODE')) {
  6      splice(@{$_[0]->{children}}, $_[1], 1);
  7      return 1;
  8    }
  9    return 0;
 10  }
 11
 12  Parse::Eyapp->new_grammar(
 13    input=>$translationscheme,
 14    classname=>'Calc',
 15    firstline =>7,
 16  );
 17  my $parser = Calc->new();                # Create the parser
 18
 19  $parser->YYData->{INPUT} = "2*-3\n";  print "2*-3\n"; # Set the input
 20  my $t = $parser->Run;                    # Parse it
 21  print $t->str."\n";
 22  my $p = Parse::Eyapp::YATW->new(PATTERN => \&is_code);
 23  $p->s($t);
 24  { no warnings; # make attr info available only for this display
 25    local *TERMINAL::info = sub { $_[0]{attr} };
 26    print $t->str."\n";
 27  }

After the C<Parse::Eyapp::YATW> object C<$p> is built at line 22
the call to method C<$p-E<gt>s($t)> applies  the 
transformation C<is_code> using a bottom-up traversing of the tree C<$t>.
The achieved effect is the elimination of C<CODE> references
in the translation scheme tree.
When executed the former code produces:

 nereida:~/src/perl/YappWithDefaultAction/examples> 12ts_simplify_with_s.pl
 2*-3
 EXP(TIMES(NUM(TERMINAL,CODE),TERMINAL,UMINUS(TERMINAL,NUM(TERMINAL,CODE),CODE),CODE),CODE)
 EXP(TIMES(NUM(TERMINAL[2]),TERMINAL[*],UMINUS(TERMINAL[-],NUM(TERMINAL[3]))))

The file C<foldrule6.pl> in the C<examples/> distribution directory
gives you another example:

 nereida:~/src/perl/YappWithDefaultAction/examples> cat -n foldrule6.pl
   1  #!/usr/bin/perl -w
   2  use strict;
   3  use Rule6;
   4  use Parse::Eyapp::YATW;
   5
   6  my %BinaryOperation = (PLUS=>'+', MINUS => '-', TIMES=>'*', DIV => '/');
   7
   8  sub set_terminfo {
   9    no warnings;
  10    *TERMINAL::info = sub { $_[0]{attr} };
  11  }
  12  sub is_foldable {
  13    my ($op, $left, $right);
  14    return 0 unless defined($op = $BinaryOperation{ref($_[0])});
  15    return 0 unless ($left = $_[0]->child(0), $left->isa('NUM'));
  16    return 0 unless ($right = $_[0]->child(1), $right->isa('NUM'));
  17
  18    my $leftnum = $left->child(0)->{attr};
  19    my $rightnum = $right->child(0)->{attr};
  20    $left->child(0)->{attr} = eval "$leftnum $op $rightnum";
  21    $_[0] = $left;
  22  }
  23
  24  my $parser = new Rule6();
  25  $parser->YYData->{INPUT} = "2*3";
  26  my $t = $parser->Run;
  27  &set_terminfo;
  28  print "\n***** Before ******\n";
  29  print $t->str;
  30  my $p = Parse::Eyapp::YATW->new(PATTERN => \&is_foldable);
  31  $p->s($t);
  32  print "\n***** After ******\n";
  33  print $t->str."\n";

when executed produces:

 nereida:~/src/perl/YappWithDefaultAction/examples> foldrule6.pl

 ***** Before ******
 TIMES(NUM(TERMINAL[2]),NUM(TERMINAL[3]))
 ***** After ******
 NUM(TERMINAL[6])

=head2  The YATW Tree Transformation Call Protocol

For a subroutine  C<pattern_sub> to work as a YATW tree transformation
- as subroutines C<is_foldable> and  C<is_code> above - has to conform to the following
call description:

  pattern_sub(
      $_[0],  # Node being visited
      $_[1],  # Father of this node
      $index, # Index of this node in @Father->children
      $self,  # The YATW pattern object
  );

The C<pattern_sub> must return TRUE if matched 
and FALSE otherwise.

The protocol may change in the near future. 
Avoid using other information than the fact that 
the first argument 
is the node being visited.


=head2  Parse::Eyapp::YATW->buildpatterns

Works as Parse::Eyapp->new but receives an array of subs 
conforming to the YATW Tree Transformation Call Protocol.

  our @all = Parse::Eyapp::YATW->buildpatt(\&delete_code, \&delete_tokens);

=head2  $yatw->delete

The root of the tree that is currently matched 
by the YATW transformation C<$yatw> will be deleted from 
the tree as soon as is safe. That usually means 
when the processing of their siblings
is finished. The following
example (taken from file C<examples/13ts_simplify_with_delete.pl> in 
the Parse::Eyapp distribution) 
illustrates how to eliminate CODE and syntactic terminals from the 
syntax tree:

 pl@nereida:~/src/perl/YappWithDefaultAction/examples$ \
        sed -ne '62,$p' 13ts_simplify_with_delete.pl | cat -n
  1  sub not_useful {
  2    my $self = shift; # node
  3    my $pat = $_[2];  # get the YATW object
  4
  5    (ref($self) eq 'CODE') or ((ref($self) eq 'TERMINAL') and ($self->{token} eq $self->{attr}))
  6      or do { return 0 };
  7    $pat->delete();
  8    return 1;
  9  }
 10
 11  Parse::Eyapp->new_grammar(
 12    input=>$translationscheme,
 13    classname=>'Calc',
 14    firstline =>7,
 15  );
 16  my $parser = Calc->new();                # Create the parser
 17
 18  $parser->YYData->{INPUT} = "2*3\n"; print $parser->YYData->{INPUT};
 19  my $t = $parser->Run;                    # Parse it
 20  print $t->str."\n";                      # Show the tree
 21  my $p = Parse::Eyapp::YATW->new(PATTERN => \&not_useful); 
 22  $p->s($t);                               # Delete nodes
 23  print $t->str."\n";                      # Show the tree

when executed we get the following output:

 pl@nereida:~/src/perl/YappWithDefaultAction/examples$ 13ts_simplify_with_delete.pl
 2*3
 EXP(TIMES(NUM(TERMINAL[2],CODE),TERMINAL[*],NUM(TERMINAL[3],CODE),CODE))
 EXP(TIMES(NUM(TERMINAL[2]),NUM(TERMINAL[3])))

=head2  $yatw->unshift

Tha call C<$yatw-E<gt>unshift($b)> 
safely unshifts (inserts at the beginning)
the node C<$b> in the list of its 
siblings of the node that matched (i.e in the list of siblings of C<$_[0]>). 
The following example
shows a YATW transformation
C<insert_child> that illustrates the use of C<unshift> (file C<examples/26delete_with_trreereg.pl>):

 pl@nereida:~/src/perl/YappWithDefaultAction/examples$ \
         sed -ne '70,$p' 26delete_with_trreereg.pl | cat -n
  1  my $transform = Parse::Eyapp::Treeregexp->new( STRING => q{
  2
  3      delete_code : CODE => { $delete_code->delete() }
  4
  5      {
  6        sub not_semantic {
  7          my $self = shift;
  8          return  1 if ((ref($self) eq 'TERMINAL') and ($self->{token} eq $self->{attr}));
  9          return 0;
 10        }
 11      }
 12
 13      delete_tokens : TERMINAL and { not_semantic($TERMINAL) } => {
 14        $delete_tokens->delete();
 15      }
 16
 17      insert_child : TIMES(NUM(TERMINAL), NUM(TERMINAL)) => {
 18        my $b = Parse::Eyapp::Node->new( 'UMINUS(TERMINAL)',
 19          sub { $_[1]->{attr} = '4.5' }); # The new node will be a sibling of TIMES
 20
 21        $insert_child->unshift($b); 
 22      }
 23    },
 24  )->generate();
 25
 26  Parse::Eyapp->new_grammar(
 27    input=>$translationscheme,
 28    classname=>'Calc',
 29    firstline =>7,
 30  );
 31  my $parser = Calc->new();                # Create the parser
 32
 33  $parser->YYData->{INPUT} = "2*3\n"; print $parser->YYData->{INPUT}; # Set the input
 34  my $t = $parser->Run;                # Parse it
 35  print $t->str."\n";                        # Show the tree
 36  # Get the AST
 37  our ($delete_tokens, $delete_code);
 38  $t->s($delete_tokens, $delete_code);
 39  print $t->str."\n";                        # Show the tree
 40  our $insert_child;
 41  $insert_child->s($t);
 42  print $t->str."\n";                        # Show the tree

When is executed the program produces the following output:

 pl@nereida:~/src/perl/YappWithDefaultAction/examples$ 26delete_with_trreereg.pl
 2*3
 EXP(TIMES(NUM(TERMINAL[2],CODE),TERMINAL[*],NUM(TERMINAL[3],CODE),CODE))
 EXP(TIMES(NUM(TERMINAL[2]),NUM(TERMINAL[3])))
 EXP(UMINUS(TERMINAL[4.5]),TIMES(NUM(TERMINAL[2]),NUM(TERMINAL[3])))

Don't try to take advantage that the transformation sub receives
in C<$_[1]> a reference to the father 
(see the section L<The YATW Tree Transformation Call Protocol>) 
and do something like: 

  unshift $_[1]->{children}, $b

it is unsafe.

=head2  $yatw->insert_before

A call to C<$yatw-E<gt>insert_before($node)> safely inserts 
C<$node> in the list of siblings of C<$_[0]>
just before C<$_[0]> (i.e. the ndoe that matched with C<$yatw>).
The following example (file C<t/33moveinvariantoutofloop.t>)
illustrates its use:

  my $p = Parse::Eyapp::Treeregexp->new( STRING => q{
    moveinvariant: WHILE(VAR($b), BLOCK(@a, ASSIGN($x, $e), @c)) 
         and { is_invariant($ASSIGN, $WHILE) } => {
           my $assign = $ASSIGN;
           $BLOCK->delete($ASSIGN);
           $moveinvariant->insert_before($assign);
         }
    },
  );

Here the C<ASSIGN($x, $e)> subtree - if is loop invariant - 
will be moved
to the list of siblings of C<$WHILE>
just before the C<$WHILE>.

=head1 Matching Trees

Both the transformation objects in C<Parse::Eyapp::YATW>
and the nodes in C<Parse::Eyapp::Node> have a method 
named C<m> for matching. 
For a C<Parse::Eyapp::YATW> object, the method -when called
in a list context- returns a list of 
C<Parse::Eyapp::Node::Match> nodes. 

                    @R = $t->m($yatw1, $yatw2, $yatw3, ...)

A C<Parse::Eyapp::Node::Match> 
object describes 
the nodes of the actual tree that have matched.
The nodes in the returned list are organized in a hierarchy.
They appear in the list 
sorted according to a depth-first visit of the actual tree C<$t>.
In a scalar context C<m> returns the first element of
the list.

Let us denote by C<$t> the actual tree being searched
and C<$r> one of the C<Parse::Eyapp::Node::Match>
nodes in the resulting forest C<@R>.
Then we have the following methods: 

=over 

=item *
The method C<$r-E<gt>node> return the node C<$t> of the actual 
tree that matched

=item *
The method C<$r-E<gt>father> returns the father of C<$r>
in the matching forest.
The father of C<$r> is defined by this property:
C<$r-E<gt>father-E<gt>node> is the nearest ancestor of
C<$r-E<gt>node> that matched with the treeregexp pattern.
That is, there is no ancestor that matched between
C<$r-E<gt>node> and C<$r-E<gt>father-E<gt>node>.
Otherwise C<$r-E<gt>father> is C<undef>

=item *

The method C<$r-E<gt>coord> returns the coordinates of C<$r-E<gt>node> 
relative to C<$t>.
For example, the coordinate C<".1.3.2"> 
denotes the node C<$t-E<gt>child(1)-E<gt>child(3)-E<gt>child(2)>, where C<$t>
is the root of the search.

=item *

The method C<$r-E<gt>depth> returns the depth of C<$r-E<gt>node> 
in C<$t>.

=item * 

When C<m> was called as a C<Parse::Eyapp::Node> method, i. e. 
with potentially more than one C<YATW> treeregexp, the method C<$r-E<gt>names>
returns the array of names of the transformations that matched with
C<$r-E<gt>node>.

=back

The following example illustrates a use of C<m> as 
a C<Parse::Eyapp:YATW> method.
It solves a problem of scope analysis in a C compiler:
matching each C<RETURN> statement with the function
that surrounds it. The parsing was already done, the 
AST was built and left in C<$t>. The treeregexp used is:

  retscope: /FUNCTION|RETURN/

and the code that solves the problem is:

 # Associate each "return exp" with its "function"
 my @returns = $retscope->m($t); 
 for (@returns) {
   my $node = $_->node;
   if (ref($node) eq 'RETURN') {
     my $function = $_->father->node; 
     $node->{function}  = $function;  
     $node->{t} = $function->{t};
   }
 }

The first line gets a list of C<Parse::Eyapp::Node::Match> nodes 
describing  the actual nodes that matched C</FUNCTION|RETURN/>. 
If the node described by C<$_> is a C<'RETURN'> node,
the expresion C< $_-E<gt>father-E<gt>node> must necessarily point
to the function node that encloses it. 

The second example shows the use of C<m> as
a C<Parse::Eyapp::Node> method.

 pl@nereida:~/src/perl/YappWithDefaultAction/examples$ cat -n m2.pl
  1  #!/usr/bin/perl -w
  2  use strict;
  3  use Rule6;
  4  use Parse::Eyapp::Treeregexp;
  5
  6  Parse::Eyapp::Treeregexp->new( STRING => q{
  7    fold: /times|plus|div|minus/i:bin(NUM($n), NUM($m))
  8    zxw: TIMES(NUM($x), .) and { $x->{attr} == 0 }
  9    wxz: TIMES(., NUM($x)) and { $x->{attr} == 0 }
 10  })->generate();
 11
 12  # Syntax analysis
 13  my $parser = new Rule6();
 14  $parser->YYData->{INPUT} = "0*0*0";
 15  my $t = $parser->Run;
 16  print "Tree:",$t->str,"\n";
 17
 18  # Search
 19  my $m = $t->m(our ($fold, $zxw, $wxz));
 20  print "Match Node:\n",$m->str,"\n";


When executed with input C<0*0*0> the program generates this output:

 pl@nereida:~/src/perl/YappWithDefaultAction/examples$ m2.pl
 Tree:TIMES(TIMES(NUM(TERMINAL),NUM(TERMINAL)),NUM(TERMINAL))
 Match Node:
 Match[[TIMES:0:wxz]](Match[[TIMES:1:fold,zxw,wxz]])

The representation of C<Match> nodes by C<str> deserves a comment.
C<Match> nodes have their own C<info> method. It returns a string
containing the concatenation of the class of C<$r-E<gt>node> 
(i.e. the actual node that matched), the depth
(C<$r-E<gt>depth>) and the names of the transformations
that matched (as provided by the method C<$r-E<gt>names>) 

=head2  The C<SEVERITY> option of C<Parse::Eyapp::Treeregexp::new>

The C<SEVERITY> option of C<Parse::Eyapp::Treeregexp::new> controls the
way matching succeeds regarding the number of children.
To illustrate its use let us consider the following example.
The grammar used C<Rule6.yp> is similar
to the one in the L<SYNOPSIS> example.

 pl@nereida:~/src/perl/YappWithDefaultAction/examples$ cat -n numchildren.pl
  1  #!/usr/bin/perl -w
  2  use strict;
  3  use Rule6;
  4  use Parse::Eyapp::Treeregexp;
  5
  6  sub TERMINAL::info { $_[0]{attr} }
  7
  8  my $severity = shift || 0;
  9  my $parser = new Rule6();
 10  $parser->YYData->{INPUT} = shift || '0*2';
 11  my $t = $parser->Run;
 12
 13  my $transform = Parse::Eyapp::Treeregexp->new(
 14    STRING => q{
 15      zero_times_whatever: TIMES(NUM($x)) and { $x->{attr} == 0 } => { $_[0] = $NUM }
 16    },
 17    SEVERITY => $severity,
 18    FIRSTLINE => 14,
 19  )->generate;
 20
 21  $t->s(our @all);
 22
 23  print $t->str,"\n";


The program gets the severity level from the command line (line 9).
The specification of the term C<TIMES(NUM($x))> inside the
transformation C<zero_times_whatever> does not
clearly state that C<TIMES> must have two children.
There are several interpretations of the treregexp depending
on the level fixed for C<SEVERITY>:

=over

=item *
0: C<TIMES> must have at least one child. Don't care if it has more.

=item *
1: C<TIMES> must have exactly one child.

=item *
2: C<TIMES> must have exactly one child. When visit a 
C<TIMES> node with a different number of children issue a warning.

=item *
3: C<TIMES> must have exactly one child.  When visit a
C<TIMES> node with a different number of children issue an
error. 

=back

Observe the change in behavior according to the level of C<SEVERITY>:

 pl@nereida:~/src/perl/YappWithDefaultAction/examples$ numchildren.pl 0 '0*2'
 NUM(TERMINAL[0])
 pl@nereida:~/src/perl/YappWithDefaultAction/examples$ numchildren.pl 1 '0*2'
 TIMES(NUM(TERMINAL[0]),NUM(TERMINAL[2]))
 pl@nereida:~/src/perl/YappWithDefaultAction/examples$ numchildren.pl 2 '0*2'
 Warning! found node TIMES with 2 children.
 Expected 1 children (see line 15 of ./numchildren.pl)"
 TIMES(NUM(TERMINAL[0]),NUM(TERMINAL[2]))
 pl@nereida:~/src/perl/YappWithDefaultAction/examples$ numchildren.pl 3 '0*2'
 Error! found node TIMES with 2 children.
 Expected 1 children (see line 15 of ./numchildren.pl)"
  at (eval 2) line 29


=head1 Tree Substitution: The C<s> methods

Both C<Parse::Eyapp:Node> and C<Parse::Eyapp::YATW> objects (i.e.
nodes and tree transformations) are provided with a C<s> method.

In the case of a C<Parse::Eyapp::YATW> object the method C<s>
applies the tree transformation using a single bottom-up traversing:
the transformation is recursively applied to the children and 
then to the current node.

For C<Parse::Eyapp:Node> nodes the set of transformations is applied
to each node until no transformation matches any more.
The example in the L</SYNOPSIS> section illustrates the use:

  1  # Let us transform the tree. Define the tree-regular expressions ..
  2  my $p = Parse::Eyapp::Treeregexp->new( STRING => q{
  3    { #  Example of support code
  4      my %Op = (PLUS=>'+', MINUS => '-', TIMES=>'*', DIV => '/');
  5    }
  6    constantfold: /TIMES|PLUS|DIV|MINUS/:bin(NUM($x), NUM($y))
  7      => {
  8        my $op = $Op{ref($_[0])};
  9        $x->{attr} = eval  "$x->{attr} $op $y->{attr}";
 10        $_[0] = $NUM[0];
 11      }
 12    uminus: UMINUS(NUM($x)) => { $x->{attr} = -$x->{attr}; $_[0] = $NUM }
 13    zero_times_whatever: TIMES(NUM($x), .) and { $x->{attr} == 0 } => { $_[0] = $NUM }
 14    whatever_times_zero: TIMES(., NUM($x)) and { $x->{attr} == 0 } => { $_[0] = $NUM }
 15    },
 16    OUTPUTFILE=> 'main.pm'
 17  );
 18  $p->generate(); # Create the tranformations
 19 
 20  $t->s($uminus); # Transform UMINUS nodes
 21  $t->s(@all);    # constant folding and mult. by zero

The call at line 20 can be substituted by C<$uminus-E<gt>s($t)>
without changes.

=head1 Scope Analysis with Parse::Eyapp::Scope

A scope manager helps to compute the mapping function
that maps the uses (instances) of 
source objects to their definitions. For instance, 
in I<identifier scope analysis> the problem is to associate
each ocurrence of an identifier with the declaration
that applies to it. Another example is I<loop scope analysis>
where the problem is to associate each occurrence
of a C<CONTINUE> or C<BREAK> node with the 
shallowest C<LOOP> that encloses it. Or I<label scope
analysis>, the problem to associate a C<GOTO>
node with the node to jump to, that is,
with the C<STATEMENT> associated with the label.

To take advantage of C<Parse::Eyapp::Scope>, 
the compiler writer must mark at the appropriate time 
(for example a new block or new subroutine for I<identifier scope analysis>,
a new loop for I<loop scope analysis>, etc.) the I<beginning of a new scope>
calling the method L<begin_scope|/$scope-E<gt>begin_scope>.
From that point on any I<ocurring instance> of an object 
(for example,
variables in expressions for I<identifier scope analysis>, breaks and continues
for I<loop scope analysis>, etc.) must be declared 
calling the method L<scope_instance|/$scope-E<gt>scope_instance>.
The programmer must also mark the I<end of the current scope> 
at the appropriate time. 

=head2 $scope->end_scope

There are two ways of calling C<$scope-E<gt>end_scope>.
The first one is for Scope Analysis Problems where
a symbol table is needed (for example in I<identifier scope analysis>
and I<label scope analysis>.

=head3 $scope->end_scope with first Arg a Symbol Table

For each I<ocurring instance> of an object C<$x>
that occurred since the last call to  L<begin_scope|/$scope-E<gt>begin_scope>
the call to 

  $scope->end_scope(\%symboltable, $definition_node, 'attr1', 'attr2', ... )

decorates the I<ocurring instance> C<$x> with several attributes: 

=over

=item * An entry C<$x-E<gt>{SCOPE_NAME}> is built that will reference C<$definition_node>.

=item * An entry C<$x-E<gt>{ENTRY_NAME}> is built. That
entry references C<$symboltable{$x-E<gt>key}> (to have a
faster access from the instance to the attributes of the object).
The instantiated nodes must have a C<$x-E<gt>key> method which provides
the entry for the node in the symbol table:

  pl@nereida:~/src/perl/YappWithDefaultAction/examples$ sed -ne '651,657p' Types.eyp
  sub VAR::key {
    my $self = shift;

    return $self->child(0)->{attr}[0];
  }

  *VARARRAY::key = *FUNCTIONCALL::key = \&VAR::key;

=item * For each aditional arguments C<attr#k> an
entry C<$x-E<gt>{attr#k>} will be built.
That entry references C<$symboltable{$x-E<gt>key}{attr#k}>. Therefore
the entry for C<$x> in the symbol table must already 
have a field named C<attr#k>.

=back

In a list context C<$scopeE<gt>end_scope> returns
two references. The first one
is a reference to a list of node instantiated
that weren't defined in the current scope.
The second is a reference to a list of nodes
that were defined in this scope. 
In a scalar context returns the first of these two.
An instance C<$x> is I<defined> if, and only if, 
C<exists $symboltable{$_-E<gt>key}>.

=head3 $scope->end_scope for Simple Scope Analysis

Some scope analysis problems do not require the existence
of a symbol table (for instance, the problem of associating
a C<RETURN> node with the C<FUNCTION> that encloses it). 
For such kind of problems C<$scopeE<gt>end_scope> provides
a second form of call.
The second way to call C<$scopeE<gt>end_scope> is

                 $declared = $scopemanager->end_scope($definition_node);

The only argument is the reference to the node that controls/defines
the scope. The method returns a reference to the declared
nodes. Any node instanced with C<scope_instance>
since the last call to C<begin_scope> is considered I<declared>.

=head2 $scope->begin_scope

Marks the beginning of an scope.
Example (file C<examples/Types.eyp>):

   loopPrefix:
       $WHILE '(' expression ')'
         {
           $loops->begin_scope;
           $_[3]->{line} = $WHILE->[1]; # Save the line for error diagostic
           $_[3]
         }

=head2 $scope->scope_instance

Declares the node argument to be an occurring instance of the scope:

   nereida:~/doc/casiano/PLBOOK/PLBOOK/code> \
       sed -ne '375,380p' Simple6.eyp | cat -n
    1      $Variable '=' binary
    2        {
    3          my $parser = shift;
    4          $ids->scope_instance($Variable);
    5          $parser->YYBuildAST(@_); # "Manually" build the node
    6        }


=head2 Parse::Eyapp::Scope->new

C<Parse::Eyapp::Scope-E<gt>new> returns a scope managment object. 
The scope mapping function is implemented 
by C<Parse::Eyapp::Scope> through a set of attributes
that are added to the nodes involved in the scope analysis.
The names of these attributes can be specified 
using the parameters of C<Parse::Eyapp::Scope-E<gt>new>.
The arguments of C<new> are:

=over

=item * C<SCOPE_NAME> 
is the name chosen for the attribute of the 
I<node instance>  which will held
the reference to the I<definition node>.
If not specified it will take the value C<"scope">.

=item * C<ENTRY_NAME> is the name of the attribute of the
I<node instance>  which will held
the reference to the symbol table entry.
By default takes the value C<"entry">.

=item * C<SCOPE_DEPTH> is the name for an attribute of the 
I<definition node>. Optional. If not specified it will not be
defined.

=back

=head1 ENVIRONMENT
 
Remember to set the environment variable C<PERL5LIB>
if you decide to install C<Parse::Eyapp> at a location other than the standard.
For example, on a bash or sh:

  export PERL5LIB-/home/user/wherever_it_is/lib/:$PERL5LIB

on a C<csh> or C<tcsh>

  setenv PERL5LIB /home/user/wherever_it_is/lib/:$PERL5LIB

Be sure the scripts C<eyapp> and C<treereg> are in the execution PATH.
 
=head1 DEPENDENCIES
 
This distribution depends on the following modules:

=over

=item * L<List::MoreUtils>

=item * L<List::Util>

=item * L<Data::Dumper>

=item * L<Pod::Usage>

=back 

It seems that L<List::Util> is in the core of Perl
distributions since version 5.73:

  > perl -MModule::CoreList -e 'print Module::CoreList->first_release("List::Util")'
  5.007003

and L<Data::Dumper> is also in the core since 5.5:

  > perl -MModule::CoreList -e 'print Module::CoreList->first_release("Data::Dumper")'
  5.005

and L<Pod::Usage> is also in the core since 5.6:

  > perl -MModule::CoreList -e 'print Module::CoreList->first_release("Pod::Usage")'
  5.006

=for none
  > perl -MModule::CoreList -e 'print Module::CoreList->first_release("Memoize")'
  5.007003

I also recommend the following modules:

=over

=item * L<Test::Pod>

=item * L<Test::Warn>

=item * L<Test::Exception>

=back

The dependence on  L<Test::Warn>, L<Test::Pod> and L<Test::Exception> is merely for
the execution of tests. If the modules aren't installed the tests
depending on them will be skipped.

=head1 INSTALLATION

To install it, follow the traditional mantra:

                                 perl Makefile.PL
                                 make
                                 make test
                                 make install

Also:

=over


=item * Make a local copy of the C<examples/> directory in this distribution

=item * Probably it will be also a good idea to make a copy of the tests in the C<t/> directory.
  They also illustrate the use of Eyapp

=item * Print and read the pdf file in L<http://nereida.deioc.ull.es/~pl/perlexamples/Eyapp.pdf> 

=back

=head1 BUGS AND LIMITATIONS
 
=over

=item *
This distribution is an alpha version. 
A release will be in CPAN by February 2007.
Hopefully, at that time the interface will freeze or -at least-
changes in the API will be minor. In the meanwhile 
it will be likely to change.

=item *
The way Parse::Eyapp parses Perl code is verbatim the way it does Parse::Yapp 1.05.
Quoting Francois Desarmenien L<Parse::Yapp> documentation:

"Be aware that matching braces in Perl is much more difficult than
in C: inside strings they don't need to match. While in C it is
very easy to detect the beginning of a string construct, or a
single character, it is much more difficult in Perl, as there
are so many ways of writing such literals. So there is no check
for that today. If you need a brace in a double-quoted string, just
quote it (C<\{> or C<\}>). For single-quoted strings, you will need
to make a comment matching it I<in th right order>.
Sorry for the inconvenience.

    {
        "{ My string block }".
        "\{ My other string block \}".
        qq/ My unmatched brace \} /.
        # Force the match: {
        q/ for my closing brace } /
        q/ My opening brace { /
        # must be closed: }
    }

All of these constructs should work."

Alternative I<exact solutions> were tried but resulted in much slower
code. Therefore, until something faster is found, I rather prefer for
Parse::Eyapp to live with this limitation.

The same limitation may appear inside header code (code between C<%{> and C<%}>)

=item * 

English is not 
my native language. For sure this text has
lexical, syntactic and semantic errors. 
I'll be most gratefull to know about 
any typos, grammar mistakes, 
ways to rewrite paragraphs and misconceptions
you have found. 

=item *

There are unknown bugs. 
Please report problems to Casiano Rodriguez-Leon (casiano@cpan.org).
 
=back

=head1 SEE ALSO

=over

=item * L<eyapptut>

=item * The pdf file in L<http://nereida.deioc.ull.es/~pl/perlexamples/Eyapp.pdf> 

=item *
perldoc L<eyapp>, 

=item *
perldoc L<treereg>,

=item * I<An�lisis L�xico y Sint�ctico>, (Notes for a course in compiler 
construction) by  Casiano Rodriguez-Leon. 
Available at  L<http://nereida.deioc.ull.es/~pl/perlexamples/>
Is the more complete and reliable source for Parse::Eyapp. However is in Spanish.


=item *
L<Parse::Yapp>,

=item *
Man pages of yacc(1),

=item *
Man pages of bison(1),

=item *
L<Language::AttributeGrammar>

=item *
L<Parse::RecDescent>.

=back

=head1 REFERENCES

=over

=item *
The classic Dragon's book I<Compilers: Principles, Techniques, and Tools> 
by Alfred V. Aho, Ravi Sethi and
Jeffrey D. Ullman (Addison-Wesley 1986)

=back

=head1 AUTHOR
 
Casiano Rodriguez-Leon (casiano@ull.es)

A large percentage of  code is verbatim taken from Parse::Yapp 1.05.
The author of Parse::Yapp is Francois Desarmenien.
 
=head1 ACKNOWLEDGMENTS

This work has been supported by CEE (FEDER) and the Spanish Ministry of
I<Educaci�n y Ciencia> through I<Plan Nacional I+D+I> number TIN2005-08818-C04-04
(ULL::OPLINK project L<http://www.oplink.ull.es/>). 
Support from Gobierno de Canarias was through GC02210601
(I<Grupos Consolidados>).
The University of La Laguna has also supported my work in many ways
and for many years.

I wish to thank Francois Desarmenien for his C<Parse::Yapp> module, 
to my students at La Laguna and to the Perl Community. Special thanks to 
my family and Larry Wall.

=head1 LICENCE AND COPYRIGHT
 
Copyright (c) 2006-2007 Casiano Rodriguez-Leon (casiano@ull.es). All rights reserved.

Parse::Yapp copyright is of Francois Desarmenien, all rights reserved. 1998-2001
 
These modules are free software; you can redistribute it and/or
modify it under the same terms as Perl itself. See L<perlartistic>.
 
This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 

