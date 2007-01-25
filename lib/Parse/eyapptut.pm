#
# Module Parse::Eyapp.pm.
#
#
package Parse::eyapptut;

our $VERSION = $Parse::Eyapp::Driver::VERSION;

1;

__END__

=head1 NAME
 
Parse::Eyapp 
 
 
=head1 VERSION
 
1.06503

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
       my $op = $Op{ref($_[0])};
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

  
=head1 Introduction to C<Parse::Eyapp> 

Parse::Eyapp (Extended yapp) is a collection of modules
that extends Francois Desarmenien Parse::Yapp 1.05.
Eyapp extends yacc/yapp syntax with 
the functionalities briefly described in this section.
This is an introductory tutorial. For a reference guide
see L<Parse::Eyapp>. If you are not familiar with L<yacc>
or L<yapp> and you can speak Spanish start reading the contents in
L<http://nereida.deioc.ull.es/~pl/perlexamples/chapter_parseeyapp.html>.


=head1 Input from strings

Grammars can be compiled from a file or from source on the fly
(See the synopsis section for an example).

=head1 Names for attributes

Attributes can be referenced by meaningful names instead
of the classic error-prone positional approach using the B<dot notation>
like in:

       exp : exp.left '-' exp.right  { $left - $right }

By qualifying the first appearance of the syntactic variable C<exp>
with the notation C<exp.left> we can later refer inside the actions
to the associated attribute using the lexical variable
C<$left>.  The B<dolar notation> C<$A> can be used as an abbreviation
of C<A.A>. For example:

       exp:  -' $exp %prec NEG { -$exp }

=head1 Lists and Optionals

Lists, optional lists, list separated by tokens, etc. like in the start
rule in the Synopsis example can be used:

         line: exp <%name EXPRESION_LIST + ';'>  { $_[1] } 

which defines C<line> as the language of non empty lists of C<exp> elements
separated by semicolons. The use of C<%name EXPRESION_LIST> gives a name
to the created list. Actually the right hand side of this production has 
only one element which is the reference to the list.
The associated action C<{ $_[1] }> makes the generated parser to return 
the reference to such list.

The former rule
is almost equivalent to: 

      line: line ';' exp  { push $_[1]->{children}, $_[3] }
          | exp           { bless { children => [ $_[1] ] }, 'EXPRESION_LIST' }



=head1 Default actions

When no action is specified both C<yapp> and C<eyapp>
implicitly insert the semantic action C<{ $_[1] }>. 
In C<Parse::Eyapp> you can modify such behavior using the C<%defaultaction { Perl code }>
directive. The Perl code that follows the directive is
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

=head2 Compiling with C<eyapp>

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

=head1 Abstract Syntax Trees

C<Parse::Eyapp> facilitates the construction of concrete syntax trees and 
abstract syntax trees (abbreviated AST from now on) through the C<%tree>
directive. 
Nodes in the AST are blessed in the production
C<name>. 
By default the name of a production is the concatenation
of the left hand side and the production number. The production number
is the ordinal number of the production as they appear in the associated 
C<.output> file (see option C<-v> of L<eyapp>) However, a production can be 
I<named> using the C<%name> directive. Therefore, in the following
code:

 exp:
        %name NUM  NUM            
      | %name VAR   VAR         
      | %name ASSIGN VAR '=' exp
      . .............................
      | %name UMINUS '-' exp %prec NEG
      |   '(' exp ')'  { $_[2] }  /* Let us simplify a bit the tree */

we are explictly naming the productions. Thus, the node corresponding to the 
production C<exp: VAR '=' exp> will be named C<ASSIGN>.
Explicit actions can be specified by the programmer like in 

      |   '(' exp ')'  { $_[2] }  /* Let us simplify a bit the tree */

the action receives as arguments the references to the children nodes already 
built. The programmer can influence the shape of the tree by inserting
this explicit actions. In the example the programmer has decided to simplify the 
syntax tree: the nodes associated with the parenthesis are 
discarded and the reference to the subtree containing the proper
expression is returned. 

When a I<explicit user action> returns s.t. that is not a reference
no child will be inserted in the father of the current
production.

=head2 Displaying Trees

All the node classes build by C<%tree>
inherit from C<Parse::Eyapp::Node> and consequently have
acces to the methods provided in such module. Among them is the C<str> method
which dumps the tree. The I<str> method traverses the syntax tree dumping the type
of the node being visited in a string. If the node has a method C<info> it will
be executed and its result concatenated to the string. Thus, in the Synopsis
example, by adding the C<info> method to the class C<TERMINAL>:

 sub TERMINAL::info {
   $_[0]{attr}
 }

we achieve the insertion of attributes in the string build 
by C<str> (see the partial output of C<synopsis.pl> in section
L</Syntactic and Semantic tokens>).

The existence of some methods (like C<footnote>) and
the values of some package variables
influence the behavior of C<str>. Among the most
important are:

  @PREFIXES = qw(Parse::Eyapp::Node::); # Prefixes to supress 
  $INDENT = 0; # 0 = compact, 1 = indent, 2 = indent and include Types in closing parenthesis
  $STRSEP = ',';
  $DELIMITER = '[';
  $FOOTNOTE_HEADER = "\n---------------------------\n";
  $FOOTNOTE_SEP = ")\n";
  $FOOTNOTE_LEFT = '^{';
  $FOOTNOTE_RIGHT = '}';
  $LINESEP = 4;

=head2 C<TERMINAL> nodes

Nodes named C<TERMINAL> correspond to 
tokens provided by the lexical analyzer. They are C<Parse::Eyapp::Node>
nodes (hashes) with an attribute C<attr> holding the attribute provided
by the lexical analyzer. The C<attr> method can be used to get/set the 
attribute.

=head2 User Attributes and System Attributes

All the nodes in the AST are C<Parse::Eyapp::Node> nodes.
They are hashes that the user can decorate with new keys/attributes.
The only reserved words are those listed in the reference section.
Basically they have a C<children> key. C<TERMINAL>  nodes have the 
C<attr> key. 


=head2 Syntactic and Semantic tokens

C<Parse::Eyapp> diferences between C<syntactic tokens>
and C<semantic tokens>. By default all tokens
declared using string notation (i.e. between quotes
like C<'+'>, C<'='>, in the Synopsis example)
are considered C<syntactic tokens>. Tokens declared by an identifier
(like C<NUM> or C<VAR> in the Synopsis example) are by default considered
C<semantic tokens>. B<Syntactic tokens are eliminated when building the 
syntactic tree>. Thus, the first print in the former Synopsis example:

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

=head2 Saving the Information In Syntactic Tokens

The reason for the adjective C<%syntactic> applied to a token is to 
state that the token influences the shape of the syntax tree
but carries no other information. When the tree is built
the node corresponding to the token is discarded.

Sometimes the difference between syntactic and semantic 
tokens is blurred. For example the line number associated
with an instance of the syntactic token C<'+'> can be used later
-say during type checking- to emit a more accurate error
diagnostic. But if the node was discarded the information
about that line number is no longer available.
When building the syntax tree C<Parse::Eyapp> (namely
the method C<Parse::Eyapp::YYBuildAST>) checks 
a C<TERMINAL::save_attributes> method exists and if so
it will be called when visiting a syntactic terminal. 
The method receives as argument - additionally
to the reference to the C<TERMINAL> node - a reference
to the node associated with the left hand side of the
production. Here is an example (file C<examples/Types.eyp>)
of use:

  sub TERMINAL::save_attributes {
    # $_[0] is a syntactic terminal
    # $_[1] is the father.
    push @{$_[1]->{lines}}, $_[0]->[1]; # save the line!
  }

=head2 The directives C<%syntactic token> and  C<%semantic token>

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

the tree build for input C<abc> will be C<ABC(A(TERMINAL),B,C(TERMINAL))>.

=head2 The  C<bypass> clause and the C<%no bypass> directive

The shape of the tree can be also modified using some C<%tree> clauses
as C<%tree bypass> which will produce an automatic I<bypass> of any
node with only one child at tree-construction-time. 

A I<bypass operation> consists in I<returning the only child 
of the node being visited to the father of the node and re-typing (re-blessing)
the node in the name of the production> (if a name is provided). 

A node may have only one child at tree-construction-time for one of
two reasons. 

=over

=item *
The first occurs when the right hand side of the production
was already unary like in:

                           exp:
                               %name NUM  NUM 

Here the C<NUM> node will be bypassed and the child C<TERMINAL> built
from the information provided by the lexical analyzer will be renamed
as C<NUM>.
  
=item *
Another reason for a node to be I<bypassed> is  the fact that though the right
hand side of the production may have more than one symbol, 
only one of them is not a syntactic token
like in:

                           exp: '(' exp ')'

=back

As consequence of the blind application of the I<bypass rule>
undesired bypasses may occur like in

                           exp : %name UMINUS
                                 '-' $exp %prec NEG

though the right hand side has two symbols, token C<'-'> is
a syntactic token and therefore only C<exp> is left. The I<bypass>
operation will be applied when building this node.
This I<bypass> can be avoided applying the C<no bypass ID> directive to the corresponding 
production:

                           exp : %no bypass UMINUS
                                 '-' $exp %prec NEG

The following example is the equivalent of the I<Synopsis example>
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

when running this example we obtain the following output:

 nereida:~/src/perl/YappWithDefaultAction/examples> bypass.pl

 ************
 EXPRESION_LIST(ASSIGN(TERMINAL[a],PLUS(TIMES(NUM[2],UMINUS(NUM[3])),TIMES(VAR[b],NUM[0]))))
 ************
 EXPRESION_LIST(ASSIGN(TERMINAL[a],NUM[-6]))

As you can see the trees are more compact when using the C<bypass> directive.

=head2 Explictly building nodes with the C<YYBuildAST> method

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

This example defines the expression to access an array element 
as an identifier followed by
a non empty list of binary expressions. The node corresponding
to the list of indices has been named C<INDEXSPEC>. 

When no explicit action is
inserted a binary node will be built having as first child the node
corresponding to the identifier C<$ID> and as second child the reference 
to the list of binary expressions. However, the programmer wants to decorate
the node being built with a C<line> attribute holding the line number in the source
code where the identifier being used appears. The call to the C<Parse::Eyapp::Driver>
method C<YYBuildAST> does the job of building the node. After
that the node can be decorated and returned. 

Actually, the C<%tree> directive is semantically equivalent to:

  %default action { goto &Parse::Eyapp::Driver::YYBuildAST }

=head2 The C<child> and C< descendant> methods

Access to the children of the AST is achieved through the C<children> and C<child>
methods. More general is the C< descendant> method that
returns the descendant of a node given its coordinates. See a session
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

=head2 The C<alias> clause of the C<%tree> directive

There are occasions however where access by name to the children may be preferable.
The use of the C<alias> clause with the C<%tree> directive creates accessors
to the children with names specified by the programmer. The dot and dolar notations
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
of the AST nodes. Follows an example of a small calculator:

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

=head1 Tree Regular Expressions

C<Parse::Eyapp> introduces a new language
called I<Tree Regular Expressions> that easies the 
transformation of trees. Let us recall the previous
example used in the C<bypass> section:

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

The call to the constructor C<new> builds a C<Parse::Eyapp::Treeregexp> object.
The subsequent call to the method C<$p-E<gt>generate> compiles the object producing
tree-transformations built according to the specification
given in the treeregexp program. A tree transformation is a C<Parse::Eyapp::YATW> 
object. The example contains four tree programa transformations
named C<constantfold>, C<zero_times_whatever>, C<whatever_times_zero>
and <uminus>. These transformations can be grouped in transformation 
families. Such families of transformations can be applied 
to any C<Parse::Eyapp::Node> trees.
An special variable C<@PACKAGE::all> refers to the whole set of transformations
in the program.
Here C<PACKAGE> refers to the package where the transformations live.
When no C<PACKAGE> argument is specified in the call to C<new> - as is the case in this example -
the package of the caller is used instead.
The call C<$t-E<gt>s(@all)> proceeds to the execution of the method C<s> (for
substitution) using
all the specified transformations. The transformations will be iteratively applied
to all nodes of the tree until there are no changes.
Summarizing, that means that 

=over

=item * All 
C<UMINUS> nodes whose only child
is a number C<NUM> will be substituted by the  C<NUM> node but with the sign changed

=item Constant folding will be applied: trees representing
constants expressions will be substituted by a C<NUM> node representing its value
 
=item All the C<TIMES> nodes with one child holding the value 0 will be substituted
by that child

=back

=head2 The Syntax of Treeregexp

The example illustrates the syntax of the language. A tree transformation
conforms to the syntax:

  treeregexp: 
      IDENT ':' treereg ('and' CODE)?  ('=>' CODE)?  

like in:

 zero_times_whatever: TIMES(NUM, .) and { $NUM->{attr} == 0 } => { $_[0] = $NUM }

The C<IDENT> is the name given to the tree transformation. A tree transformation
is actually a C<Parse::Eyapp::YATW> object. 
After generation time two package objects are created per transformation:

=over

=item *
A subroutine with name C<zero_times_whatever> holding the actual 
code for the tree transformation will be available and 

=item *
A scalar variable named C<$zero_times_whatever> will refer to the 
C<Parse::Eyapp::YATW>  tree transformation object. 

=back

These names
live in the package specified by the user in the call to C<new> through the C<PACKAGE>
argument. When
no package name is specified the name of the caller package is used instead.

After the C<IDENT> and the colon comes the B<treeregexp>. The treeregexp 
is a term, that is a parenthesized description of the shape of the tree
like C<TIMES(NUM, .)> which says: I<match nodes of type> C<TIMES>
I<whose left child is a> C<NUM> I<and whose right child is whatever>.
The dot stands for I<whatever> and is a treeregexp that matches any node. 

Then comes the reserved word C<and> and some Perl code specifying
the semantic conditions for the node being visited to match

                              { $NUM->{attr} == 0 }

The code can access to the different subtrees using lexical variables
whose names match the type of the node. Thus, in the example:

 zero_times_whatever: TIMES(NUM, .) and { $NUM->{attr} == 0 } => { $_[0] = $NUM }

variable C<$NUM> refers to the left child while variables 
C<$TIMES> and C<$_[0]> will refer refer to the node being visited.
When more than one node of the same
type exists (for instance C<TIMES(NUM,NUM)>)
the associated lexical variable changes its type from scalar to array and thus
if several C<NUM> nodes appear in the term we will speak about 
C<$NUM[0]>, C<$NUM[1]>, etc.

=head2 Separated Compilation with C<treereg>

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

 nereida:~/src/perl/YappWithDefaultAction/examples> treereg Shift
 nereida:~/src/perl/YappWithDefaultAction/examples> ls -ltr | tail -1
 -rw-rw----  1 pl users   1405 2006-11-06 14:09 Shift.pm

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
  7  sub SHIFTLEFT::info { $_[0]{shift} }
  8
  9  $Data::Dumper::Indent = 1;
 10  my $parser = new Rule6();
 11  $parser->YYData->{INPUT} = <>;
 12  my $t = $parser->Run;
 13  print "***********\n",$t->str,"\n";
 14  $t->s(@Shift::all);
 15  print "***********\n",$t->str,"\n";

Multiplications by a power of two are substituted by the corresponding shifts:

 nereida:~/src/perl/YappWithDefaultAction/examples> useruleandshift.pl
 a=b*8
 ***********
 ASSIGN(TERMINAL[a],TIMES(VAR(TERMINAL[b]),NUM(TERMINAL[8])))
 ***********
 ASSIGN(TERMINAL[a],SHIFTLEFT[3])

=head2 Regexp Treeregexps

We can use an ordinary regular expression C<regexp> inside 
the term part of a treeregexp.
The C<constantfold> transformation in the Synopsis example
shows how:

   constantfold: /TIMES|PLUS|DIV|MINUS/:bin(NUM($x), NUM($y))
     => {
       my $op = $Op{ref($bin)};
       $x->{attr} = eval  "$x->{attr} $op $y->{attr}";
       $_[0] = $NUM[0];
     }

The regexp is specified between division slashes C</>.
It is legal to specify options after the second slash (like C<e>, C<i>, etc.).
The optional identifier C<bin> after the regexp indicates the name for the lexical
variable holding a copy that references the node.
If no identifier is specified, the special variable C<$W> is used instead.
If the treeregexp has several anonymous regexp or dot treeregexps
they will be stored in the array variable C<@W>.

The operation of the ordinary string oriented regexps are slightly modified
when they are used inside a treeregexp.
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

The following fragment of the type checking stage of a simple 
compiler shows that C<x> is implictly assumed:
 
 # Binary Operations
 bin: / PLUS
       |MINUS
       |TIMES
       |DIV
       |MOD
       |GT
       |GE
       |LE
       |EQ
       |NE
       |LT
       |AND
       |EXP
       |OR
      /($x, $y)
   => {
     $x = char2int($_[0], 0);
     $y = char2int($_[0], 1);

     if (($x->{t} == $INT) and ( $y->{t} == $INT)) {
       $_[0]->{t} = $INT;
       return 1;
     }
     type_error("Incompatible types with operator '".($_[0]->lexeme)."'", $_[0]->line);
   }

With the natural Perl regexp semantic the language reserved
word C<WHILE> would match the regexp (see the C<LE> for I<less or equal>)
leading to an erroneous
type checking. The automatic insertion of word anchors prevent it.

=head2 Matching Trees

Both the transformation objects in C<Parse::Eyapp::YATW>
and the nodes in C<Parse::Eyapp::Node> have a method 
named C<m> for matching. 

For a C<Parse::Eyapp::YATW> object, the method -when called
in a list context- returns a list of 
C<Parse::Eyapp::Node::Match> nodes referencing
the nodes of the actual tree that have matched.
The nodes in the list are organized in a hierarchy.

The nodes are sorted in the list of trees (a forest)
according to a depth-first visit of the actual tree C<$t>.

In a scalar context C<m> returns the first element of
the list.

Let us denote by C<$t> the actual tree being searched
and C<$r> one of the C<Parse::Eyapp::Node::Match>
nodes in the resulting forest.
Then we have the following methods: 

=over 

=item *
The method C<$r-E<gt>node> return the node C<$t> of the actual 
tree that matched

=item *
The method C<$r-E<gt>father> returns the tree in the matching forest.
The father is defined by this property:
C<$r-E<gt>father-E<gt>node> is the nearest ancestor of
C<$r-E<gt>node> that matched with the treeregexp pattern.
That is, there is no ancestor that matched between
C<$r-E<gt>node> and C<$r-E<gt>father-E<gt>node>.
Otherwise C<$r-E<gt>father> is C<undef>

=item *

The method C<$r-E<gt>coord> returns the coordinates of the actual tree
that matched using s.t similar to the Dewey notation.
for example, the coordinate C<".1.3.2"> 
denotes the node C<$t-E<gt>child(1)-E<gt>child(3)-E<gt>child(2)>, where C<$t>
is the root of the search.

=item *

The method C<$r-E<gt>depth> returns the depth of C<$r-E<gt>node> 
in C<$t>.

=item * 

When called as a C<Parse::Eyapp::Node> method, C<$r-E<gt>names>
returns the array of names of the transformations that matched.

=back

The following example illustrates a use of C<m> as 
a C<Parse::Eyapp:YATW> method.
It solves a problem of scope analysis in a C compiler:
matching each C<RETURN> statement with the function
that surrounds it. The treeregexp used is:

  retscope: /FUNCTION|RETURN/

and the code that solves the problem is:

 # Scope Analysis: Return-Function
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
to the function node that surrounds it. 

The second example shows the use of C<m> as
a C<Parse::Eyapp::Node> method.

 nereida:~/src/perl/YappWithDefaultAction/examples> cat -n m2.pl
  1  #!/usr/bin/perl -w
  2  use strict;
  3  use Rule6;
  4  use Parse::Eyapp::Treeregexp;
  5
  6  Parse::Eyapp::Treeregexp->new( STRING => q{
  7    fold: /times|plus|div|minus/i:bin(NUM($n), NUM($m))
  8    zero_times_whatever: TIMES(NUM($x), .) and { $x->{attr} == 0 }
  9    whatever_times_zero: TIMES(., NUM($x)) and { $x->{attr} == 0 }
 10  })->generate();
 11
 12  # Syntax analysis
 13  my $parser = new Rule6();
 14  print "Expression: "; $parser->YYData->{INPUT} = <>;
 15  my $t = $parser->Run;
 16  local $Parse::Eyapp::Node::INDENT = 1;
 17  print "Tree:",$t->str,"\n";
 18
 19  # Search
 20  my $m = $t->m(our ($fold, $zero_times_whatever, $whatever_times_zero));
 21  print "Match Node:",$m->str,"\n";

When executed with input C<0*0*0> the program generates this output:

 nereida:~/src/perl/YappWithDefaultAction/examples> m2.pl
 Expression: 0*0*0
 Tree:
 TIMES(
   TIMES(
     NUM(
       TERMINAL
     ),
     NUM(
       TERMINAL
     )
   ),
   NUM(
     TERMINAL
   )
 )
 Match Node:
 Match[TIMES:0:whatever_times_zero](
   Match[TIMES:1:fold,zero_times_whatever,whatever_times_zero]
 )

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
The grammar C<Rule6> used by the example is similar
to the one in the Synopsis example.

 nereida:~/src/perl/YappWithDefaultAction/examples> cat -n numchildren.pl
  1  #!/usr/bin/perl -w
  2  use strict;
  3  use Rule6;
  4  use Parse::Eyapp::Treeregexp;
  5  use Parse::Eyapp::Node;
  6
  7  sub TERMINAL::info { $_[0]{attr} }
  8
  9  my $severity = shift || 0;
 10  my $parser = new Rule6();
 11  $parser->YYData->{INPUT} = shift || '0*2';
 12  my $t = $parser->Run;
 13
 14  my $transform = Parse::Eyapp::Treeregexp->new(
 15    STRING => q{
 16      zero_times_whatever: TIMES(NUM($x)) and { $x->{attr} == 0 } => { $_[0] = $NUM }
 17    },
 18    SEVERITY => $severity,
 19    FIRSTLINE => 15,
 20  )->generate;
 21
 22  $t->s(our @all);
 23
 24  print $t->str,"\n";


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

 nereida:~/src/perl/YappWithDefaultAction/examples> numchildren.pl 0 '0*2'
 NUM(TERMINAL[0])
 nereida:~/src/perl/YappWithDefaultAction/examples> numchildren.pl 1 '0*2'
 TIMES(NUM(TERMINAL[0]),NUM(TERMINAL[2]))
 nereida:~/src/perl/YappWithDefaultAction/examples> numchildren.pl 2 '0*2'
 Warning! found node TIMES with 2 children.
 Expected 1 children (see line 16 of numchildren.pl)"
 TIMES(NUM(TERMINAL[0]),NUM(TERMINAL[2]))
 nereida:~/src/perl/YappWithDefaultAction/examples> numchildren.pl 3 '0*2'
 Error! found node TIMES with 2 children.
 Expected 1 children (see line 16 of numchildren.pl)"
  at (eval 2) line 29

=head2 Array Treeregexp Expressions

The Treeregexp language permits expressions like:

                   A(@a,B($x),@c)

After the matching variable C<@A> contains the shortest prefix
of C<$A-E<gt>children> that does not match C<B($x)>.
The variable C<@c> contains the remaining sufix of
 C<$A-E<gt>children>. 

The following example uses 
array treereg expressions to move an assignment out
of loop (to be correct, we have to guarantee that
the assignment is an invariant of the loop). See
lines 98-111: 

 nereida:~/src/perl/YappWithDefaultAction/examples> \
       cat -n moveinvariantoutofloopcomplexformula.pl
   1  #!/usr/bin/perl -w
   2  use strict;
   3  use Parse::Eyapp;
   4  use Parse::Eyapp::Treeregexp;
   5
   6  my $grammar = q{
  ..      ............ # as usual, but including WHILE loops
  80  }; # end grammar
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
 113  $moveinvariant->s($t);
 114  my @output2 = split /\n/, $t->str;
 115
 116  my ($node1, $node2);
 117  format STDOUT_TOP =
 118                          PROGRAM
 119  -------------------------------------------------------
 120  @||||||||||||||||||||||||||||||||||||||||||||||||||||||
 121  $program
 122  -------------------------------------------------------
 123  Before                     |    After
 124  ---------------------------|---------------------------
 125  .
 126
 127  format STDOUT =
 128  @<<<<<<<<<<<<<<<<<<<<<<<<<<@|@<<<<<<<<<<<<<<<<<<<<<<<<<
 129  $node1,                    '|',$node2
 130  .
 131
 132  for (1..$#output) {
 133    $node1 = $output[$_];
 134    $node2 = $output2[$_];
 135    write;
 136  }

The call to the method C<delete> at line 106 deletes
the C<ASSIGN> child of the second C<BLOCK>.
The copy saved in C<$assign> is inserted
as a child of the first block before
the loop.  Here is the output:

 nereida:~/src/perl/YappWithDefaultAction/examples> \
       moveinvariantoutofloopcomplexformula.pl | cat -n
  1                          PROGRAM
  2  -------------------------------------------------------
  3   a =1000; c = 1; while (a) { c = c*a; b = 5; a = a-1 }
  4  -------------------------------------------------------
  5  Before                     |    After
  6  ---------------------------|---------------------------
  7  BLOCK(                     | BLOCK(
  8    ASSIGN(                  |   ASSIGN(
  9      TERMINAL[a],           |     TERMINAL[a],
 10      NUM(                   |     NUM(
 11        TERMINAL[1000]       |       TERMINAL[1000]
 12      )                      |     )
 13    ) # ASSIGN,              |   ) # ASSIGN,
 14    ASSIGN(                  |   ASSIGN(
 15      TERMINAL[c],           |     TERMINAL[c],
 16      NUM(                   |     NUM(
 17        TERMINAL[1]          |       TERMINAL[1]
 18      )                      |     )
 19    ) # ASSIGN,              |   ) # ASSIGN,
 20    WHILE(                   |   ASSIGN(
 21      VAR(                   |     TERMINAL[b],
 22        TERMINAL[a]          |     NUM(
 23      ),                     |       TERMINAL[5]
 24      BLOCK(                 |     )
 25        ASSIGN(              |   ) # ASSIGN,
 26          TERMINAL[c],       |   WHILE(
 27          TIMES(             |     VAR(
 28            VAR(             |       TERMINAL[a]
 29              TERMINAL[c]    |     ),
 30            ),               |     BLOCK(
 31            VAR(             |       ASSIGN(
 32              TERMINAL[a]    |         TERMINAL[c],
 33            )                |         TIMES(
 34          ) # TIMES          |           VAR(
 35        ) # ASSIGN,          |             TERMINAL[c]
 36        ASSIGN(              |           ),
 37          TERMINAL[b],       |           VAR(
 38          NUM(               |             TERMINAL[a]
 39            TERMINAL[5]      |           )
 40          )                  |         ) # TIMES
 41        ) # ASSIGN,          |       ) # ASSIGN,
 42        ASSIGN(              |       ASSIGN(
 43          TERMINAL[a],       |         TERMINAL[a],
 44          MINUS(             |         MINUS(
 45            VAR(             |           VAR(
 46              TERMINAL[a]    |             TERMINAL[a]
 47            ),               |           ),
 48            NUM(             |           NUM(
 49              TERMINAL[1]    |             TERMINAL[1]
 50            )                |           )
 51          ) # MINUS          |         ) # MINUS
 52        ) # ASSIGN           |       ) # ASSIGN
 53      ) # BLOCK              |     ) # BLOCK
 54    ) # WHILE                |   ) # WHILE
 55  ) # BLOCK                  | ) # BLOCK


=head1 Translation Schemes

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

=head2 Execution Stages of a Translation Scheme

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

This combination of bottom-up parsing with depth first traversin
leads to a semantic behavior similar to LL and top-down parsers
but with several differences:

=over

=item * The grammar can be left-recursive

=item * At the time of executing the action the syntax tree is already built, therefore we can refer
to nodes on the right side of the action like in:

                      D : $T { $L->{t} = $T->{t} } $L

=back

=head2 The C<%begin> directive

The C<%begin { code }> directive  can be used when
building a translation scheme, i.e. when under the 
control of the C<%metatree> directive.
It indicates that such C<code> will be executed at tree
construction time. Therefore the code receives as arguments
the references to the nodes of the branch than is being built.
Usually the code assist in the construction of the tree.
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
  39          |   '(' $exp ')'  %begin { $exp }
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

=head1 Scope Analysis with C<Parse::Eyapp::Scope>

C<Parse::Eyapp> provides support for I<Scope Analysis>
through the module C<Parse::Eyapp::Scope>.
I<Scope Analysis> solves the problem of I<matching>
each instance or use of an object in the source text with
the definition that applies to such instance. Since it is
a I<matching> problem it can sometimes easily solved 
using C<m> as
it was explained in section L</Matching Trees>.

The following pieces of code show how to implement scope
analysis for a C-like language using C<Parse::Eyapp::Scope>

 nereida:~/doc/casiano/PLBOOK/PLBOOK/code/Simple-Types/lib/Simple> \
                           sed -n -e '131,149p' Types.eyp | cat -n
  1  sub reset_file_scope_vars {
  2    %st = (); # reset symbol table
  3    ($tokenbegin, $tokenend) = (1, 1);
  4    %type = ( INT  => Parse::Eyapp::Node->hnew('INT'),  # like new but
  5              CHAR => Parse::Eyapp::Node->hnew('CHAR'), # creates a DAG
  6              VOID => Parse::Eyapp::Node->hnew('VOID'), 
  7            );
  8    $depth = 0;
  9    $ids = Parse::Eyapp::Scope->new(
 10             SCOPE_NAME => 'block',
 11             ENTRY_NAME => 'info',
 12             SCOPE_DEPTH => 'depth',
 13    );
 14    $loops = Parse::Eyapp::Scope->new(
 15             SCOPE_NAME => 'exits',
 16    );
 17    $ids->begin_scope();
 18    $loops->begin_scope(); 
 19  }


Of course you have to include a directive

                 use Parse::Eyapp::Scope

in your client program.

The calls to C<Parse::Eyapp::Scope-E<gt>new> method (lines 9-13 and 14-16 in
the code above) create two I<Scope Manager> objects. One 
scope manager to solve the
scope problem for variables (C<$ids>) and another to solve
the scope problem for loops (C<$loops>). The scope problem for
loops consists in matching each instance of a C<BREAK> or C<CONTINUE>
with the enclosing loop. The beginning of a
scope is set by calling to the C<begin_scope> method
(lines 17 and 18). The end of a scope is signalled by a call to the
method C<end_scope>. Of course, sub 
C<reset_file_scope_vars> must be executed at the proper time:

 nereida:~/doc/casiano/PLBOOK/PLBOOK/code/Simple-Types/lib/Simple> \
                             sed -n -e '170,203p' Types.eyp | cat -n
  1  program: /* program -> definition +  */
  2        {
  3          reset_file_scope_vars();
  4        }
  5      definition<%name PROGRAM +>.program
  6        {
  7          $program->{symboltable} = { %st };  # creates a copy of the s.t.
  8          $program->{depth} = 0;
  9          $program->{line}  = 1;
 10          $program->{types} = { %type };
 11          $program->{lines} = $tokenend;
 12
 13          my ($nondec, $declared) = $ids->end_scope($program->{symboltable}, $program, 'type');
 14
 15          # Type checking: add a direct pointer to the data-structure
 16          # describing the type
 17          $_->{t} = $type{$_->{type}} for @$declared;
 18
 19          if (@$nondec) {
 20            warn "Identifier ".$_->key." not declared at line ".$_->line."\n" for @$nondec;
 21            die "\n";
 22          }
 23
 24          my $out_of_loops = $loops->end_scope($program);
 25          if (@$out_of_loops) {
 26            warn "Error: ".ref($_)." outside of loop at line $_->{line}\n" for @$out_of_loops;
 27            die "\n";
 28          }
 29
 30          # Check that are not dangling breaks
 31          reset_file_scope_vars();
 32
 33          $program;
 34        }

Observe the different ways of calling C<end_scope> (lines 13 and 24).
When a hash table is provided as first argument the declared symbols
will be automatically inserted in it. In such case the classes of the
nodes being inserted must have a C<key> method that computes the key
for such node.

 nereida:~/doc/casiano/PLBOOK/PLBOOK/code/Simple-Types/lib/Simple> \
                            sed -n -e '651,657p' Types.eyp | cat -n
   1  sub VAR::key {
   2    my $self = shift;
   3
   4    return $self->child(0)->{attr}[0];
   5  }
   6
   7  *VARARRAY::key = *FUNCTIONCALL::key = \&VAR::key;

Each instance of an I<scoped object> must be declared as belonging
to the current scope using the C<scope_instance> method. The following
is an example for the C<$loops> scope manager object:

 nereida:~/doc/casiano/PLBOOK/PLBOOK/code/Simple-Types/lib/Simple> \
                            sed -n -e '335,346p' Types.eyp | cat -n
     1  statement:
     2      expression ';' { $_[1] }
     3    | ';'
     4    | %name BREAK
     5      $BREAK ';'
     6        {
     7          my $self = shift;
     8          my $node = $self->YYBuildAST(@_);
     9          $node->{line} = $BREAK->[1];
    10          $loops->scope_instance($node);
    11          return $node;
    12        }

and the following illustrates the same for the C<$ids> scope manager:

 nereida:~/doc/casiano/PLBOOK/PLBOOK/code/Simple-Types/lib/Simple> \
                             sed -n -e '410,425p' Types.eyp | cat -n
     1  Primary:
     2      %name INUM
     3      INUM
     4    | %name CHARCONSTANT
     5      CHARCONSTANT
     6    | $Variable
     7        {
     8          $ids->scope_instance($Variable);
     9          return $Variable
    10        }
    11    | '(' expression ')' { $_[2] }
    12    | $function_call
    13        {
    14          $ids->scope_instance($function_call);
    15          return $function_call  # bypass
    16        }

Of course, in each place where a new scope begins/ends the corresponding
calls to C<begin_scope> and C<end_scope> must be issued. See the following
code:

 nereida:~/doc/casiano/PLBOOK/PLBOOK/code/Simple-Types/lib/Simple> \
                             sed -n -e '277,302p' Types.eyp | cat -n
   1  block:  /* Production is: block -> '{' declaration * statement * '}' */
   2      '{'.bracket
   3         { $ids->begin_scope(); }
   4       declaration<%name DECLARATIONS *>.decs statement<%name STATEMENTS *>.sts '}'
   5         {
   6           my %st;
   7
   8           for my $lst ($decs->children) {
   9
  10               # control duplicated declarations
  11             my $message;
  12             die $message if $message = is_duplicated(\%st, $lst);
  13
  14             %st = (%st, %$lst);
  15           }
  16           $sts->{symboltable} = \%st;
  17           $sts->{line} = $bracket->[1];
  18           $sts->type("BLOCK") if (%st);
  19           my ($nondec, $dec) = $ids->end_scope(\%st, $sts, 'type');
  20
  21           # Type checking: add a direct pointer to the data-structure
  22           # describing the type
  23           $_->{t} = $type{$_->{type}} for @$dec;
  24
  25           return $sts;
  26         }


=head1 SEE ALSO

=over

=item * perldoc L<Parse::Eyapp>

=item * The C<Eyapp.pdf> and  C<eyapptut.pdf> files accompanying this distribution

=item * perldoc L<eyapp>, 

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
The classic book "Compilers: Principles, Techniques, and Tools" by Alfred V. Aho, Ravi Sethi and
Jeffrey D. Ullman (Addison-Wesley 1986)

=back

=head1 AUTHOR
 
Casiano Rodriguez-Leon (casiano@ull.es)

A large percentage of  code is verbatim taken from Parse::Yapp 1.05.
The author of Parse::Yapp is Francois Desarmenien.
 
=head1 ACKNOWLEDGMENTS

This work has been supported by CEE (FEDER) and the Spanish Ministry of
I<Educaci�n y Ciencia> through Plan Nacional I+D+I number TIN2005-08818-C04-04
(ULL::OPLINK project L<http://www.oplink.ull.es/>). 
Support from Gobierno de Canarias was through GC02210601
(Grupos Consolidados).
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

