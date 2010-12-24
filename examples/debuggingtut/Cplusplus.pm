#!/usr/bin/perl
########################################################################################
#
#    This file was generated using Parse::Eyapp version 1.174.
#
# (c) Parse::Yapp Copyright 1998-2001 Francois Desarmenien.
# (c) Parse::Eyapp Copyright 2006-2008 Casiano Rodriguez-Leon. Universidad de La Laguna.
#        Don't edit this file, use source file 'Cplusplus.eyp' instead.
#
#             ANY CHANGE MADE HERE WILL BE LOST !
#
########################################################################################
package Cplusplus;
use strict;

push @Cplusplus::ISA, 'Parse::Eyapp::Driver';




BEGIN {
  # This strange way to load the modules is to guarantee compatibility when
  # using several standalone and non-standalone Eyapp parsers

  require Parse::Eyapp::Driver unless Parse::Eyapp::Driver->can('YYParse');
  require Parse::Eyapp::Node unless Parse::Eyapp::Node->can('hnew'); 
}
  

sub unexpendedInput { defined($_) ? substr($_, (defined(pos $_) ? pos $_ : 0)) : '' }

#line 1 "Cplusplus.eyp"

our $VERSION = '0.01';

my $WHITES = qr{\G\s*};
my $INT    = qr{\Gint\b};
my $ID     = qr{\G([a-zA-Z_][a-zA-Z_0-9]*)};
my $NUM    = qr{\G(\d+)};
my $PUN    = qr{\G([-+*/=();,])};

# If the incoming input looks like this then it is a declaration
my $ISDEC  = qr{\G(?=
                      [)\s]*  # closing parenthesis, s.t. like: ') ) )'
                      [;=]\   # followed by semicolon or '='
                  )
               }x;
#line 19 "Cplusplus.eyp"
__PACKAGE__->YYLexer( 
  sub { # lexical analyzer
    my $self = $_[0]; 
    for (${$self->input()}) {  # contextualize
#line 19 "Cplusplus.eyp"
      
    m{$WHITES}gc;

    return ('INT', $1) if m{$INT}gc;
    return ('ID',  $1) if m{$ID}gc;
    return ('NUM', $1) if m{$NUM}gc;
    return ($1,    $1) if m{$PUN}gc;
         
#line 61 ./Cplusplus.pm
      return ('', undef) if ($_ eq '') || (defined(pos($_)) && (pos($_) >= length($_)));
      die("Error inside the lexical analyzer. Line: 26. File: Cplusplus.eyp. No regexp matched.\n");
    } 
  } # end lexical analyzer
);




#line 71 ./Cplusplus.pm

my $warnmessage =<< "EOFWARN";
Warning!: Did you changed the \@Cplusplus::ISA variable inside the header section of the eyapp program?
EOFWARN

sub new {
  my($class)=shift;
  ref($class) and $class=ref($class);

  warn $warnmessage unless __PACKAGE__->isa('Parse::Eyapp::Driver'); 
  my($self)=$class->SUPER::new( 
    yyversion => '1.174',
    yyGRAMMAR  =>
[#[productionNameAndLabel => lhs, [ rhs], bypass]]
  [ '_SUPERSTART' => '$start', [ 'prog', '$end' ], 1 ],
  [ 'EMPTY' => 'prog', [  ], 1 ],
  [ 'PROG' => 'prog', [ 'prog', 'stmt' ], 1 ],
  [ 'EXP' => 'stmt', [ 'expr', ';' ], 1 ],
  [ 'DECL' => 'stmt', [ 'decl' ], 1 ],
  [ 'ID:EXP' => 'expr', [ 'ID', 'decORexp' ], 1 ],
  [ 'NUM' => 'expr', [ 'NUM' ], 1 ],
  [ 'TYPECAST' => 'expr', [ 'INT', '(', 'expr', ')' ], 1 ],
  [ 'PLUS' => 'expr', [ 'expr', '+', 'expr' ], 1 ],
  [ 'ASSIGN' => 'expr', [ 'expr', '=', 'expr' ], 1 ],
  [ 'DECLARATOR' => 'decl', [ 'INT', 'declarator', ';' ], 1 ],
  [ 'DECLARATORINIT' => 'decl', [ 'INT', 'declarator', '=', 'expr', ';' ], 1 ],
  [ 'ID:DEC' => 'declarator', [ 'ID', 'decORexp' ], 1 ],
  [ 'declarator_13' => 'declarator', [ '(', 'declarator', ')' ], 1 ],
  [ 'decORexp' => 'decORexp', [  ], 1 ],
],
    yyLABELS  =>
{
  '_SUPERSTART' => 0,
  'EMPTY' => 1,
  'PROG' => 2,
  'EXP' => 3,
  'DECL' => 4,
  ':EXP' => 5,
  'ID:EXP' => 5,
  'NUM' => 6,
  'TYPECAST' => 7,
  'PLUS' => 8,
  'ASSIGN' => 9,
  'DECLARATOR' => 10,
  'DECLARATORINIT' => 11,
  ':DEC' => 12,
  'ID:DEC' => 12,
  'declarator_13' => 13,
  'decORexp' => 14,
},
    yyTERMS  =>
{ '' => { ISSEMANTIC => 0 },
	'(' => { ISSEMANTIC => 0 },
	')' => { ISSEMANTIC => 0 },
	'+' => { ISSEMANTIC => 0 },
	';' => { ISSEMANTIC => 0 },
	'=' => { ISSEMANTIC => 0 },
	ID => { ISSEMANTIC => 1 },
	INT => { ISSEMANTIC => 1 },
	NUM => { ISSEMANTIC => 1 },
	error => { ISSEMANTIC => 0 },
},
    yyFILENAME  => 'Cplusplus.eyp',
    yystates =>
[
	{#State 0
		DEFAULT => -1,
		GOTOS => {
			'prog' => 1
		}
	},
	{#State 1
		ACTIONS => {
			'' => 5,
			'ID' => 3,
			'NUM' => 2,
			'INT' => 8
		},
		GOTOS => {
			'stmt' => 4,
			'expr' => 7,
			'decl' => 6
		}
	},
	{#State 2
		DEFAULT => -6
	},
	{#State 3
		DEFAULT => -14,
		GOTOS => {
			'decORexp' => 9
		}
	},
	{#State 4
		DEFAULT => -2
	},
	{#State 5
		DEFAULT => 0
	},
	{#State 6
		DEFAULT => -4
	},
	{#State 7
		ACTIONS => {
			";" => 11,
			"+" => 10,
			"=" => 12
		}
	},
	{#State 8
		ACTIONS => {
			'ID' => 13,
			"(" => 14
		},
		GOTOS => {
			'declarator' => 15
		}
	},
	{#State 9
		DEFAULT => -5
	},
	{#State 10
		ACTIONS => {
			'ID' => 3,
			'NUM' => 2,
			'INT' => 17
		},
		GOTOS => {
			'expr' => 16
		}
	},
	{#State 11
		DEFAULT => -3
	},
	{#State 12
		ACTIONS => {
			'ID' => 3,
			'NUM' => 2,
			'INT' => 17
		},
		GOTOS => {
			'expr' => 18
		}
	},
	{#State 13
		DEFAULT => -14,
		GOTOS => {
			'decORexp' => 19
		}
	},
	{#State 14
		ACTIONS => {
			'ID' => 20,
			'NUM' => 2,
			"(" => 21,
			'INT' => 17
		},
		GOTOS => {
			'expr' => 22,
			'declarator' => 23
		}
	},
	{#State 15
		ACTIONS => {
			";" => 24,
			"=" => 25
		}
	},
	{#State 16
		DEFAULT => -8
	},
	{#State 17
		ACTIONS => {
			"(" => 26
		}
	},
	{#State 18
		ACTIONS => {
			"+" => 10,
			"=" => 12
		},
		DEFAULT => -9
	},
	{#State 19
		DEFAULT => -12
	},
	{#State 20
		DEFAULT => -14,
		GOTOS => {
			'decORexp' => 27
		}
	},
	{#State 21
		ACTIONS => {
			'ID' => 13,
			"(" => 21
		},
		GOTOS => {
			'declarator' => 23
		}
	},
	{#State 22
		ACTIONS => {
			"+" => 10,
			")" => 28,
			"=" => 12
		}
	},
	{#State 23
		ACTIONS => {
			")" => 29
		}
	},
	{#State 24
		DEFAULT => -10
	},
	{#State 25
		ACTIONS => {
			'ID' => 3,
			'NUM' => 2,
			'INT' => 17
		},
		GOTOS => {
			'expr' => 30
		}
	},
	{#State 26
		ACTIONS => {
			'ID' => 3,
			'NUM' => 2,
			'INT' => 17
		},
		GOTOS => {
			'expr' => 22
		}
	},
	{#State 27
		DEFAULT => -5
	},
	{#State 28
		DEFAULT => -7
	},
	{#State 29
		DEFAULT => -13
	},
	{#State 30
		ACTIONS => {
			";" => 31,
			"+" => 10,
			"=" => 12
		}
	},
	{#State 31
		DEFAULT => -11
	}
],
    yyrules  =>
[
	[#Rule _SUPERSTART
		 '$start', 2, undef
#line 332 ./Cplusplus.pm
	],
	[#Rule EMPTY
		 'prog', 0,
sub {
#line 42 "Cplusplus.eyp"
 goto &Parse::Eyapp::Driver::YYBuildAST }
#line 339 ./Cplusplus.pm
	],
	[#Rule PROG
		 'prog', 2,
sub {
#line 42 "Cplusplus.eyp"
 goto &Parse::Eyapp::Driver::YYBuildAST }
#line 346 ./Cplusplus.pm
	],
	[#Rule EXP
		 'stmt', 2,
sub {
#line 42 "Cplusplus.eyp"
 goto &Parse::Eyapp::Driver::YYBuildAST }
#line 353 ./Cplusplus.pm
	],
	[#Rule DECL
		 'stmt', 1,
sub {
#line 42 "Cplusplus.eyp"
 goto &Parse::Eyapp::Driver::YYBuildAST }
#line 360 ./Cplusplus.pm
	],
	[#Rule ID:EXP
		 'expr', 2,
sub {
#line 42 "Cplusplus.eyp"
 goto &Parse::Eyapp::Driver::YYBuildAST }
#line 367 ./Cplusplus.pm
	],
	[#Rule NUM
		 'expr', 1,
sub {
#line 42 "Cplusplus.eyp"
 goto &Parse::Eyapp::Driver::YYBuildAST }
#line 374 ./Cplusplus.pm
	],
	[#Rule TYPECAST
		 'expr', 4,
sub {
#line 42 "Cplusplus.eyp"
 goto &Parse::Eyapp::Driver::YYBuildAST }
#line 381 ./Cplusplus.pm
	],
	[#Rule PLUS
		 'expr', 3,
sub {
#line 42 "Cplusplus.eyp"
 goto &Parse::Eyapp::Driver::YYBuildAST }
#line 388 ./Cplusplus.pm
	],
	[#Rule ASSIGN
		 'expr', 3,
sub {
#line 42 "Cplusplus.eyp"
 goto &Parse::Eyapp::Driver::YYBuildAST }
#line 395 ./Cplusplus.pm
	],
	[#Rule DECLARATOR
		 'decl', 3,
sub {
#line 42 "Cplusplus.eyp"
 goto &Parse::Eyapp::Driver::YYBuildAST }
#line 402 ./Cplusplus.pm
	],
	[#Rule DECLARATORINIT
		 'decl', 5,
sub {
#line 42 "Cplusplus.eyp"
 goto &Parse::Eyapp::Driver::YYBuildAST }
#line 409 ./Cplusplus.pm
	],
	[#Rule ID:DEC
		 'declarator', 2,
sub {
#line 42 "Cplusplus.eyp"
 goto &Parse::Eyapp::Driver::YYBuildAST }
#line 416 ./Cplusplus.pm
	],
	[#Rule declarator_13
		 'declarator', 3,
sub {
#line 42 "Cplusplus.eyp"
 goto &Parse::Eyapp::Driver::YYBuildAST }
#line 423 ./Cplusplus.pm
	],
	[#Rule decORexp
		 'decORexp', 0,
sub {
#line 85 "Cplusplus.eyp"
  my $self = $_[0];
  for (${$self->input()}) {  
#line 31 "Cplusplus.eyp" 

        if (m{$ISDEC}) {
          $self->YYSetReduce(')', 'ID:DEC' );
        }
        else {
          $self->YYSetReduce(')', 'ID:EXP' ); 
        }
      
#line 440 ./Cplusplus.pm
  }
}
#line 443 ./Cplusplus.pm
	]
],
#line 446 ./Cplusplus.pm
    yybypass       => 1,
    yybuildingtree => 1,
    yyprefix       => '',
    yyaccessors    => {
   },
    yyconflicthandlers => {
          'decORexp' => {
                          'production' => {
                                            '-5' => [
                                                      1
                                                    ],
                                            '-12' => [
                                                       1
                                                     ]
                                          },
                          'states' => [
                                        {
                                          '27' => [
                                                    '\')\''
                                                  ]
                                        }
                                      ],
                          'line' => 31
                        }
        }
,
    @_,
  );
  bless($self,$class);

  $self->make_node_classes('TERMINAL', '_OPTIONAL', '_STAR_LIST', '_PLUS_LIST', 
         '_SUPERSTART', 
         'EMPTY', 
         'PROG', 
         'EXP', 
         'DECL', 
         'ID:EXP', 
         'NUM', 
         'TYPECAST', 
         'PLUS', 
         'ASSIGN', 
         'DECLARATOR', 
         'DECLARATORINIT', 
         'ID:DEC', 
         'declarator_13', 
         'decORexp', );
  $self;
}

#line 85 "Cplusplus.eyp"


unless (caller()) {
  my $prompt = 'Try first "int (x) = 2;" then "int (x) + 2;" '.
               '(press <CR><CTRL-D> to finish): ';
  __PACKAGE__->main($prompt) 
}

####################################################

=head1 SYNOPSIS

Compile it with
 
   eyapp -b '' Cplusplus

Run it with:

   ./Cplusplus.pm -t -nos -i

or 

   ./Cplusplus.pm -t -i -c 'int (x) + 2;'

try with inputs:

     int (x) = 2;
     int (x) + 2;

the output will be a description of the generated abstract syntax tree

=head1  C++ Ambiguities

This grammar models a problematic part of the C++ grammar
the ambiguity between certain
declarations and statements. For example,

     int (x) = y+z;

parses as either an expr or a stmt.


Eyapp detects this as a reduce/reduce conflict:

  State 17 contains 1 reduce/reduce conflict

  State 17:

	expr -> ID .	(Rule 5)
	declarator -> ID .	(Rule 11)

	')'	[reduce using rule 11 (declarator)]
	$default	reduce using rule 5 (expr)

The C++ disambiguation rule is: 
take it as a declaration if it looks as a declaration,
otherwise is an expression.

This Eyapp parser solves the problem by dynamically changing the parser.

=head1 SEE ALSO

=over 2

=item * The file C<Cplusplus2.eyp> in C<examples/debuggintut>

=item * L<http://www.gnu.org/software/bison/manual/html_mono/bison.html#GLR-Parsers>

=item * L<http://en.wikipedia.org/wiki/Significantly_Prettier_and_Easier_C%2B%2B_Syntax>

=item * L<http://www.csse.monash.edu.au/~damian/papers/PS/ModestProposal.ps> 

=item * L<http://www.nobugs.org/developer/parsingcpp/>

=item * Edward Willink's "Meta-Compilation for C++" PhD thesis at L<http://www.computing.surrey.ac.uk/Research/CSRG/fog/FogThesis.pdf>

=back

=cut


#line 578 ./Cplusplus.pm



1;
