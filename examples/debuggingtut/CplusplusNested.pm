#!/usr/bin/perl
########################################################################################
#
#    This file was generated using Parse::Eyapp version 1.175.
#
# (c) Parse::Yapp Copyright 1998-2001 Francois Desarmenien.
# (c) Parse::Eyapp Copyright 2006-2008 Casiano Rodriguez-Leon. Universidad de La Laguna.
#        Don't edit this file, use source file 'CplusplusNested.eyp' instead.
#
#             ANY CHANGE MADE HERE WILL BE LOST !
#
########################################################################################
package CplusplusNested;
use strict;

push @CplusplusNested::ISA, 'Parse::Eyapp::Driver';




BEGIN {
  # This strange way to load the modules is to guarantee compatibility when
  # using several standalone and non-standalone Eyapp parsers

  require Parse::Eyapp::Driver unless Parse::Eyapp::Driver->can('YYParse');
  require Parse::Eyapp::Node unless Parse::Eyapp::Node->can('hnew'); 
}
  

sub unexpendedInput { defined($_) ? substr($_, (defined(pos $_) ? pos $_ : 0)) : '' }

#line 1 "CplusplusNested.eyp"

my $INT    = '(int)\b';
my $ID     = '([a-zA-Z_][a-zA-Z_0-9]*)';
my $NUM    = '(\d+)';


# Default lexical analyzer
our $LEX = sub {
    my $self = shift;

    for (${$self->input}) {
      

      m{\G(\s+)}gc and $self->tokenline($1 =~ tr{\n}{});

      m{\G(\(|\+|\;|\)|\=)}gc and return ($1, $1);

      /\G$NUM/gc and return ('NUM', $1);
      /\G$INT/gc and return ('INT', $1);
      /\G$ID/gc and return ('ID', $1);


      return ('', undef) if ($_ eq '') || (defined(pos($_)) && (pos($_) >= length($_)));
      /\G\s*(\S+)/;
      my $near = substr($1,0,10); 

      return($near, $near);

     # die( "Error inside the lexical analyzer near '". $near
     #     ."'. Line: ".$self->line()
     #     .". File: '".$self->YYFilename()."'. No match found.\n");
    }
  }
;


#line 68 ./CplusplusNested.pm

my $warnmessage =<< "EOFWARN";
Warning!: Did you changed the \@CplusplusNested::ISA variable inside the header section of the eyapp program?
EOFWARN

sub new {
  my($class)=shift;
  ref($class) and $class=ref($class);

  warn $warnmessage unless __PACKAGE__->isa('Parse::Eyapp::Driver'); 
  my($self)=$class->SUPER::new( 
    yyversion => '1.175',
    yyGRAMMAR  =>
[#[productionNameAndLabel => lhs, [ rhs], bypass]]
  [ '_SUPERSTART' => '$start', [ 'prog', '$end' ], 1 ],
  [ 'EMPTY' => 'prog', [  ], 1 ],
  [ 'PROG' => 'prog', [ 'prog', 'decORexp_explorer', 'stmt' ], 1 ],
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
  [ 'decORexp_explorer' => 'decORexp_explorer', [  ], 1 ],
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
  'decORexp_explorer' => 15,
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
    yyFILENAME  => 'CplusplusNested.eyp',
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
			'' => 2
		},
		DEFAULT => -15,
		GOTOS => {
			'decORexp_explorer' => 3
		}
	},
	{#State 2
		DEFAULT => 0
	},
	{#State 3
		ACTIONS => {
			'ID' => 5,
			'NUM' => 4,
			'INT' => 9
		},
		GOTOS => {
			'stmt' => 6,
			'expr' => 8,
			'decl' => 7
		}
	},
	{#State 4
		DEFAULT => -6
	},
	{#State 5
		DEFAULT => -14,
		GOTOS => {
			'decORexp' => 10
		}
	},
	{#State 6
		DEFAULT => -2
	},
	{#State 7
		DEFAULT => -4
	},
	{#State 8
		ACTIONS => {
			";" => 12,
			"+" => 11,
			"=" => 13
		}
	},
	{#State 9
		ACTIONS => {
			'ID' => 14,
			"(" => 15
		},
		GOTOS => {
			'declarator' => 16
		}
	},
	{#State 10
		DEFAULT => -5
	},
	{#State 11
		ACTIONS => {
			'ID' => 5,
			'NUM' => 4,
			'INT' => 18
		},
		GOTOS => {
			'expr' => 17
		}
	},
	{#State 12
		DEFAULT => -3
	},
	{#State 13
		ACTIONS => {
			'ID' => 5,
			'NUM' => 4,
			'INT' => 18
		},
		GOTOS => {
			'expr' => 19
		}
	},
	{#State 14
		DEFAULT => -14,
		GOTOS => {
			'decORexp' => 20
		}
	},
	{#State 15
		ACTIONS => {
			'ID' => 21,
			'NUM' => 4,
			"(" => 22,
			'INT' => 18
		},
		GOTOS => {
			'expr' => 23,
			'declarator' => 24
		}
	},
	{#State 16
		ACTIONS => {
			";" => 25,
			"=" => 26
		}
	},
	{#State 17
		DEFAULT => -8
	},
	{#State 18
		ACTIONS => {
			"(" => 27
		}
	},
	{#State 19
		ACTIONS => {
			"+" => 11,
			"=" => 13
		},
		DEFAULT => -9
	},
	{#State 20
		DEFAULT => -12
	},
	{#State 21
		DEFAULT => -14,
		GOTOS => {
			'decORexp' => 28
		}
	},
	{#State 22
		ACTIONS => {
			'ID' => 14,
			"(" => 22
		},
		GOTOS => {
			'declarator' => 24
		}
	},
	{#State 23
		ACTIONS => {
			"+" => 11,
			")" => 29,
			"=" => 13
		}
	},
	{#State 24
		ACTIONS => {
			")" => 30
		}
	},
	{#State 25
		DEFAULT => -10
	},
	{#State 26
		ACTIONS => {
			'ID' => 5,
			'NUM' => 4,
			'INT' => 18
		},
		GOTOS => {
			'expr' => 31
		}
	},
	{#State 27
		ACTIONS => {
			'ID' => 5,
			'NUM' => 4,
			'INT' => 18
		},
		GOTOS => {
			'expr' => 23
		}
	},
	{#State 28
		DEFAULT => -5
	},
	{#State 29
		DEFAULT => -7
	},
	{#State 30
		DEFAULT => -13
	},
	{#State 31
		ACTIONS => {
			";" => 32,
			"+" => 11,
			"=" => 13
		}
	},
	{#State 32
		DEFAULT => -11
	}
],
    yyrules  =>
[
	[#Rule _SUPERSTART
		 '$start', 2, undef
#line 339 ./CplusplusNested.pm
	],
	[#Rule EMPTY
		 'prog', 0,
sub {
#line 23 "CplusplusNested.eyp"
 goto &Parse::Eyapp::Driver::YYBuildAST }
#line 346 ./CplusplusNested.pm
	],
	[#Rule PROG
		 'prog', 3,
sub {
#line 23 "CplusplusNested.eyp"
 goto &Parse::Eyapp::Driver::YYBuildAST }
#line 353 ./CplusplusNested.pm
	],
	[#Rule EXP
		 'stmt', 2,
sub {
#line 23 "CplusplusNested.eyp"
 goto &Parse::Eyapp::Driver::YYBuildAST }
#line 360 ./CplusplusNested.pm
	],
	[#Rule DECL
		 'stmt', 1,
sub {
#line 23 "CplusplusNested.eyp"
 goto &Parse::Eyapp::Driver::YYBuildAST }
#line 367 ./CplusplusNested.pm
	],
	[#Rule ID:EXP
		 'expr', 2,
sub {
#line 23 "CplusplusNested.eyp"
 goto &Parse::Eyapp::Driver::YYBuildAST }
#line 374 ./CplusplusNested.pm
	],
	[#Rule NUM
		 'expr', 1,
sub {
#line 23 "CplusplusNested.eyp"
 goto &Parse::Eyapp::Driver::YYBuildAST }
#line 381 ./CplusplusNested.pm
	],
	[#Rule TYPECAST
		 'expr', 4,
sub {
#line 23 "CplusplusNested.eyp"
 goto &Parse::Eyapp::Driver::YYBuildAST }
#line 388 ./CplusplusNested.pm
	],
	[#Rule PLUS
		 'expr', 3,
sub {
#line 23 "CplusplusNested.eyp"
 goto &Parse::Eyapp::Driver::YYBuildAST }
#line 395 ./CplusplusNested.pm
	],
	[#Rule ASSIGN
		 'expr', 3,
sub {
#line 23 "CplusplusNested.eyp"
 goto &Parse::Eyapp::Driver::YYBuildAST }
#line 402 ./CplusplusNested.pm
	],
	[#Rule DECLARATOR
		 'decl', 3,
sub {
#line 23 "CplusplusNested.eyp"
 goto &Parse::Eyapp::Driver::YYBuildAST }
#line 409 ./CplusplusNested.pm
	],
	[#Rule DECLARATORINIT
		 'decl', 5,
sub {
#line 23 "CplusplusNested.eyp"
 goto &Parse::Eyapp::Driver::YYBuildAST }
#line 416 ./CplusplusNested.pm
	],
	[#Rule ID:DEC
		 'declarator', 2,
sub {
#line 23 "CplusplusNested.eyp"
 goto &Parse::Eyapp::Driver::YYBuildAST }
#line 423 ./CplusplusNested.pm
	],
	[#Rule declarator_13
		 'declarator', 3,
sub {
#line 23 "CplusplusNested.eyp"
 goto &Parse::Eyapp::Driver::YYBuildAST }
#line 430 ./CplusplusNested.pm
	],
	[#Rule decORexp
		 'decORexp', 0,
sub {
#line 66 "CplusplusNested.eyp"
  my $self = $_[0];
  for (${$self->input()}) {  
#line 14 "CplusplusNested.eyp" 

  if ($self->YYIs('decl')) { $self->YYSetReduce('ID:DEC' ); }
  else { $self->YYSetReduce('ID:EXP' ); }
#line 442 ./CplusplusNested.pm
  }

}
#line 446 ./CplusplusNested.pm
	],
	[#Rule decORexp_explorer
		 'decORexp_explorer', 0,
sub {
#line 66 "CplusplusNested.eyp"
  my $self = $_[0];
  for (${$self->input()}) {  
#line 19 "CplusplusNested.eyp" 
{ $self->YYNestedParse('decl'); }
#line 456 ./CplusplusNested.pm
  }

}
#line 460 ./CplusplusNested.pm
	]
],
#line 463 ./CplusplusNested.pm
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
                          'explorerline' => 19,
                          'states' => [
                                        {
                                          '28' => [
                                                    ')'
                                                  ]
                                        }
                                      ],
                          'line' => 14
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
         'decORexp', 
         'decORexp_explorer', );
  $self;
}

#line 66 "CplusplusNested.eyp"


####################################################

=head1 SYNOPSIS

Compile it with
 
   eyapp -C CplusplusNested

the compile it again, using 'decl' as start symbol:

   eyapp -S decl -P CplusplusNested.eyp

Run it with:

   ./CplusplusNested.pm -t -nos -i

or 

   ./CplusplusNested.pm -t -i -c 'int (x) + 2; int (z) = 4;'

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


#line 595 ./CplusplusNested.pm

unless (caller) {
  exit !__PACKAGE__->main('');
}


1;
