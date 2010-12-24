########################################################################################
#
#    This file was generated using Parse::Eyapp version 1.175.
#
# (c) Parse::Yapp Copyright 1998-2001 Francois Desarmenien.
# (c) Parse::Eyapp Copyright 2006-2008 Casiano Rodriguez-Leon. Universidad de La Laguna.
#        Don't edit this file, use source file 'Decl.eyp' instead.
#
#             ANY CHANGE MADE HERE WILL BE LOST !
#
########################################################################################
package Decl;
use strict;

push @Decl::ISA, 'Parse::Eyapp::Driver';




BEGIN {
  # This strange way to load the modules is to guarantee compatibility when
  # using several standalone and non-standalone Eyapp parsers

  require Parse::Eyapp::Driver unless Parse::Eyapp::Driver->can('YYParse');
  require Parse::Eyapp::Node unless Parse::Eyapp::Node->can('hnew'); 
}
  

sub unexpendedInput { defined($_) ? substr($_, (defined(pos $_) ? pos $_ : 0)) : '' }

#line 1 "Decl.eyp"

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


#line 68 ./Decl.pm

my $warnmessage =<< "EOFWARN";
Warning!: Did you changed the \@Decl::ISA variable inside the header section of the eyapp program?
EOFWARN

sub new {
  my($class)=shift;
  ref($class) and $class=ref($class);

  warn $warnmessage unless __PACKAGE__->isa('Parse::Eyapp::Driver'); 
  my($self)=$class->SUPER::new( 
    yyversion => '1.175',
    yyGRAMMAR  =>
[#[productionNameAndLabel => lhs, [ rhs], bypass]]
  [ '_SUPERSTART' => '$start', [ 'decl', '$end' ], 0 ],
  [ 'DECLARATOR' => 'decl', [ 'INT', 'declarator', ';' ], 0 ],
  [ 'DECLARATORINIT' => 'decl', [ 'INT', 'declarator', '=', 'expr', ';' ], 0 ],
  [ 'ID:EXP' => 'expr', [ 'ID' ], 0 ],
  [ 'NUM' => 'expr', [ 'NUM' ], 0 ],
  [ 'TYPECAST' => 'expr', [ 'INT', '(', 'expr', ')' ], 0 ],
  [ 'PLUS' => 'expr', [ 'expr', '+', 'expr' ], 0 ],
  [ 'ASSIGN' => 'expr', [ 'expr', '=', 'expr' ], 0 ],
  [ 'ID:DEC' => 'declarator', [ 'ID' ], 0 ],
  [ 'declarator_9' => 'declarator', [ '(', 'declarator', ')' ], 0 ],
],
    yyLABELS  =>
{
  '_SUPERSTART' => 0,
  'DECLARATOR' => 1,
  'DECLARATORINIT' => 2,
  ':EXP' => 3,
  'ID:EXP' => 3,
  'NUM' => 4,
  'TYPECAST' => 5,
  'PLUS' => 6,
  'ASSIGN' => 7,
  ':DEC' => 8,
  'ID:DEC' => 8,
  'declarator_9' => 9,
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
    yyFILENAME  => 'Decl.eyp',
    yystates =>
[
	{#State 0
		ACTIONS => {
			'INT' => 2
		},
		GOTOS => {
			'decl' => 1
		}
	},
	{#State 1
		DEFAULT => 3
	},
	{#State 2
		ACTIONS => {
			'ID' => 4,
			"(" => 5
		},
		GOTOS => {
			'declarator' => 6
		}
	},
	{#State 3
		DEFAULT => 0
	},
	{#State 4
		DEFAULT => -8
	},
	{#State 5
		ACTIONS => {
			'ID' => 4,
			"(" => 5
		},
		GOTOS => {
			'declarator' => 7
		}
	},
	{#State 6
		ACTIONS => {
			";" => 8,
			"=" => 9
		}
	},
	{#State 7
		ACTIONS => {
			")" => 10
		}
	},
	{#State 8
		DEFAULT => -1
	},
	{#State 9
		ACTIONS => {
			'ID' => 12,
			'NUM' => 11,
			'INT' => 14
		},
		GOTOS => {
			'expr' => 13
		}
	},
	{#State 10
		DEFAULT => -9
	},
	{#State 11
		DEFAULT => -4
	},
	{#State 12
		DEFAULT => -3
	},
	{#State 13
		ACTIONS => {
			";" => 16,
			"+" => 15,
			"=" => 17
		}
	},
	{#State 14
		ACTIONS => {
			"(" => 18
		}
	},
	{#State 15
		ACTIONS => {
			'ID' => 12,
			'NUM' => 11,
			'INT' => 14
		},
		GOTOS => {
			'expr' => 19
		}
	},
	{#State 16
		DEFAULT => -2
	},
	{#State 17
		ACTIONS => {
			'ID' => 12,
			'NUM' => 11,
			'INT' => 14
		},
		GOTOS => {
			'expr' => 20
		}
	},
	{#State 18
		ACTIONS => {
			'ID' => 12,
			'NUM' => 11,
			'INT' => 14
		},
		GOTOS => {
			'expr' => 21
		}
	},
	{#State 19
		DEFAULT => -6
	},
	{#State 20
		ACTIONS => {
			"+" => 15,
			"=" => 17
		},
		DEFAULT => -7
	},
	{#State 21
		ACTIONS => {
			"+" => 15,
			")" => 22,
			"=" => 17
		}
	},
	{#State 22
		DEFAULT => -5
	}
],
    yyrules  =>
[
	[#Rule _SUPERSTART
		 '$start', 2, undef
#line 262 ./Decl.pm
	],
	[#Rule DECLARATOR
		 'decl', 3, undef
#line 266 ./Decl.pm
	],
	[#Rule DECLARATORINIT
		 'decl', 5, undef
#line 270 ./Decl.pm
	],
	[#Rule ID:EXP
		 'expr', 1, undef
#line 274 ./Decl.pm
	],
	[#Rule NUM
		 'expr', 1, undef
#line 278 ./Decl.pm
	],
	[#Rule TYPECAST
		 'expr', 4, undef
#line 282 ./Decl.pm
	],
	[#Rule PLUS
		 'expr', 3, undef
#line 286 ./Decl.pm
	],
	[#Rule ASSIGN
		 'expr', 3, undef
#line 290 ./Decl.pm
	],
	[#Rule ID:DEC
		 'declarator', 1, undef
#line 294 ./Decl.pm
	],
	[#Rule declarator_9
		 'declarator', 3, undef
#line 298 ./Decl.pm
	]
],
#line 301 ./Decl.pm
    yybypass       => 0,
    yybuildingtree => 0,
    yyprefix       => '',
    yyaccessors    => {
   },
    yyconflicthandlers => {}
,
    @_,
  );
  bless($self,$class);

  $self->make_node_classes('TERMINAL', '_OPTIONAL', '_STAR_LIST', '_PLUS_LIST', 
         '_SUPERSTART', 
         'DECLARATOR', 
         'DECLARATORINIT', 
         'ID:EXP', 
         'NUM', 
         'TYPECAST', 
         'PLUS', 
         'ASSIGN', 
         'ID:DEC', 
         'declarator_9', );
  $self;
}

#line 43 "Decl.eyp"




=for None

=cut


#line 337 ./Decl.pm



1;
