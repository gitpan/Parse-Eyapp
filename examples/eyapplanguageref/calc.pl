#!/usr/bin/perl
########################################################################################
#
#    This file was generated using Parse::Eyapp version 1.155.
#
# (c) Parse::Yapp Copyright 1998-2001 Francois Desarmenien.
# (c) Parse::Eyapp Copyright 2006-2008 Casiano Rodriguez-Leon. Universidad de La Laguna.
#        Don't edit this file, use source file 'Postfix.eyp' instead.
#
#             ANY CHANGE MADE HERE WILL BE LOST !
#
########################################################################################
package Postfix;
use strict;

push @Postfix::ISA, 'Parse::Eyapp::Driver';


BEGIN {
  # This strange way to load the modules is to guarantee compatibility when
  # using several standalone and non-standalone Eyapp parsers

  require Parse::Eyapp::Driver unless Parse::Eyapp::Driver->can('YYParse');
  require Parse::Eyapp::Node unless Parse::Eyapp::Node->can('hnew'); 
}
  

# Default lexical analyzer
our $LEX = sub {
    my $self = shift;

    for (${$self->input}) {
      m{\G(\s+)}gc and $self->tokenline($1 =~ tr{\n}{});;

      m{\G(\-|\+|\/|\=|\(|\*|\))}gc and return ($1, $1);

      /\G([0-9]+(?:\.[0-9]+)?)/gc and return ('NUM', $1);
      /\G([A-Za-z][A-Za-z0-9_]*)/gc and return ('VAR', $1);


      return ('', undef) if ($_ eq '') || (defined(pos($_)) && (pos($_) >= length($_)));
      /\G\s*(\S+)/;
      my $near = substr($1,0,10); 
      die( "Error inside the lexical analyzer near '". $near
          ."'. Line: ".$self->line()
          .". File: '".$self->YYFilename()."'. No match found.\n");
    }
  }
;


sub unexpendedInput { substr($_, pos $_) }


#line 54 calc.pl

my $warnmessage =<< "EOFWARN";
Warning!: Did you changed the \@Postfix::ISA variable inside the header section of the eyapp program?
EOFWARN

sub new {
  my($class)=shift;
  ref($class) and $class=ref($class);

  warn $warnmessage unless __PACKAGE__->isa('Parse::Eyapp::Driver'); 
  my($self)=$class->SUPER::new( 
    yyversion => '1.155',
    yyGRAMMAR  =>
[
  [ '_SUPERSTART' => '$start', [ 'line', '$end' ], 0 ],
  [ 'line_1' => 'line', [ 'exp' ], 0 ],
  [ 'exp_2' => 'exp', [ 'NUM' ], 0 ],
  [ 'exp_3' => 'exp', [ 'VAR' ], 0 ],
  [ 'exp_4' => 'exp', [ 'VAR', '=', 'exp' ], 0 ],
  [ 'exp_5' => 'exp', [ 'exp', '+', 'exp' ], 0 ],
  [ 'exp_6' => 'exp', [ 'exp', '-', 'exp' ], 0 ],
  [ 'exp_7' => 'exp', [ 'exp', '*', 'exp' ], 0 ],
  [ 'exp_8' => 'exp', [ 'exp', '/', 'exp' ], 0 ],
  [ 'exp_9' => 'exp', [ '-', 'exp' ], 0 ],
  [ 'exp_10' => 'exp', [ '(', 'exp', ')' ], 0 ],
],
    yyTERMS  =>
{ '' => { ISSEMANTIC => 0 },
	'(' => { ISSEMANTIC => 0 },
	')' => { ISSEMANTIC => 0 },
	'*' => { ISSEMANTIC => 0 },
	'+' => { ISSEMANTIC => 0 },
	'-' => { ISSEMANTIC => 0 },
	'/' => { ISSEMANTIC => 0 },
	'=' => { ISSEMANTIC => 0 },
	NEG => { ISSEMANTIC => 1 },
	NUM => { ISSEMANTIC => 1 },
	VAR => { ISSEMANTIC => 1 },
	error => { ISSEMANTIC => 0 },
},
    yyFILENAME  => 'Postfix.eyp',
    yystates =>
[
	{#State 0
		ACTIONS => {
			'NUM' => 2,
			"-" => 1,
			"(" => 4,
			'VAR' => 5
		},
		GOTOS => {
			'exp' => 3,
			'line' => 6
		}
	},
	{#State 1
		ACTIONS => {
			'NUM' => 2,
			"-" => 1,
			"(" => 4,
			'VAR' => 5
		},
		GOTOS => {
			'exp' => 7
		}
	},
	{#State 2
		DEFAULT => -2
	},
	{#State 3
		ACTIONS => {
			"-" => 8,
			"*" => 9,
			"+" => 10,
			"/" => 11
		},
		DEFAULT => -1
	},
	{#State 4
		ACTIONS => {
			'NUM' => 2,
			"-" => 1,
			"(" => 4,
			'VAR' => 5
		},
		GOTOS => {
			'exp' => 12
		}
	},
	{#State 5
		ACTIONS => {
			"=" => 13
		},
		DEFAULT => -3
	},
	{#State 6
		ACTIONS => {
			'' => 14
		}
	},
	{#State 7
		DEFAULT => -9
	},
	{#State 8
		ACTIONS => {
			'NUM' => 2,
			"-" => 1,
			"(" => 4,
			'VAR' => 5
		},
		GOTOS => {
			'exp' => 15
		}
	},
	{#State 9
		ACTIONS => {
			'NUM' => 2,
			"-" => 1,
			"(" => 4,
			'VAR' => 5
		},
		GOTOS => {
			'exp' => 16
		}
	},
	{#State 10
		ACTIONS => {
			'NUM' => 2,
			"-" => 1,
			"(" => 4,
			'VAR' => 5
		},
		GOTOS => {
			'exp' => 17
		}
	},
	{#State 11
		ACTIONS => {
			'NUM' => 2,
			"-" => 1,
			"(" => 4,
			'VAR' => 5
		},
		GOTOS => {
			'exp' => 18
		}
	},
	{#State 12
		ACTIONS => {
			"-" => 8,
			"*" => 9,
			"+" => 10,
			"/" => 11,
			")" => 19
		}
	},
	{#State 13
		ACTIONS => {
			'NUM' => 2,
			"-" => 1,
			"(" => 4,
			'VAR' => 5
		},
		GOTOS => {
			'exp' => 20
		}
	},
	{#State 14
		DEFAULT => 0
	},
	{#State 15
		ACTIONS => {
			"*" => 9,
			"/" => 11
		},
		DEFAULT => -6
	},
	{#State 16
		DEFAULT => -7
	},
	{#State 17
		ACTIONS => {
			"*" => 9,
			"/" => 11
		},
		DEFAULT => -5
	},
	{#State 18
		DEFAULT => -8
	},
	{#State 19
		DEFAULT => -10
	},
	{#State 20
		ACTIONS => {
			"-" => 8,
			"*" => 9,
			"+" => 10,
			"/" => 11
		},
		DEFAULT => -4
	}
],
    yyrules  =>
[
	[#Rule _SUPERSTART
		 '$start', 2, undef
#line 262 calc.pl
	],
	[#Rule line_1
		 'line', 1,
sub {
#line 15 "Postfix.eyp"
my $exp = $_[1];  print "$exp\n" }
#line 269 calc.pl
	],
	[#Rule exp_2
		 'exp', 1,
sub {
#line 18 "Postfix.eyp"
my $NUM = $_[1];  $NUM }
#line 276 calc.pl
	],
	[#Rule exp_3
		 'exp', 1,
sub {
#line 19 "Postfix.eyp"
my $VAR = $_[1];  $VAR }
#line 283 calc.pl
	],
	[#Rule exp_4
		 'exp', 3,
sub {
#line 12 "Postfix.eyp"
my $left = $_[1]; my $right = $_[3]; my $op = $_[2];  "$left $right $op"; }
#line 290 calc.pl
	],
	[#Rule exp_5
		 'exp', 3,
sub {
#line 12 "Postfix.eyp"
my $left = $_[1]; my $right = $_[3]; my $op = $_[2];  "$left $right $op"; }
#line 297 calc.pl
	],
	[#Rule exp_6
		 'exp', 3,
sub {
#line 12 "Postfix.eyp"
my $left = $_[1]; my $right = $_[3]; my $op = $_[2];  "$left $right $op"; }
#line 304 calc.pl
	],
	[#Rule exp_7
		 'exp', 3,
sub {
#line 12 "Postfix.eyp"
my $left = $_[1]; my $right = $_[3]; my $op = $_[2];  "$left $right $op"; }
#line 311 calc.pl
	],
	[#Rule exp_8
		 'exp', 3,
sub {
#line 12 "Postfix.eyp"
my $left = $_[1]; my $right = $_[3]; my $op = $_[2];  "$left $right $op"; }
#line 318 calc.pl
	],
	[#Rule exp_9
		 'exp', 2,
sub {
#line 25 "Postfix.eyp"
my $exp = $_[2];  "$exp NEG" }
#line 325 calc.pl
	],
	[#Rule exp_10
		 'exp', 3,
sub {
#line 26 "Postfix.eyp"
my $exp = $_[2];  $exp }
#line 332 calc.pl
	]
],
#line 335 calc.pl
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
         'line_1', 
         'exp_2', 
         'exp_3', 
         'exp_4', 
         'exp_5', 
         'exp_6', 
         'exp_7', 
         'exp_8', 
         'exp_9', 
         'exp_10', );
  $self;
}

#line 29 "Postfix.eyp"



=for None

=cut


#line 371 calc.pl

unless (caller) {
  exit !__PACKAGE__->main('');
}


1;
