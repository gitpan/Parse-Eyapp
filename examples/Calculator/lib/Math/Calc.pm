########################################################################################
#
#    This file was generated using Parse::Eyapp version 1.122.
#
# (c) Parse::Yapp Copyright 1998-2001 Francois Desarmenien.
# (c) Parse::Eyapp Copyright 2006-2008 Casiano Rodriguez-Leon. Universidad de La Laguna.
#        Don't edit this file, use source file "lib/Math/Calc.eyp" instead.
#
#             ANY CHANGE MADE HERE WILL BE LOST !
#
########################################################################################
package Math::Calc;
use strict;

push @Math::Calc::ISA, 'Parse::Eyapp::Driver';


BEGIN {
  # This strange way to load the modules is to guarantee compatibility when
  # using several standalone and non-standalone Eyapp parsers

  require Parse::Eyapp::Driver unless Parse::Eyapp::Driver->can('YYParse');
  require Parse::Eyapp::Node unless Parse::Eyapp::Node->can('hnew'); 
}
  

#line 7 "lib/Math/Calc.eyp"

use Math::Tail;
my %s; # symbol table

#line 32 lib/Math/Calc.pm

my $warnmessage =<< "EOFWARN";
Warning!: Did you changed the \@Math::Calc::ISA variable inside the header section of the eyapp program?
EOFWARN

sub new {
  my($class)=shift;
  ref($class) and $class=ref($class);

  warn $warnmessage unless __PACKAGE__->isa('Parse::Eyapp::Driver'); 
  my($self)=$class->SUPER::new( 
    yyversion => '1.122',
    yyGRAMMAR  =>
[
  [ _SUPERSTART => '$start', [ 'start', '$end' ], 0 ],
  [ start_1 => 'start', [ 'input' ], 0 ],
  [ _STAR_LIST => 'STAR-1', [ 'STAR-1', 'line' ], 0 ],
  [ _STAR_LIST => 'STAR-1', [  ], 0 ],
  [ input_4 => 'input', [ 'STAR-1' ], 0 ],
  [ line_5 => 'line', [ '\n' ], 0 ],
  [ line_6 => 'line', [ 'exp', '\n' ], 0 ],
  [ exp_7 => 'exp', [ 'NUM' ], 0 ],
  [ exp_8 => 'exp', [ 'VAR' ], 0 ],
  [ exp_9 => 'exp', [ 'VAR', '=', 'exp' ], 0 ],
  [ exp_10 => 'exp', [ 'exp', '+', 'exp' ], 0 ],
  [ exp_11 => 'exp', [ 'exp', '-', 'exp' ], 0 ],
  [ exp_12 => 'exp', [ 'exp', '*', 'exp' ], 0 ],
  [ exp_13 => 'exp', [ 'exp', '/', 'exp' ], 0 ],
  [ exp_14 => 'exp', [ '-', 'exp' ], 0 ],
  [ exp_15 => 'exp', [ 'exp', '^', 'exp' ], 0 ],
  [ exp_16 => 'exp', [ '(', 'exp', ')' ], 0 ],
],
    yyTERMS  =>
{ '$end' => 0, '(' => 0, ')' => 0, '*' => 0, '+' => 0, '-' => 0, '/' => 0, '=' => 0, '\n' => 0, '^' => 0, NEG => 1, NUM => 1, VAR => 1 },
    yyFILENAME  => "lib/Math/Calc.eyp",
    yystates =>
[
	{#State 0
		DEFAULT => -3,
		GOTOS => {
			'input' => 2,
			'STAR-1' => 1,
			'start' => 3
		}
	},
	{#State 1
		ACTIONS => {
			'NUM' => 5,
			"-" => 4,
			"(" => 7,
			"\n" => 9,
			'VAR' => 8
		},
		DEFAULT => -4,
		GOTOS => {
			'exp' => 6,
			'line' => 10
		}
	},
	{#State 2
		DEFAULT => -1
	},
	{#State 3
		ACTIONS => {
			'' => 11
		}
	},
	{#State 4
		ACTIONS => {
			'NUM' => 5,
			"-" => 4,
			"(" => 7,
			'VAR' => 8
		},
		GOTOS => {
			'exp' => 12
		}
	},
	{#State 5
		DEFAULT => -7
	},
	{#State 6
		ACTIONS => {
			"-" => 13,
			"^" => 14,
			"*" => 15,
			"\n" => 17,
			"+" => 16,
			"/" => 18
		}
	},
	{#State 7
		ACTIONS => {
			'NUM' => 5,
			"-" => 4,
			"(" => 7,
			'VAR' => 8
		},
		GOTOS => {
			'exp' => 19
		}
	},
	{#State 8
		ACTIONS => {
			"=" => 20
		},
		DEFAULT => -8
	},
	{#State 9
		DEFAULT => -5
	},
	{#State 10
		DEFAULT => -2
	},
	{#State 11
		DEFAULT => 0
	},
	{#State 12
		ACTIONS => {
			"^" => 14
		},
		DEFAULT => -14
	},
	{#State 13
		ACTIONS => {
			'NUM' => 5,
			"-" => 4,
			"(" => 7,
			'VAR' => 8
		},
		GOTOS => {
			'exp' => 21
		}
	},
	{#State 14
		ACTIONS => {
			'NUM' => 5,
			"-" => 4,
			"(" => 7,
			'VAR' => 8
		},
		GOTOS => {
			'exp' => 22
		}
	},
	{#State 15
		ACTIONS => {
			'NUM' => 5,
			"-" => 4,
			"(" => 7,
			'VAR' => 8
		},
		GOTOS => {
			'exp' => 23
		}
	},
	{#State 16
		ACTIONS => {
			'NUM' => 5,
			"-" => 4,
			"(" => 7,
			'VAR' => 8
		},
		GOTOS => {
			'exp' => 24
		}
	},
	{#State 17
		DEFAULT => -6
	},
	{#State 18
		ACTIONS => {
			'NUM' => 5,
			"-" => 4,
			"(" => 7,
			'VAR' => 8
		},
		GOTOS => {
			'exp' => 25
		}
	},
	{#State 19
		ACTIONS => {
			"-" => 13,
			"^" => 14,
			"*" => 15,
			"+" => 16,
			"/" => 18,
			")" => 26
		}
	},
	{#State 20
		ACTIONS => {
			'NUM' => 5,
			"-" => 4,
			"(" => 7,
			'VAR' => 8
		},
		GOTOS => {
			'exp' => 27
		}
	},
	{#State 21
		ACTIONS => {
			"^" => 14,
			"*" => 15,
			"/" => 18
		},
		DEFAULT => -11
	},
	{#State 22
		ACTIONS => {
			"^" => 14
		},
		DEFAULT => -15
	},
	{#State 23
		ACTIONS => {
			"^" => 14
		},
		DEFAULT => -12
	},
	{#State 24
		ACTIONS => {
			"^" => 14,
			"*" => 15,
			"/" => 18
		},
		DEFAULT => -10
	},
	{#State 25
		ACTIONS => {
			"^" => 14
		},
		DEFAULT => -13
	},
	{#State 26
		DEFAULT => -16
	},
	{#State 27
		ACTIONS => {
			"-" => 13,
			"^" => 14,
			"*" => 15,
			"+" => 16,
			"/" => 18
		},
		DEFAULT => -9
	}
],
    yyrules  =>
[
	[#Rule _SUPERSTART
		 '$start', 2, undef
#line 287 lib/Math/Calc.pm
	],
	[#Rule start_1
		 'start', 1,
sub {
#line 14 "lib/Math/Calc.eyp"
 \%s }
#line 294 lib/Math/Calc.pm
	],
	[#Rule _STAR_LIST
		 'STAR-1', 2,
sub {
#line 17 "lib/Math/Calc.eyp"
 goto &Parse::Eyapp::Driver::YYActionforT_TX1X2 }
#line 301 lib/Math/Calc.pm
	],
	[#Rule _STAR_LIST
		 'STAR-1', 0,
sub {
#line 17 "lib/Math/Calc.eyp"
 goto &Parse::Eyapp::Driver::YYActionforT_empty }
#line 308 lib/Math/Calc.pm
	],
	[#Rule input_4
		 'input', 1, undef
#line 312 lib/Math/Calc.pm
	],
	[#Rule line_5
		 'line', 1,
sub {
#line 21 "lib/Math/Calc.eyp"
 undef }
#line 319 lib/Math/Calc.pm
	],
	[#Rule line_6
		 'line', 2,
sub {
#line 22 "lib/Math/Calc.eyp"
 print "$_[1]\n" if defined($_[1]); $_[1] }
#line 326 lib/Math/Calc.pm
	],
	[#Rule exp_7
		 'exp', 1,
sub {
#line 26 "lib/Math/Calc.eyp"
my $NUM = $_[1];  $NUM->[0]     }
#line 333 lib/Math/Calc.pm
	],
	[#Rule exp_8
		 'exp', 1,
sub {
#line 27 "lib/Math/Calc.eyp"
my $VAR = $_[1];  $s{$VAR->[0]} }
#line 340 lib/Math/Calc.pm
	],
	[#Rule exp_9
		 'exp', 3,
sub {
#line 28 "lib/Math/Calc.eyp"
my $exp = $_[3]; my $VAR = $_[1];  $s{$VAR->[0]} = $exp }
#line 347 lib/Math/Calc.pm
	],
	[#Rule exp_10
		 'exp', 3,
sub {
#line 29 "lib/Math/Calc.eyp"
my $y = $_[3]; my $x = $_[1];  $x + $y }
#line 354 lib/Math/Calc.pm
	],
	[#Rule exp_11
		 'exp', 3,
sub {
#line 30 "lib/Math/Calc.eyp"
my $y = $_[3]; my $x = $_[1];  $x - $y }
#line 361 lib/Math/Calc.pm
	],
	[#Rule exp_12
		 'exp', 3,
sub {
#line 31 "lib/Math/Calc.eyp"
my $y = $_[3]; my $x = $_[1];  $x * $y }
#line 368 lib/Math/Calc.pm
	],
	[#Rule exp_13
		 'exp', 3,
sub {
#line 33 "lib/Math/Calc.eyp"
my $y = $_[3]; my $x = $_[1]; 
       $y and return($x/$y);
       $_[0]->YYData->{ERRMSG} = "Illegal division by zero at line $_[2][1].\n";
       $_[0]->YYError; 
       undef
    }
#line 380 lib/Math/Calc.pm
	],
	[#Rule exp_14
		 'exp', 2,
sub {
#line 39 "lib/Math/Calc.eyp"
my $exp = $_[2];  -$exp }
#line 387 lib/Math/Calc.pm
	],
	[#Rule exp_15
		 'exp', 3,
sub {
#line 40 "lib/Math/Calc.eyp"
my $y = $_[3]; my $x = $_[1];  $x ** $y }
#line 394 lib/Math/Calc.pm
	],
	[#Rule exp_16
		 'exp', 3,
sub {
#line 41 "lib/Math/Calc.eyp"
my $exp = $_[2];  $exp }
#line 401 lib/Math/Calc.pm
	]
],
#line 404 lib/Math/Calc.pm
    yybypass       => 0,
    yybuildingtree => 0,
    yyprefix       => '',
    yyaccessors    => {
   },
    @_,
  );
  bless($self,$class);

  $self->make_node_classes( qw{TERMINAL _OPTIONAL _STAR_LIST _PLUS_LIST 
         _SUPERSTART
         start_1
         input_4
         line_5
         line_6
         exp_7
         exp_8
         exp_9
         exp_10
         exp_11
         exp_12
         exp_13
         exp_14
         exp_15
         exp_16} );
  $self;
}

#line 44 "lib/Math/Calc.eyp"




=for None

=cut



#line 444 lib/Math/Calc.pm

1;
