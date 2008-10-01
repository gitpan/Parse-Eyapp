###################################################################################
#
#    This file was generated using Parse::Eyapp version 1.113.
#
# (c) Parse::Yapp Copyright 1998-2001 Francois Desarmenien.
# (c) Parse::Eyapp Copyright 2006-2008 Casiano Rodriguez-Leon. Universidad de La Laguna.
#        Don't edit this file, use source file "Rule9.yp" instead.
#
#             ANY CHANGE MADE HERE WILL BE LOST !
#
###################################################################################
package Calc;
use strict;

push @Calc::ISA, 'Parse::Eyapp::Driver';


BEGIN {
  # This strange way to load the modules is to guarantee compatibility when
  # using several standalone and non-standalone Eyapp parsers

  require Parse::Eyapp::Driver unless Parse::Eyapp::Driver->can('YYParse');
  require Parse::Eyapp::Node unless Parse::Eyapp::Node->can('hnew'); 
}
    

#line 1 "Rule9.yp"

use Data::Dumper;

#line 31 Calc.pm

my $warnmessage =<< "EOFWARN";
Warning!: Did you changed the \@Calc::ISA variable inside the header section of the eyapp program?
EOFWARN

sub new {
        my($class)=shift;
        ref($class)
    and $class=ref($class);

    warn $warnmessage unless __PACKAGE__->isa('Parse::Eyapp::Driver'); 
    my($self)=$class->SUPER::new( yyversion => '1.113',
                                  yyGRAMMAR  =>
[
  [ _SUPERSTART => '$start', [ 'line', '$end' ], 0 ],
  [ line_1 => 'line', [ 'exp' ], 0 ],
  [ NUM => 'exp', [ 'NUM' ], 0 ],
  [ VAR => 'exp', [ 'VAR' ], 0 ],
  [ ASSIGN => 'exp', [ 'VAR', '=', 'exp' ], 0 ],
  [ PLUS => 'exp', [ 'exp', '+', 'exp' ], 0 ],
  [ MINUS => 'exp', [ 'exp', '-', 'exp' ], 0 ],
  [ TIMES => 'exp', [ 'exp', '*', 'exp' ], 0 ],
  [ DIV => 'exp', [ 'exp', '/', 'exp' ], 0 ],
  [ UMINUS => 'exp', [ '-', 'exp' ], 0 ],
  [ exp_10 => 'exp', [ '(', 'exp', ')' ], 0 ],
],
                                  yyTERMS  =>
{ '$end' => 0, '(' => 0, ')' => 0, '*' => 1, '+' => 1, '-' => 1, '/' => 1, '=' => 1, NEG => 1, NUM => 1, VAR => 1 },
                                  yyFILENAME  => "Rule9.yp",
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
#line 227 Calc.pm
	],
	[#Rule line_1
		 'line', 1,
sub {
#line 14 "Rule9.yp"
 $_[1] }
#line 234 Calc.pm
	],
	[#Rule NUM
		 'exp', 1,
sub {
#line 11 "Rule9.yp"
 goto &Parse::Eyapp::Driver::YYBuildAST }
#line 241 Calc.pm
	],
	[#Rule VAR
		 'exp', 1,
sub {
#line 11 "Rule9.yp"
 goto &Parse::Eyapp::Driver::YYBuildAST }
#line 248 Calc.pm
	],
	[#Rule ASSIGN
		 'exp', 3,
sub {
#line 11 "Rule9.yp"
 goto &Parse::Eyapp::Driver::YYBuildAST }
#line 255 Calc.pm
	],
	[#Rule PLUS
		 'exp', 3,
sub {
#line 11 "Rule9.yp"
 goto &Parse::Eyapp::Driver::YYBuildAST }
#line 262 Calc.pm
	],
	[#Rule MINUS
		 'exp', 3,
sub {
#line 11 "Rule9.yp"
 goto &Parse::Eyapp::Driver::YYBuildAST }
#line 269 Calc.pm
	],
	[#Rule TIMES
		 'exp', 3,
sub {
#line 11 "Rule9.yp"
 goto &Parse::Eyapp::Driver::YYBuildAST }
#line 276 Calc.pm
	],
	[#Rule DIV
		 'exp', 3,
sub {
#line 11 "Rule9.yp"
 goto &Parse::Eyapp::Driver::YYBuildAST }
#line 283 Calc.pm
	],
	[#Rule UMINUS
		 'exp', 2,
sub {
#line 11 "Rule9.yp"
 goto &Parse::Eyapp::Driver::YYBuildAST }
#line 290 Calc.pm
	],
	[#Rule exp_10
		 'exp', 3,
sub {
#line 33 "Rule9.yp"
 $_[2] }
#line 297 Calc.pm
	]
],
#line 300 Calc.pm
                                  yybypass => 0,
                                  yybuildingtree => 1,
                                  yyprefix => '',
                                  @_,);
    bless($self,$class);

    $self->make_node_classes( qw{TERMINAL _OPTIONAL _STAR_LIST _PLUS_LIST 
         _SUPERSTART
         line_1
         NUM
         VAR
         ASSIGN
         PLUS
         MINUS
         TIMES
         DIV
         UMINUS
         exp_10} );
    $self;
}

#line 36 "Rule9.yp"


sub Error {
        exists $_[0]->YYData->{ERRMSG}
    and do {
        print $_[0]->YYData->{ERRMSG};
        delete $_[0]->YYData->{ERRMSG};
        return;
    };
    print "Syntax error.\n";
}

sub Lexer {
    my($parser)=shift;

        $parser->YYData->{INPUT}
    or  $parser->YYData->{INPUT} = <STDIN>
    or  return('',undef);

    $parser->YYData->{INPUT}=~s/^\s+//;

    for ($parser->YYData->{INPUT}) {
        s/^([0-9]+(?:\.[0-9]+)?)//
                and return('NUM',$1);
        s/^([A-Za-z][A-Za-z0-9_]*)//
                and return('VAR',$1);
        s/^(.)//s
                and return($1,$1);
    }
}

sub Run {
    my($self)=shift;
    $self->YYParse( yylex => \&_Lexer, yyerror => \&_Error, 
		    #yydebug =>0xFF
		  );
}

#line 361 Calc.pm

1;
