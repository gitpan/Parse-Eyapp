#!/usr/bin/perl
########################################################################################
#
#    This file was generated using Parse::Eyapp version 1.155.
#
# (c) Parse::Yapp Copyright 1998-2001 Francois Desarmenien.
# (c) Parse::Eyapp Copyright 2006-2008 Casiano Rodriguez-Leon. Universidad de La Laguna.
#        Don't edit this file, use source file 'dynamicgrammar.eyp' instead.
#
#             ANY CHANGE MADE HERE WILL BE LOST !
#
########################################################################################
package dynamicgrammar;
use strict;

push @dynamicgrammar::ISA, 'Parse::Eyapp::Driver';


BEGIN {
  # This strange way to load the modules is to guarantee compatibility when
  # using several standalone and non-standalone Eyapp parsers

  require Parse::Eyapp::Driver unless Parse::Eyapp::Driver->can('YYParse');
  require Parse::Eyapp::Node unless Parse::Eyapp::Node->can('hnew'); 
}
  

#line 1 "dynamicgrammar.eyp"

=head1 SYNOPSIS

This example shows how the dynamic conlfict resolution
technique makes possible to change the behavior of the parser
at the programmer's command.

Compile it with:

    eyapp -C dynamicgrammar.eyp 

Run with:

    $ ./dynamicgrammar.pm -f input_for_dynamicgrammar.txt 

The file C<input_for_dynamicgrammar.txt> contains:

            2-1-1 # left: 0
            RIGHT
            2-1-1 # right: 2
            LEFT
            3-1-1 # left: 1
            RIGHT
            3-1-1 # right: 3

=cut

my $reduce = 1;
#line 31 "dynamicgrammar.eyp"
__PACKAGE__->YYLexer( 
  sub { # lexical analyzer
    my $self = $_[0]; 
    for (${$self->input()}) {  # contextualize
#line 31 "dynamicgrammar.eyp"
      
    m{\G(\s*)(?:#.*)?(\s*)}gc and $self->tokenline("$1$2" =~ tr{\n}{});
    m{\G(LEFT|RIGHT)\b}gic         and return (uc($1), uc($1));
    m{\G([0-9]+)}gc                and return ('NUM', $1);
    m{\G(.)}gc                     and return ($1,    $1);
       
#line 68 dyng.pl
      return ('', undef) if ($_ eq '') || (defined(pos($_)) && (pos($_) >= length($_)));
      die("Error at the end of lexical analyzer. Line: 36. File: dynamicgrammar.eyp. No regexp matched.\n");
    } 
  } # end lexical analyzer
);

#line 75 dyng.pl

my $warnmessage =<< "EOFWARN";
Warning!: Did you changed the \@dynamicgrammar::ISA variable inside the header section of the eyapp program?
EOFWARN

sub new {
  my($class)=shift;
  ref($class) and $class=ref($class);

  warn $warnmessage unless __PACKAGE__->isa('Parse::Eyapp::Driver'); 
  my($self)=$class->SUPER::new( 
    yyversion => '1.155',
    yyGRAMMAR  =>
[
  [ '_SUPERSTART' => '$start', [ 'p', '$end' ], 0 ],
  [ 'p_1' => 'p', [  ], 0 ],
  [ 'p_2' => 'p', [ 'p', 'c' ], 0 ],
  [ 'c_3' => 'c', [ 'expr' ], 0 ],
  [ 'c_4' => 'c', [ 'RIGHT' ], 0 ],
  [ 'c_5' => 'c', [ 'LEFT' ], 0 ],
  [ 'expr_6' => 'expr', [ '(', 'expr', ')' ], 0 ],
  [ 'expr_7:MINUS' => 'expr', [ 'expr', 'leftORright', '-', 'expr', 'leftORright' ], 0 ],
  [ 'expr_8' => 'expr', [ 'NUM' ], 0 ],
  [ 'leftORright' => 'leftORright', [  ], 0 ],
],
    yyTERMS  =>
{ '' => { ISSEMANTIC => 0 },
	'(' => { ISSEMANTIC => 0 },
	')' => { ISSEMANTIC => 0 },
	'-' => { ISSEMANTIC => 0 },
	LEFT => { ISSEMANTIC => 1 },
	NUM => { ISSEMANTIC => 1 },
	RIGHT => { ISSEMANTIC => 1 },
	error => { ISSEMANTIC => 0 },
},
    yyFILENAME  => 'dynamicgrammar.eyp',
    yystates =>
[
	{#State 0
		DEFAULT => -1,
		GOTOS => {
			'p' => 1
		}
	},
	{#State 1
		ACTIONS => {
			'' => 3,
			'NUM' => 2,
			"(" => 4,
			'LEFT' => 7,
			'RIGHT' => 8
		},
		GOTOS => {
			'c' => 5,
			'expr' => 6
		}
	},
	{#State 2
		DEFAULT => -8
	},
	{#State 3
		DEFAULT => 0
	},
	{#State 4
		ACTIONS => {
			'NUM' => 2,
			"(" => 4
		},
		GOTOS => {
			'expr' => 9
		}
	},
	{#State 5
		DEFAULT => -2
	},
	{#State 6
		ACTIONS => {
			"-" => -9
		},
		DEFAULT => -3,
		GOTOS => {
			'leftORright' => 10
		}
	},
	{#State 7
		DEFAULT => -5
	},
	{#State 8
		DEFAULT => -4
	},
	{#State 9
		ACTIONS => {
			")" => 11
		},
		DEFAULT => -9,
		GOTOS => {
			'leftORright' => 10
		}
	},
	{#State 10
		ACTIONS => {
			"-" => 12
		}
	},
	{#State 11
		DEFAULT => -6
	},
	{#State 12
		ACTIONS => {
			'NUM' => 2,
			"(" => 4
		},
		GOTOS => {
			'expr' => 13
		}
	},
	{#State 13
		DEFAULT => -9,
		GOTOS => {
			'leftORright' => 14
		}
	},
	{#State 14
		ACTIONS => {
			"-" => 12
		},
		DEFAULT => -7
	}
],
    yyrules  =>
[
	[#Rule _SUPERSTART
		 '$start', 2, undef
#line 209 dyng.pl
	],
	[#Rule p_1
		 'p', 0,
sub {
#line 49 "dynamicgrammar.eyp"
}
#line 216 dyng.pl
	],
	[#Rule p_2
		 'p', 2,
sub {
#line 50 "dynamicgrammar.eyp"
}
#line 223 dyng.pl
	],
	[#Rule c_3
		 'c', 1,
sub {
#line 54 "dynamicgrammar.eyp"
my $expr = $_[1];  print "$expr\n" }
#line 230 dyng.pl
	],
	[#Rule c_4
		 'c', 1,
sub {
#line 55 "dynamicgrammar.eyp"
 $reduce = 0}
#line 237 dyng.pl
	],
	[#Rule c_5
		 'c', 1,
sub {
#line 56 "dynamicgrammar.eyp"
 $reduce = 1}
#line 244 dyng.pl
	],
	[#Rule expr_6
		 'expr', 3,
sub {
#line 61 "dynamicgrammar.eyp"
my $expr = $_[2];  $expr }
#line 251 dyng.pl
	],
	[#Rule expr_7:MINUS
		 'expr', 5,
sub {
#line 65 "dynamicgrammar.eyp"
my $left = $_[1]; my $right = $_[4];  $left -$right }
#line 258 dyng.pl
	],
	[#Rule expr_8
		 'expr', 1, undef
#line 262 dyng.pl
	],
	[#Rule leftORright
		 'leftORright', 0,
sub {
#line 70 "dynamicgrammar.eyp"
  my $self = $_[0];
  for (${$self->input()}) {  
#line 38 "dynamicgrammar.eyp" 

  # reduce if $reduce and next token is '-'
  if ($reduce && m{\G(?=-)}gc) 
       { $self->YYSetReduce('-', ':MINUS' ); }
  else { $self->YYSetShift('-'); }
#line 276 dyng.pl
  }

}
#line 280 dyng.pl
	]
],
#line 283 dyng.pl
    yybypass       => 0,
    yybuildingtree => 0,
    yyprefix       => '',
    yyaccessors    => {
   },
    @_,
  );
  bless($self,$class);

  $self->make_node_classes('TERMINAL', '_OPTIONAL', '_STAR_LIST', '_PLUS_LIST', 
         '_SUPERSTART', 
         'p_1', 
         'p_2', 
         'c_3', 
         'c_4', 
         'c_5', 
         'expr_6', 
         'expr_7:MINUS', 
         'expr_8', 
         'leftORright', );
  $self;
}

#line 70 "dynamicgrammar.eyp"



=for None

=cut


#line 316 dyng.pl

unless (caller) {
  exit !__PACKAGE__->main('');
}


1;
