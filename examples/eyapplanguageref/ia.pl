#!/usr/bin/perl
########################################################################################
#
#    This file was generated using Parse::Eyapp version 1.155.
#
# (c) Parse::Yapp Copyright 1998-2001 Francois Desarmenien.
# (c) Parse::Eyapp Copyright 2006-2008 Casiano Rodriguez-Leon. Universidad de La Laguna.
#        Don't edit this file, use source file 'intermediateaction2.yp' instead.
#
#             ANY CHANGE MADE HERE WILL BE LOST !
#
########################################################################################
package intermediateaction2;
use strict;

push @intermediateaction2::ISA, 'Parse::Eyapp::Driver';


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

      m{\G(a)}gc and return ($1, $1);



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


#line 52 ia.pl

my $warnmessage =<< "EOFWARN";
Warning!: Did you changed the \@intermediateaction2::ISA variable inside the header section of the eyapp program?
EOFWARN

sub new {
  my($class)=shift;
  ref($class) and $class=ref($class);

  warn $warnmessage unless __PACKAGE__->isa('Parse::Eyapp::Driver'); 
  my($self)=$class->SUPER::new( 
    yyversion => '1.155',
    yyGRAMMAR  =>
[
  [ '_SUPERSTART' => '$start', [ 'S', '$end' ], 0 ],
  [ 'S_1' => 'S', [ 'a', '@1-1', 'a' ], 0 ],
  [ '_CODE' => '@1-1', [  ], 0 ],
],
    yyTERMS  =>
{ '' => { ISSEMANTIC => 0 },
	'a' => { ISSEMANTIC => 0 },
	error => { ISSEMANTIC => 0 },
},
    yyFILENAME  => 'intermediateaction2.yp',
    yystates =>
[
	{#State 0
		ACTIONS => {
			"a" => 2
		},
		GOTOS => {
			'S' => 1
		}
	},
	{#State 1
		ACTIONS => {
			'' => 3
		}
	},
	{#State 2
		DEFAULT => -2,
		GOTOS => {
			'@1-1' => 4
		}
	},
	{#State 3
		DEFAULT => 0
	},
	{#State 4
		ACTIONS => {
			"a" => 5
		}
	},
	{#State 5
		DEFAULT => -1
	}
],
    yyrules  =>
[
	[#Rule _SUPERSTART
		 '$start', 2, undef
#line 114 ia.pl
	],
	[#Rule S_1
		 'S', 3,
sub {
#line 2 "intermediateaction2.yp"
my $mid = $_[2];  print "\n<<$_[2], $mid, $_[3]>>\n"; }
#line 121 ia.pl
	],
	[#Rule _CODE
		 '@1-1', 0,
sub {
#line 2 "intermediateaction2.yp"
my $mid = $_[2];  $_[1]x4 }
#line 128 ia.pl
	]
],
#line 131 ia.pl
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
         'S_1', 
         '_CODE', );
  $self;
}

#line 4 "intermediateaction2.yp"



=for None

=cut


#line 159 ia.pl

unless (caller) {
  exit !__PACKAGE__->main('');
}


1;
