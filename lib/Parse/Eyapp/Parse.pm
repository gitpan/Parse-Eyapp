########################################################################################
#
#    This file was generated using Parse::Eyapp version 1.151.
#
# (c) Parse::Yapp Copyright 1998-2001 Francois Desarmenien.
# (c) Parse::Eyapp Copyright 2006-2008 Casiano Rodriguez-Leon. Universidad de La Laguna.
#        Don't edit this file, use source file 'lib/Parse/Eyapp/Parse.yp' instead.
#
#             ANY CHANGE MADE HERE WILL BE LOST !
#
########################################################################################
package Parse::Eyapp::Parse;
use strict;

push @Parse::Eyapp::Parse::ISA, 'Parse::Eyapp::Driver';


BEGIN {
  # This strange way to load the modules is to guarantee compatibility when
  # using several standalone and non-standalone Eyapp parsers

  require Parse::Eyapp::Driver unless Parse::Eyapp::Driver->can('YYParse');
  require Parse::Eyapp::Node unless Parse::Eyapp::Node->can('hnew'); 
}
  

# (c) Copyright Casiano Rodriguez-Leon 
# Based on the original yapp by Francois Desarmenien 1998-2001
# (c) Parse::Yapp Copyright 1998-2001 Francois Desarmenien, all rights reserved.
# (c) Parse::Eyapp Copyright 2006-2008 Casiano Rodriguez Leon, all rights reserved.

require 5.004;

use Carp;

my (
  $input,
  $lexlevel, # Used by the lexical analyzer. Controls in which section we are:
             # head (0), body(1) or tail (2)
  @lineno,   # Used by the lexical analyzer. $lineno[0] is the lione number for 
             # the beginning of the token, $lineno[1] the end
  $nberr,    # Number of errors up to now
  $prec,
  $labelno);
my($syms,$head,$tail,$token,$term,$nterm,$rules,$precterm,$start,$nullable,
   $semantic);
my($expect);
my ($namingscheme, $defaultaction);
my $filename;
my $tree = 0; # true if %tree or %metatree
my $metatree = 0;
my $flatlists = 0; # true if flat list semantic for * + and ? operators
my $bypass = 0;
my $prefix = ''; # yyprefix
my $buildingtree = 0;
my $alias = 0;
my $accessors = {}; # Hash for named accessors when %tree or %metatree is active { exp::left => 0 }
my $strict = 0; # When true, all tokens must be declared or a warning will be issued
my $nocompact; # Do not compact action tables. No DEFAULT field for "STATES"

my %nondeclared; # Potential non declared token identifiers appearing in the program
my @conflict;    # List of [conflict name, code handler]

sub not_an_id {
  my $id = shift;

  !defined($id) or $id !~ m/^[a-zA-Z_][[a-zA-Z_0-9]*$/;
}

# When using %metatree, i.e. generating a Translation Scheme
# returns true if $code was preceded by a %begin directive
sub is_begin_code {
  my $code = shift;

  return (UNIVERSAL::isa($code, 'ARRAY') and exists($code->[2]) and $code->[2] eq 'BEGINCODE');
}

# Produces the text containing the declarations
# and initializations of the associated variables
sub prefixcode {
  my  %index = @_;

  # When TS var $lhs refers to the father node
  my $text = ($metatree)? 'my $lhs = $_[0]; ' : '';

  # No identifiers were associated with the attributes if %index is empty
  return $text unless %index;

  $text .= join "", (map { "my \$$_ = \$_[$index{$_}]; " } (keys(%index)));

  # The former line produces the code for initialization of the attribute 
  # variables so that a production like:
  #                   exp: VAR.left '='.op exp.right { ... semantic action }
  # will produce s.t. like:
  #        sub {
  #            my $left = $_[1]; my $right = $_[3]; my $op = $_[2];  
  #            ... semantic action
  #        }

  return $text;
}

# Computes the hash %index used in sub 'prefixcode' 
# $index{a} is the index of the symbol associated with 'a' in the right hand side
# of the production. For example in 
#                              R: B.b A.a
# $index{a} will be 2.
sub symbol_index {
  my $rhs = shift || [];
  my $position = shift || @$rhs;
  my %index;

  local $_ = 0;
  for my $value (@{$rhs}) {
    $_++ unless ($value->[0] eq 'CODE') and $metatree;
    my $id = $value->[1][2];
    if (defined($id)) {
        _SyntaxError(
          2, 
          "Error: attribute variable '\$$id' appears more than once", 
          $value->[1][1]) 
      if exists($index{$id});
      $index{$id} = $_;
    }
    last if $_ >= $position;
  }

  return %index;
}

# Computes the hash %index holding the position in the generated
# AST (as it is build by YYBuildAST) of the node associated with
# the identifier. For ex. in "E: E.left '+' E.right"
# $index{right} will be 1 (remember that '+' is a syntactic token)
sub child_index_in_AST {
  my $rhs = shift || [];
  my %index;

  local $_ = 0;
  for my $value (@{$rhs}) {
    my ($symb, $line, $id) = @{$value->[1]};

    # Accessors will be build only for explictly named attributes
    # Hal Finkel's patch
    next unless $$semantic{$symb};
    $index{$id} = $_ if defined($id);
    $_++ ;
  }

  return %index;
}

# This sub gives support to the "%tree alias" directive.
# Expands the 'accessors' hash relation 
# for the current production. Uses 'child_index_in_AST'
# to build the mapping between names and indices
sub make_accessors {
  my $name = shift;
  return unless ($tree and $alias and defined($name) and $name->[0] =~m{^[a-zA-Z_]\w*$});

  my $rhs = shift;

  my %index = child_index_in_AST($rhs);
  for (keys(%index)) {
    $accessors->{"$name->[0]::$_"} = $index{$_};
  }
}

# Gives support to %metatree
sub insert_delaying_code {
  my $code = shift;

  # If %begin the code will be executed at "tree time construction"
  return if is_begin_code($$code);
  if ($$code) {
    $$code = [ 
      # The user code is substituted by a builder of a node referencing the
      # actual sub
      "push \@_,  sub { $$code->[0] }; goto &Parse::Eyapp::Driver::YYBuildTS; ", 
      $$code->[1]
    ]; 
  }
  else {
    $$code = [ ' goto &Parse::Eyapp::Driver::YYBuildTS ', $lineno[0]]
  }
}

# Called only from _AddRules
sub process_production {
  my ($rhs) = @_;

  my $position = $#$rules;
  my @newrhs = ();

  for my $s (0..$#$rhs) {
      my($what,$value)=@{$$rhs[$s]};

          $what eq 'CODE'
      and do { # TODO: modify name scheme: RULE_POSITION
          my($tmplhs)='@'.$position."-$s";

          if ($value) {

            # The auxiliary production generated for 
            # intermediate actions has access to the
            # attributes of the symbols to its left
            # Not needed if generating a TS
            my @optarg = $metatree? () : ($s+1); 

            # Variable declarations
            my %index = symbol_index($rhs, @optarg);
            $value->[0] = prefixcode(%index).$value->[0];
          }

          insert_delaying_code(\$value) if $metatree;

          #                       rhs prec   name   code
          push(@$rules,[ $tmplhs, [], undef, undef, $value ]);
          push(@newrhs, $tmplhs);
          next;
      };
      push(@newrhs, $$value[0]);
  }
  return \@newrhs;
}

# Receives a specification of the RHS of a production like in:
#       rhs([ $A, $val], name => $_[2], code => $code_rec, prec => $prec)
# Returns the data structure used to represent the RHS:
#      [ @rhs, $arg{prec}, $arg{name}, $arg{code}]
sub rhs {
  my @rhs = @{shift()};

  my %arg = @_;
  $arg{prec} = exists($arg{prec})? token($arg{prec}): undef;
  $arg{name} = undef unless exists($arg{name});
  $arg{code} = exists($arg{code})? token($arg{code}): undef;
 
  @rhs = map { ['SYMB', $_] } @rhs;

  return [ @rhs, $arg{prec}, $arg{name}, $arg{code}];
}

sub token {
  my $value = shift;

  return [ $value,  $lineno[0]];
}

sub symbol {
  my $id = shift;

  return ['SYMB', $id];
}

################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################

my $warnmessage =<< "EOFWARN";
Warning!: Did you changed the \@Parse::Eyapp::Parse::ISA variable inside the header section of the eyapp program?
EOFWARN

sub new {
  my($class)=shift;
  ref($class) and $class=ref($class);

  warn $warnmessage unless __PACKAGE__->isa('Parse::Eyapp::Driver'); 
  my($self)=$class->SUPER::new( 
    yyversion => '1.151',
    yyGRAMMAR  =>
[
  [ '_SUPERSTART' => '$start', [ 'eyapp', '$end' ], 0 ],
  [ 'eyapp_1' => 'eyapp', [ 'head', 'body', 'tail' ], 0 ],
  [ 'symbol_2' => 'symbol', [ 'LITERAL' ], 0 ],
  [ 'symbol_3' => 'symbol', [ 'ident' ], 0 ],
  [ 'ident_4' => 'ident', [ 'IDENT' ], 0 ],
  [ 'head_5' => 'head', [ 'headsec', '%%' ], 0 ],
  [ 'headsec_6' => 'headsec', [  ], 0 ],
  [ 'headsec_7' => 'headsec', [ 'decls' ], 0 ],
  [ 'decls_8' => 'decls', [ 'decls', 'decl' ], 0 ],
  [ 'decls_9' => 'decls', [ 'decl' ], 0 ],
  [ 'decl_10' => 'decl', [ '\n' ], 0 ],
  [ 'decl_11' => 'decl', [ 'SEMANTIC', 'typedecl', 'symlist', '\n' ], 0 ],
  [ 'decl_12' => 'decl', [ 'SYNTACTIC', 'typedecl', 'symlist', '\n' ], 0 ],
  [ 'decl_13' => 'decl', [ 'TOKEN', 'typedecl', 'symlist', '\n' ], 0 ],
  [ 'decl_14' => 'decl', [ 'ASSOC', 'typedecl', 'symlist', '\n' ], 0 ],
  [ 'decl_15' => 'decl', [ 'START', 'ident', '\n' ], 0 ],
  [ 'decl_16' => 'decl', [ 'PREFIX', '\n' ], 0 ],
  [ 'decl_17' => 'decl', [ 'NAMINGSCHEME', 'CODE', '\n' ], 0 ],
  [ 'decl_18' => 'decl', [ 'HEADCODE', '\n' ], 0 ],
  [ 'decl_19' => 'decl', [ 'UNION', 'CODE', '\n' ], 0 ],
  [ 'decl_20' => 'decl', [ 'DEFAULTACTION', 'CODE', '\n' ], 0 ],
  [ 'decl_21' => 'decl', [ 'TREE', '\n' ], 0 ],
  [ 'decl_22' => 'decl', [ 'METATREE', '\n' ], 0 ],
  [ 'decl_23' => 'decl', [ 'STRICT', '\n' ], 0 ],
  [ 'decl_24' => 'decl', [ 'NOCOMPACT', '\n' ], 0 ],
  [ 'decl_25' => 'decl', [ 'TYPE', 'typedecl', 'identlist', '\n' ], 0 ],
  [ 'decl_26' => 'decl', [ 'CONFLICT', 'ident', 'CODE', '\n' ], 0 ],
  [ 'decl_27' => 'decl', [ 'EXPECT', 'NUMBER', '\n' ], 0 ],
  [ 'decl_28' => 'decl', [ 'EXPECT', 'NUMBER', 'NUMBER', '\n' ], 0 ],
  [ 'decl_29' => 'decl', [ 'error', '\n' ], 0 ],
  [ 'typedecl_30' => 'typedecl', [  ], 0 ],
  [ 'typedecl_31' => 'typedecl', [ '<', 'IDENT', '>' ], 0 ],
  [ 'symlist_32' => 'symlist', [ 'symlist', 'symbol' ], 0 ],
  [ 'symlist_33' => 'symlist', [ 'symbol' ], 0 ],
  [ 'identlist_34' => 'identlist', [ 'identlist', 'ident' ], 0 ],
  [ 'identlist_35' => 'identlist', [ 'ident' ], 0 ],
  [ 'body_36' => 'body', [ 'rulesec', '%%' ], 0 ],
  [ 'body_37' => 'body', [ '%%' ], 0 ],
  [ 'rulesec_38' => 'rulesec', [ 'rulesec', 'rules' ], 0 ],
  [ 'rulesec_39' => 'rulesec', [ 'startrules' ], 0 ],
  [ 'startrules_40' => 'startrules', [ 'IDENT', ':', '@40-2', 'rhss', ';' ], 0 ],
  [ '_CODE' => '@40-2', [  ], 0 ],
  [ 'startrules_42' => 'startrules', [ 'error', ';' ], 0 ],
  [ 'rules_43' => 'rules', [ 'IDENT', ':', 'rhss', ';' ], 0 ],
  [ 'rules_44' => 'rules', [ 'error', ';' ], 0 ],
  [ 'rhss_45' => 'rhss', [ 'rhss', '|', 'rule' ], 0 ],
  [ 'rhss_46' => 'rhss', [ 'rule' ], 0 ],
  [ 'rule_47' => 'rule', [ 'optname', 'rhs', 'prec', 'epscode' ], 0 ],
  [ 'rule_48' => 'rule', [ 'optname', 'rhs' ], 0 ],
  [ 'rhs_49' => 'rhs', [  ], 0 ],
  [ 'rhs_50' => 'rhs', [ 'rhselts' ], 0 ],
  [ 'rhselts_51' => 'rhselts', [ 'rhselts', 'rhseltwithid' ], 0 ],
  [ 'rhselts_52' => 'rhselts', [ 'rhseltwithid' ], 0 ],
  [ 'rhseltwithid_53' => 'rhseltwithid', [ 'rhselt', '.', 'IDENT' ], 0 ],
  [ 'rhseltwithid_54' => 'rhseltwithid', [ '$', 'rhselt' ], 0 ],
  [ 'rhseltwithid_55' => 'rhseltwithid', [ '$', 'error' ], 0 ],
  [ 'rhseltwithid_56' => 'rhseltwithid', [ 'rhselt' ], 0 ],
  [ 'rhselt_57' => 'rhselt', [ 'symbol' ], 0 ],
  [ 'rhselt_58' => 'rhselt', [ 'code' ], 0 ],
  [ 'rhselt_59' => 'rhselt', [ 'DPREC', 'ident' ], 0 ],
  [ 'rhselt_60' => 'rhselt', [ '(', 'optname', 'rhs', ')' ], 0 ],
  [ 'rhselt_61' => 'rhselt', [ 'rhselt', 'STAR' ], 0 ],
  [ 'rhselt_62' => 'rhselt', [ 'rhselt', '<', 'STAR', 'symbol', '>' ], 0 ],
  [ 'rhselt_63' => 'rhselt', [ 'rhselt', 'OPTION' ], 0 ],
  [ 'rhselt_64' => 'rhselt', [ 'rhselt', '<', 'PLUS', 'symbol', '>' ], 0 ],
  [ 'rhselt_65' => 'rhselt', [ 'rhselt', 'PLUS' ], 0 ],
  [ 'optname_66' => 'optname', [  ], 0 ],
  [ 'optname_67' => 'optname', [ 'NAME', 'IDENT' ], 0 ],
  [ 'optname_68' => 'optname', [ 'NAME', 'IDENT', 'LABEL' ], 0 ],
  [ 'prec_69' => 'prec', [ 'PREC', 'symbol' ], 0 ],
  [ 'epscode_70' => 'epscode', [  ], 0 ],
  [ 'epscode_71' => 'epscode', [ 'code' ], 0 ],
  [ 'code_72' => 'code', [ 'CODE' ], 0 ],
  [ 'code_73' => 'code', [ 'BEGINCODE' ], 0 ],
  [ 'tail_74' => 'tail', [  ], 0 ],
  [ 'tail_75' => 'tail', [ 'TAILCODE' ], 0 ],
],
    yyTERMS  =>
{ '' => { ISSEMANTIC => 0 },
	'$' => { ISSEMANTIC => 0 },
	'%%' => { ISSEMANTIC => 0 },
	'(' => { ISSEMANTIC => 0 },
	')' => { ISSEMANTIC => 0 },
	'.' => { ISSEMANTIC => 0 },
	':' => { ISSEMANTIC => 0 },
	';' => { ISSEMANTIC => 0 },
	'<' => { ISSEMANTIC => 0 },
	'>' => { ISSEMANTIC => 0 },
	'\n' => { ISSEMANTIC => 0 },
	'|' => { ISSEMANTIC => 0 },
	ASSOC => { ISSEMANTIC => 1 },
	BEGINCODE => { ISSEMANTIC => 1 },
	CODE => { ISSEMANTIC => 1 },
	CONFLICT => { ISSEMANTIC => 1 },
	DEFAULTACTION => { ISSEMANTIC => 1 },
	DPREC => { ISSEMANTIC => 1 },
	EXPECT => { ISSEMANTIC => 1 },
	HEADCODE => { ISSEMANTIC => 1 },
	IDENT => { ISSEMANTIC => 1 },
	LABEL => { ISSEMANTIC => 1 },
	LITERAL => { ISSEMANTIC => 1 },
	METATREE => { ISSEMANTIC => 1 },
	NAME => { ISSEMANTIC => 1 },
	NAMINGSCHEME => { ISSEMANTIC => 1 },
	NOCOMPACT => { ISSEMANTIC => 1 },
	NUMBER => { ISSEMANTIC => 1 },
	OPTION => { ISSEMANTIC => 1 },
	PLUS => { ISSEMANTIC => 1 },
	PREC => { ISSEMANTIC => 1 },
	PREFIX => { ISSEMANTIC => 1 },
	SEMANTIC => { ISSEMANTIC => 1 },
	STAR => { ISSEMANTIC => 1 },
	START => { ISSEMANTIC => 1 },
	STRICT => { ISSEMANTIC => 1 },
	SYNTACTIC => { ISSEMANTIC => 1 },
	TAILCODE => { ISSEMANTIC => 1 },
	TOKEN => { ISSEMANTIC => 1 },
	TREE => { ISSEMANTIC => 1 },
	TYPE => { ISSEMANTIC => 1 },
	UNION => { ISSEMANTIC => 1 },
	error => { ISSEMANTIC => 1 },
	error => { ISSEMANTIC => 0 },
},
    yyFILENAME  => 'lib/Parse/Eyapp/Parse.yp',
    yystates =>
[
	{#State 0
		ACTIONS => {
			'SEMANTIC' => 1,
			'UNION' => 4,
			'START' => 5,
			'NAMINGSCHEME' => 7,
			'error' => 8,
			'DEFAULTACTION' => 9,
			'ASSOC' => 10,
			'CONFLICT' => 11,
			'TREE' => 13,
			'NOCOMPACT' => 14,
			"%%" => -6,
			'EXPECT' => 15,
			'METATREE' => 16,
			"\n" => 17,
			'SYNTACTIC' => 18,
			'TYPE' => 19,
			'PREFIX' => 21,
			'STRICT' => 22,
			'TOKEN' => 23,
			'HEADCODE' => 24
		},
		GOTOS => {
			'head' => 12,
			'decl' => 6,
			'headsec' => 2,
			'decls' => 20,
			'eyapp' => 3
		}
	},
	{#State 1
		ACTIONS => {
			"<" => 25
		},
		DEFAULT => -30,
		GOTOS => {
			'typedecl' => 26
		}
	},
	{#State 2
		ACTIONS => {
			"%%" => 27
		}
	},
	{#State 3
		ACTIONS => {
			'' => 28
		}
	},
	{#State 4
		ACTIONS => {
			'CODE' => 29
		}
	},
	{#State 5
		ACTIONS => {
			'IDENT' => 30
		},
		GOTOS => {
			'ident' => 31
		}
	},
	{#State 6
		DEFAULT => -9
	},
	{#State 7
		ACTIONS => {
			'CODE' => 32
		}
	},
	{#State 8
		ACTIONS => {
			"\n" => 33
		}
	},
	{#State 9
		ACTIONS => {
			'CODE' => 34
		}
	},
	{#State 10
		ACTIONS => {
			"<" => 25
		},
		DEFAULT => -30,
		GOTOS => {
			'typedecl' => 35
		}
	},
	{#State 11
		ACTIONS => {
			'IDENT' => 30
		},
		GOTOS => {
			'ident' => 36
		}
	},
	{#State 12
		ACTIONS => {
			"%%" => 41,
			'error' => 39,
			'IDENT' => 37
		},
		GOTOS => {
			'body' => 38,
			'rulesec' => 42,
			'startrules' => 40
		}
	},
	{#State 13
		ACTIONS => {
			"\n" => 43
		}
	},
	{#State 14
		ACTIONS => {
			"\n" => 44
		}
	},
	{#State 15
		ACTIONS => {
			'NUMBER' => 45
		}
	},
	{#State 16
		ACTIONS => {
			"\n" => 46
		}
	},
	{#State 17
		DEFAULT => -10
	},
	{#State 18
		ACTIONS => {
			"<" => 25
		},
		DEFAULT => -30,
		GOTOS => {
			'typedecl' => 47
		}
	},
	{#State 19
		ACTIONS => {
			"<" => 25
		},
		DEFAULT => -30,
		GOTOS => {
			'typedecl' => 48
		}
	},
	{#State 20
		ACTIONS => {
			'SEMANTIC' => 1,
			'UNION' => 4,
			'START' => 5,
			'NAMINGSCHEME' => 7,
			'error' => 8,
			'DEFAULTACTION' => 9,
			'ASSOC' => 10,
			'CONFLICT' => 11,
			'TREE' => 13,
			'NOCOMPACT' => 14,
			"%%" => -7,
			'EXPECT' => 15,
			'METATREE' => 16,
			"\n" => 17,
			'SYNTACTIC' => 18,
			'TYPE' => 19,
			'PREFIX' => 21,
			'STRICT' => 22,
			'TOKEN' => 23,
			'HEADCODE' => 24
		},
		GOTOS => {
			'decl' => 49
		}
	},
	{#State 21
		ACTIONS => {
			"\n" => 50
		}
	},
	{#State 22
		ACTIONS => {
			"\n" => 51
		}
	},
	{#State 23
		ACTIONS => {
			"<" => 25
		},
		DEFAULT => -30,
		GOTOS => {
			'typedecl' => 52
		}
	},
	{#State 24
		ACTIONS => {
			"\n" => 53
		}
	},
	{#State 25
		ACTIONS => {
			'IDENT' => 54
		}
	},
	{#State 26
		ACTIONS => {
			'LITERAL' => 55,
			'IDENT' => 30
		},
		GOTOS => {
			'symlist' => 58,
			'symbol' => 56,
			'ident' => 57
		}
	},
	{#State 27
		DEFAULT => -5
	},
	{#State 28
		DEFAULT => 0
	},
	{#State 29
		ACTIONS => {
			"\n" => 59
		}
	},
	{#State 30
		DEFAULT => -4
	},
	{#State 31
		ACTIONS => {
			"\n" => 60
		}
	},
	{#State 32
		ACTIONS => {
			"\n" => 61
		}
	},
	{#State 33
		DEFAULT => -29
	},
	{#State 34
		ACTIONS => {
			"\n" => 62
		}
	},
	{#State 35
		ACTIONS => {
			'LITERAL' => 55,
			'IDENT' => 30
		},
		GOTOS => {
			'symlist' => 63,
			'symbol' => 56,
			'ident' => 57
		}
	},
	{#State 36
		ACTIONS => {
			'CODE' => 64
		}
	},
	{#State 37
		ACTIONS => {
			":" => 65
		}
	},
	{#State 38
		ACTIONS => {
			'TAILCODE' => 67
		},
		DEFAULT => -74,
		GOTOS => {
			'tail' => 66
		}
	},
	{#State 39
		ACTIONS => {
			";" => 68
		}
	},
	{#State 40
		DEFAULT => -39
	},
	{#State 41
		DEFAULT => -37
	},
	{#State 42
		ACTIONS => {
			"%%" => 72,
			'error' => 71,
			'IDENT' => 69
		},
		GOTOS => {
			'rules' => 70
		}
	},
	{#State 43
		DEFAULT => -21
	},
	{#State 44
		DEFAULT => -24
	},
	{#State 45
		ACTIONS => {
			"\n" => 74,
			'NUMBER' => 73
		}
	},
	{#State 46
		DEFAULT => -22
	},
	{#State 47
		ACTIONS => {
			'LITERAL' => 55,
			'IDENT' => 30
		},
		GOTOS => {
			'symlist' => 75,
			'symbol' => 56,
			'ident' => 57
		}
	},
	{#State 48
		ACTIONS => {
			'IDENT' => 30
		},
		GOTOS => {
			'identlist' => 76,
			'ident' => 77
		}
	},
	{#State 49
		DEFAULT => -8
	},
	{#State 50
		DEFAULT => -16
	},
	{#State 51
		DEFAULT => -23
	},
	{#State 52
		ACTIONS => {
			'LITERAL' => 55,
			'IDENT' => 30
		},
		GOTOS => {
			'symlist' => 78,
			'symbol' => 56,
			'ident' => 57
		}
	},
	{#State 53
		DEFAULT => -18
	},
	{#State 54
		ACTIONS => {
			">" => 79
		}
	},
	{#State 55
		DEFAULT => -2
	},
	{#State 56
		DEFAULT => -33
	},
	{#State 57
		DEFAULT => -3
	},
	{#State 58
		ACTIONS => {
			"\n" => 81,
			'LITERAL' => 55,
			'IDENT' => 30
		},
		GOTOS => {
			'symbol' => 80,
			'ident' => 57
		}
	},
	{#State 59
		DEFAULT => -19
	},
	{#State 60
		DEFAULT => -15
	},
	{#State 61
		DEFAULT => -17
	},
	{#State 62
		DEFAULT => -20
	},
	{#State 63
		ACTIONS => {
			"\n" => 82,
			'LITERAL' => 55,
			'IDENT' => 30
		},
		GOTOS => {
			'symbol' => 80,
			'ident' => 57
		}
	},
	{#State 64
		ACTIONS => {
			"\n" => 83
		}
	},
	{#State 65
		DEFAULT => -41,
		GOTOS => {
			'@40-2' => 84
		}
	},
	{#State 66
		DEFAULT => -1
	},
	{#State 67
		DEFAULT => -75
	},
	{#State 68
		DEFAULT => -42
	},
	{#State 69
		ACTIONS => {
			":" => 85
		}
	},
	{#State 70
		DEFAULT => -38
	},
	{#State 71
		ACTIONS => {
			";" => 86
		}
	},
	{#State 72
		DEFAULT => -36
	},
	{#State 73
		ACTIONS => {
			"\n" => 87
		}
	},
	{#State 74
		DEFAULT => -27
	},
	{#State 75
		ACTIONS => {
			"\n" => 88,
			'LITERAL' => 55,
			'IDENT' => 30
		},
		GOTOS => {
			'symbol' => 80,
			'ident' => 57
		}
	},
	{#State 76
		ACTIONS => {
			"\n" => 89,
			'IDENT' => 30
		},
		GOTOS => {
			'ident' => 90
		}
	},
	{#State 77
		DEFAULT => -35
	},
	{#State 78
		ACTIONS => {
			"\n" => 91,
			'LITERAL' => 55,
			'IDENT' => 30
		},
		GOTOS => {
			'symbol' => 80,
			'ident' => 57
		}
	},
	{#State 79
		DEFAULT => -31
	},
	{#State 80
		DEFAULT => -32
	},
	{#State 81
		DEFAULT => -11
	},
	{#State 82
		DEFAULT => -14
	},
	{#State 83
		DEFAULT => -26
	},
	{#State 84
		ACTIONS => {
			'NAME' => 95
		},
		DEFAULT => -66,
		GOTOS => {
			'rule' => 92,
			'rhss' => 94,
			'optname' => 93
		}
	},
	{#State 85
		ACTIONS => {
			'NAME' => 95
		},
		DEFAULT => -66,
		GOTOS => {
			'rule' => 92,
			'rhss' => 96,
			'optname' => 93
		}
	},
	{#State 86
		DEFAULT => -44
	},
	{#State 87
		DEFAULT => -28
	},
	{#State 88
		DEFAULT => -12
	},
	{#State 89
		DEFAULT => -25
	},
	{#State 90
		DEFAULT => -34
	},
	{#State 91
		DEFAULT => -13
	},
	{#State 92
		DEFAULT => -46
	},
	{#State 93
		ACTIONS => {
			'CODE' => 105,
			'LITERAL' => 55,
			'IDENT' => 30,
			'BEGINCODE' => 98,
			"(" => 106,
			'DPREC' => 107,
			"\$" => 100
		},
		DEFAULT => -49,
		GOTOS => {
			'symbol' => 104,
			'rhselt' => 99,
			'rhs' => 97,
			'rhselts' => 102,
			'rhseltwithid' => 101,
			'ident' => 57,
			'code' => 103
		}
	},
	{#State 94
		ACTIONS => {
			"|" => 109,
			";" => 108
		}
	},
	{#State 95
		ACTIONS => {
			'IDENT' => 110
		}
	},
	{#State 96
		ACTIONS => {
			"|" => 109,
			";" => 111
		}
	},
	{#State 97
		ACTIONS => {
			'PREC' => 112
		},
		DEFAULT => -48,
		GOTOS => {
			'prec' => 113
		}
	},
	{#State 98
		DEFAULT => -73
	},
	{#State 99
		ACTIONS => {
			"<" => 114,
			'PLUS' => 115,
			'OPTION' => 116,
			'STAR' => 117,
			"." => 118
		},
		DEFAULT => -56
	},
	{#State 100
		ACTIONS => {
			"(" => 106,
			'DPREC' => 107,
			'error' => 120,
			'CODE' => 105,
			'LITERAL' => 55,
			'IDENT' => 30,
			'BEGINCODE' => 98
		},
		GOTOS => {
			'symbol' => 104,
			'rhselt' => 119,
			'ident' => 57,
			'code' => 103
		}
	},
	{#State 101
		DEFAULT => -52
	},
	{#State 102
		ACTIONS => {
			'CODE' => 105,
			'LITERAL' => 55,
			'IDENT' => 30,
			'BEGINCODE' => 98,
			"(" => 106,
			'DPREC' => 107,
			"\$" => 100
		},
		DEFAULT => -50,
		GOTOS => {
			'symbol' => 104,
			'rhselt' => 99,
			'rhseltwithid' => 121,
			'ident' => 57,
			'code' => 103
		}
	},
	{#State 103
		DEFAULT => -58
	},
	{#State 104
		DEFAULT => -57
	},
	{#State 105
		DEFAULT => -72
	},
	{#State 106
		ACTIONS => {
			'NAME' => 95
		},
		DEFAULT => -66,
		GOTOS => {
			'optname' => 122
		}
	},
	{#State 107
		ACTIONS => {
			'IDENT' => 30
		},
		GOTOS => {
			'ident' => 123
		}
	},
	{#State 108
		DEFAULT => -40
	},
	{#State 109
		ACTIONS => {
			'NAME' => 95
		},
		DEFAULT => -66,
		GOTOS => {
			'rule' => 124,
			'optname' => 93
		}
	},
	{#State 110
		ACTIONS => {
			'LABEL' => 125
		},
		DEFAULT => -67
	},
	{#State 111
		DEFAULT => -43
	},
	{#State 112
		ACTIONS => {
			'LITERAL' => 55,
			'IDENT' => 30
		},
		GOTOS => {
			'symbol' => 126,
			'ident' => 57
		}
	},
	{#State 113
		ACTIONS => {
			'CODE' => 105,
			'BEGINCODE' => 98
		},
		DEFAULT => -70,
		GOTOS => {
			'epscode' => 128,
			'code' => 127
		}
	},
	{#State 114
		ACTIONS => {
			'STAR' => 130,
			'PLUS' => 129
		}
	},
	{#State 115
		DEFAULT => -65
	},
	{#State 116
		DEFAULT => -63
	},
	{#State 117
		DEFAULT => -61
	},
	{#State 118
		ACTIONS => {
			'IDENT' => 131
		}
	},
	{#State 119
		ACTIONS => {
			'OPTION' => 116,
			"<" => 114,
			'PLUS' => 115,
			'STAR' => 117
		},
		DEFAULT => -54
	},
	{#State 120
		DEFAULT => -55
	},
	{#State 121
		DEFAULT => -51
	},
	{#State 122
		ACTIONS => {
			"(" => 106,
			'DPREC' => 107,
			"\$" => 100,
			'CODE' => 105,
			'LITERAL' => 55,
			'IDENT' => 30,
			'BEGINCODE' => 98
		},
		DEFAULT => -49,
		GOTOS => {
			'symbol' => 104,
			'rhselt' => 99,
			'rhs' => 132,
			'rhselts' => 102,
			'rhseltwithid' => 101,
			'ident' => 57,
			'code' => 103
		}
	},
	{#State 123
		DEFAULT => -59
	},
	{#State 124
		DEFAULT => -45
	},
	{#State 125
		DEFAULT => -68
	},
	{#State 126
		DEFAULT => -69
	},
	{#State 127
		DEFAULT => -71
	},
	{#State 128
		DEFAULT => -47
	},
	{#State 129
		ACTIONS => {
			'LITERAL' => 55,
			'IDENT' => 30
		},
		GOTOS => {
			'symbol' => 133,
			'ident' => 57
		}
	},
	{#State 130
		ACTIONS => {
			'LITERAL' => 55,
			'IDENT' => 30
		},
		GOTOS => {
			'symbol' => 134,
			'ident' => 57
		}
	},
	{#State 131
		DEFAULT => -53
	},
	{#State 132
		ACTIONS => {
			")" => 135
		}
	},
	{#State 133
		ACTIONS => {
			">" => 136
		}
	},
	{#State 134
		ACTIONS => {
			">" => 137
		}
	},
	{#State 135
		DEFAULT => -60
	},
	{#State 136
		DEFAULT => -64
	},
	{#State 137
		DEFAULT => -62
	}
],
    yyrules  =>
[
	[#Rule _SUPERSTART
		 '$start', 2, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule eyapp_1
		 'eyapp', 3, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule symbol_2
		 'symbol', 1,
sub { 
                    my($symbol,$lineno)=@{$_[1]};
                        exists($$syms{$symbol})
                    or  do {
                        $$syms{$symbol} = $lineno;
                        $$term{$symbol} = undef;

                        # Warning! 
                        $$semantic{$symbol} = 0 unless exists($$semantic{$symbol});
                    };
                    $_[1]
                }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule symbol_3
		 'symbol', 1, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule ident_4
		 'ident', 1,
sub { 
                    my($symbol,$lineno)=@{$_[1]};
                        exists($$syms{$symbol})
                    or  do {
                        $$syms{$symbol} = $lineno;
                        $$term{$symbol} = undef;

                        # Warning! 
                        $$semantic{$symbol} = 1 unless exists($$semantic{$symbol});
                        # Not declared identifier?
                        $nondeclared{$symbol} = 1 unless (exists($$nterm{$symbol}) or $$term{$symbol});
                    };
                    $_[1]
                }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule head_5
		 'head', 2, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule headsec_6
		 'headsec', 0, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule headsec_7
		 'headsec', 1, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decls_8
		 'decls', 2, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decls_9
		 'decls', 1, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_10
		 'decl', 1, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_11
		 'decl', 4,
sub { 
                for (@{$_[3]}) {
                    my($symbol,$lineno)=@$_;

                        exists($$token{$symbol})
                    and do {
                        _SyntaxError(0,
                                "Token $symbol redefined: ".
                                "Previously defined line $$syms{$symbol}",
                                $lineno);
                        next;
                    };
                    $$token{$symbol}=$lineno;
                    $$term{$symbol} = [ ];
                    $$semantic{$symbol} = 1;
                }
                undef
            }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_12
		 'decl', 4,
sub { 
                for (@{$_[3]}) {
                    my($symbol,$lineno)=@$_;

                        exists($$token{$symbol})
                    and do {
                        _SyntaxError(0,
                                "Token $symbol redefined: ".
                                "Previously defined line $$syms{$symbol}",
                                $lineno);
                        next;
                    };
                    $$token{$symbol}=$lineno;
                    $$term{$symbol} = [ ];
                    $$semantic{$symbol} = 0;
                }
                undef
            }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_13
		 'decl', 4,
sub { 
                for (@{$_[3]}) {
                    my($symbol,$lineno)=@$_;

                        exists($$token{$symbol})
                    and do {
                        _SyntaxError(0,
                                "Token $symbol redefined: ".
                                "Previously defined line $$syms{$symbol}",
                                $lineno);
                        next;
                    };
                    $$token{$symbol}=$lineno;
                    $$term{$symbol} = [ ];
                }
                undef
            }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_14
		 'decl', 4,
sub { 
                for (@{$_[3]}) {
                    my($symbol,$lineno)=@$_;

                        defined($$term{$symbol}[0])
                    and do {
                        _SyntaxError(1,
                            "Precedence for symbol $symbol redefined: ".
                            "Previously defined line $$syms{$symbol}",
                            $lineno);
                        next;
                    };
                    $$token{$symbol}=$lineno;
                    $$term{$symbol} = [ $_[1][0], $prec ];
                }
                ++$prec;
                undef
            }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_15
		 'decl', 3,
sub {  
              $start=$_[2][0]; 
              undef 
            }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_16
		 'decl', 2,
sub { 
              # TODO: Instead of ident has to be a prefix!!!
              $prefix=$_[1][0]; 
              undef 
            }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_17
		 'decl', 3,
sub { 
              $namingscheme = $_[2];
              undef  
            }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_18
		 'decl', 2,
sub {  push(@$head,$_[1]); undef }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_19
		 'decl', 3,
sub {  undef }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_20
		 'decl', 3,
sub {  $defaultaction = $_[2]; undef }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_21
		 'decl', 2,
sub {  
            $tree = $buildingtree = 1;
            $bypass = ($_[1][0] =~m{bypass})? 1 : 0;
            $alias = ($_[1][0] =~m{alias})? 1 : 0;
            $defaultaction = [ ' goto &Parse::Eyapp::Driver::YYBuildAST ', $lineno[0]]; 
            undef 
          }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_22
		 'decl', 2,
sub {  
            $metatree = $tree = $buildingtree = 1;
            undef 
          }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_23
		 'decl', 2,
sub {  
            $strict = 1;
            undef 
          }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_24
		 'decl', 2,
sub {  
            $nocompact = 1;
            undef 
          }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_25
		 'decl', 4,
sub { 
                for ( @{$_[3]} ) {
                    my($symbol,$lineno)=@$_;

                        exists($$nterm{$symbol})
                    and do {
                        _SyntaxError(0,
                                "Non-terminal $symbol redefined: ".
                                "Previously defined line $$syms{$symbol}",
                                $lineno);
                        next;
                    };
                    delete($$term{$symbol});   #not a terminal
                    $$nterm{$symbol}=undef;    #is a non-terminal
                }
            }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_26
		 'decl', 4,
sub { 
              my ($name, $code) = @_[2,3];
              my ($cn, $line) = @$name;
              push @conflict, [$name, $code];

              $$syms{$cn} = $line;
              $$nterm{$cn} = undef;

              undef;
            }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_27
		 'decl', 3,
sub {  $expect=$_[2][0]; undef }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_28
		 'decl', 4,
sub {  $expect= [ $_[2][0], $_[3][0]]; undef }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_29
		 'decl', 2,
sub {  $_[0]->YYErrok }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule typedecl_30
		 'typedecl', 0, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule typedecl_31
		 'typedecl', 3, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule symlist_32
		 'symlist', 2,
sub {  push(@{$_[1]},$_[2]); $_[1] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule symlist_33
		 'symlist', 1,
sub {  [ $_[1] ] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule identlist_34
		 'identlist', 2,
sub {  push(@{$_[1]},$_[2]); $_[1] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule identlist_35
		 'identlist', 1,
sub {  [ $_[1] ] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule body_36
		 'body', 2,
sub { 
                $start
            or  $start=$$rules[1][0];

                ref($$nterm{$start})
            or  _SyntaxError(2,"Start symbol $start not found ".
                                "in rules section",$_[2][1]);

            # Add conflict handlers
            # [ left hand side,   right hand side,  precedence, rulename, code, ]
            for my $c (@conflict) { 
              my ($A, $code) = @$c;
              my $rhss = [ rhs([], name => $A, code => $code->[0]), ];
              _AddRules($A, $rhss);
            }

            # # If exists an @identifiers that is not a nterm and not a term is a warn
            if ($strict) {
              for (keys %nondeclared) {
                  warn "Warning! Non declared token $_ at line $$syms{$_} of $filename\n" 
                unless ($_ eq 'error' || $$term{$_} || exists($$nterm{$_}));
              }
            }
            # Superstart rule
            # [ left hand side,   right hand side,  precedence, rulename, code, ]
            $$rules[0]=[ '$start', [ $start, chr(0) ], undef, undef, undef,];  

        }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule body_37
		 'body', 1,
sub {  _SyntaxError(2,"No rules in input grammar",$_[1][1]); }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rulesec_38
		 'rulesec', 2, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rulesec_39
		 'rulesec', 1, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule startrules_40
		 'startrules', 5,
sub {  _AddRules($_[1],$_[4]); undef }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule _CODE
		 '@40-2', 0,
sub {  $start = $_[1][0] unless $start; }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule startrules_42
		 'startrules', 2,
sub {  $_[0]->YYErrok }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rules_43
		 'rules', 4,
sub {  _AddRules($_[1],$_[3]); undef }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rules_44
		 'rules', 2,
sub {  $_[0]->YYErrok }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhss_45
		 'rhss', 3,
sub {  push(@{$_[1]},$_[3]); $_[1] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhss_46
		 'rhss', 1,
sub {  [ $_[1] ] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rule_47
		 'rule', 4,
sub {  
            my ($name, $rhs, $prec, $code) = @_[1..4];

            my %index = symbol_index($rhs);
            $code->[0] = prefixcode(%index).$code->[0] if ($code);

            insert_delaying_code(\$code) if $metatree;
            make_accessors($name, $rhs);
            
            push(@{$rhs}, $prec, $name, $code);  # only three???? what with prefixofcode?
            $rhs
          }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rule_48
		 'rule', 2,
sub { 
            my ($name, $rhs) = @_[1, 2];
            my $code;

            # Be careful: $defaultaction must be replicated per action
            # to emulate "yacc/yapp" true behavior.
            # There was a previous bug when %metatree and %defaultaction
            # were activated ------------------V
            $code = $defaultaction && [ @$defaultaction ];

                defined($rhs)
            and $rhs->[-1][0] eq 'CODE'
            and $code = ${pop(@{$rhs})}[1];

            my %index = symbol_index($rhs);
            $code->[0] = prefixcode(%index).$code->[0] if ($code);
            make_accessors($name, $rhs);

            insert_delaying_code(\$code) if $metatree;
            
            push(@{$rhs}, undef, $name, $code);

            $rhs
        }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhs_49
		 'rhs', 0, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhs_50
		 'rhs', 1, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhselts_51
		 'rhselts', 2,
sub {  
                push(@{$_[1]},$_[2]); 
                $_[1] 
              }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhselts_52
		 'rhselts', 1,
sub {  [ $_[1] ] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhseltwithid_53
		 'rhseltwithid', 3,
sub { 
          push @{$_[1][1]}, $_[3][0];
          $_[1]
        }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhseltwithid_54
		 'rhseltwithid', 2,
sub { 
          # check that is an identifier
            _SyntaxError(2,"\$ is allowed for identifiers only (Use dot notation instead)",$lineno[0]) 
          if not_an_id($_[2][1][0]);
          push @{$_[2][1]}, $_[2][1][0];
          $_[2]
        }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhseltwithid_55
		 'rhseltwithid', 2,
sub {  _SyntaxError(2,"\$ is allowed for identifiers only",$lineno[0]) }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhseltwithid_56
		 'rhseltwithid', 1, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhselt_57
		 'rhselt', 1,
sub {  [ 'SYMB', $_[1] ] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhselt_58
		 'rhselt', 1,
sub {  [ 'CODE', $_[1] ] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhselt_59
		 'rhselt', 2,
sub {  [ 'SYMB', $_[2] ] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhselt_60
		 'rhselt', 4,
sub {  
           my ($name, $rhs) = @_[2, 3];


           my $code = $defaultaction && [ @$defaultaction ];
           $code =[ ' goto &Parse::Eyapp::Driver::YYActionforParenthesis', $lineno[0]] unless $metatree;

             defined($rhs)
           and $rhs->[-1][0] eq 'CODE'
           and $code = ${pop(@$rhs)}[1];

           my %index = symbol_index($rhs);
           $code->[0] = prefixcode(%index).$code->[0] if ($code);

           insert_delaying_code(\$code) if $metatree;
            
           my $A = token('PAREN-'.++$labelno);
           _AddRules($A, [[@$rhs, undef, $name, $code]]);

           [ 'SYMB', $A] 
        }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhselt_61
		 'rhselt', 2,
sub {  
          my ($what, $val) = @{$_[1]};
          _SyntaxError(1, "Star(*) operator can't be applied to an action", $lineno[0]) 
            if $what eq 'CODE';
          my $A = token('STAR-'.++$labelno);
          my $code_rec = ' goto &Parse::Eyapp::Driver::YYActionforT_TX1X2 ';
          my $code_empty = ' goto &Parse::Eyapp::Driver::YYActionforT_empty ';

          my $rhss = [
                      rhs([ $A, $val], name => $_[2], code => $code_rec),
                      rhs([],          name => $_[2], code => $code_empty),
                    ];
          _AddRules($A, $rhss);
          [ 'SYMB', $A]
        }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhselt_62
		 'rhselt', 5,
sub {  
          my ($what, $val) = @{$_[1]};
          _SyntaxError(1, "Star(*) operator can't be applied to an action", $lineno[0]) 
            if $what eq 'CODE';
          my $B = token('STAR-'.++$labelno);
          my $code_rec = ' goto &Parse::Eyapp::Driver::YYActionforT_TX1X2 ';
          my $code_single = ' goto &Parse::Eyapp::Driver::YYActionforT_single ';
          my $rhss = [#rhs [token , [value, line]] ...,   prec,  name,  code ]
                      rhs([ $B, $_[4], $val], name => $_[3], code => $code_rec),
                      rhs([ $val],            name =>  $_[3], code => $code_single),
                    ];
          _AddRules($B, $rhss);

          my $A = token('STAR-'.++$labelno);
          my $code_empty = ' goto &Parse::Eyapp::Driver::YYActionforT_empty ';
          $code_single = ' { $_[1] } # optimize '."\n";

          $rhss = [
              rhs([ $B ], name => $_[3], code => $code_single ),
              rhs([],     name => $_[3], code => $code_empty),
          ];
          _AddRules($A, $rhss);
          [ 'SYMB', $A ]
        }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhselt_63
		 'rhselt', 2,
sub { 
          my ($what, $val) = @{$_[1]};
          _SyntaxError(1, "Question(?) operator can't be applied to an action", $lineno[0]) 
            if $what eq 'CODE';
          my $A = token('OPTIONAL-'.++$labelno);
          my $code_single = ' goto &Parse::Eyapp::Driver::YYActionforT_single ';
          my $code_empty = ' goto &Parse::Eyapp::Driver::YYActionforT_empty ';

          my $rhss = [
                      rhs([ $val], name => $_[2], code => $code_single),
                      rhs([],      name => $_[2], code => $code_empty),
                    ];
          _AddRules($A, $rhss);
          [ 'SYMB', $A]
        }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhselt_64
		 'rhselt', 5,
sub {  
          my ($what, $val) = @{$_[1]};
          _SyntaxError(1, "Plus(+) operator can't be applied to an action", $lineno[0]) 
            if $what eq 'CODE';
          my $A = token('PLUS-'.++$labelno);
          my $code_rec = ' goto &Parse::Eyapp::Driver::YYActionforT_TX1X2 ';
          my $code_single = ' goto &Parse::Eyapp::Driver::YYActionforT_single ';

          my $rhss = [
            rhs([$A, $_[4], $val], name => $_[3], code => $code_rec),
            rhs([$val],            name => $_[3], code => $code_single),
          ];
          _AddRules($A, $rhss);
          [ 'SYMB', $A]
        }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhselt_65
		 'rhselt', 2,
sub { 
           my ($what, $val) = @{$_[1]};
           _SyntaxError(1, "Plus(+) operator can't be applied to an action", $lineno[0]) 
             if $what eq 'CODE';
           my $A = token('PLUS-'.++$labelno);
           my $code_rec = ' goto &Parse::Eyapp::Driver::YYActionforT_TX1X2 ';
           my $code_single = ' goto &Parse::Eyapp::Driver::YYActionforT_single ';

           my $rhss = [
             rhs([$A, $val], name => $_[2], code => $code_rec),
             rhs([$val],     name => $_[2], code =>  $code_single)
           ];

           _AddRules($A, $rhss);
           [ 'SYMB', $A]
        }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule optname_66
		 'optname', 0, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule optname_67
		 'optname', 2,
sub {  
                      # save bypass status
           $_[2][2] = $_[1][0];
           $_[2] 
         }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule optname_68
		 'optname', 3,
sub {   # LABELs are used for dynamic conflict resolution
                      # save bypass status
           $_[2][2] = $_[1][0];
           # 0: identifier 1: line number 2: bypass 
           # concat the label to the name
           $_[2][0] .= "$_[3][0]";

           $_[2] 
         }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule prec_69
		 'prec', 2,
sub { 
                        defined($$term{$_[2][0]})
                    or  do {
                        _SyntaxError(1,"No precedence for symbol $_[2][0]",
                                         $_[2][1]);
                        return undef;
                    };

                    ++$$precterm{$_[2][0]};
                    $$term{$_[2][0]}[1];
        }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule epscode_70
		 'epscode', 0,
sub {  $defaultaction }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule epscode_71
		 'epscode', 1,
sub {  $_[1] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule code_72
		 'code', 1,
sub {  $_[1] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule code_73
		 'code', 1,
sub { 
        _SyntaxError(2, "%begin code is allowed only when metatree is active\n", $lineno[0])
          unless $metatree;
        my $code = $_[1];
        push @$code, 'BEGINCODE';
        return $code;
      }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule tail_74
		 'tail', 0, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule tail_75
		 'tail', 1,
sub {  $tail=$_[1] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	]
],
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
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
         'eyapp_1', 
         'symbol_2', 
         'symbol_3', 
         'ident_4', 
         'head_5', 
         'headsec_6', 
         'headsec_7', 
         'decls_8', 
         'decls_9', 
         'decl_10', 
         'decl_11', 
         'decl_12', 
         'decl_13', 
         'decl_14', 
         'decl_15', 
         'decl_16', 
         'decl_17', 
         'decl_18', 
         'decl_19', 
         'decl_20', 
         'decl_21', 
         'decl_22', 
         'decl_23', 
         'decl_24', 
         'decl_25', 
         'decl_26', 
         'decl_27', 
         'decl_28', 
         'decl_29', 
         'typedecl_30', 
         'typedecl_31', 
         'symlist_32', 
         'symlist_33', 
         'identlist_34', 
         'identlist_35', 
         'body_36', 
         'body_37', 
         'rulesec_38', 
         'rulesec_39', 
         'startrules_40', 
         '_CODE', 
         'startrules_42', 
         'rules_43', 
         'rules_44', 
         'rhss_45', 
         'rhss_46', 
         'rule_47', 
         'rule_48', 
         'rhs_49', 
         'rhs_50', 
         'rhselts_51', 
         'rhselts_52', 
         'rhseltwithid_53', 
         'rhseltwithid_54', 
         'rhseltwithid_55', 
         'rhseltwithid_56', 
         'rhselt_57', 
         'rhselt_58', 
         'rhselt_59', 
         'rhselt_60', 
         'rhselt_61', 
         'rhselt_62', 
         'rhselt_63', 
         'rhselt_64', 
         'rhselt_65', 
         'optname_66', 
         'optname_67', 
         'optname_68', 
         'prec_69', 
         'epscode_70', 
         'epscode_71', 
         'code_72', 
         'code_73', 
         'tail_74', 
         'tail_75', );
  $self;
}


sub _Error {
    my($value)=$_[0]->YYCurval;

    my $token = $$value[0];
    my($what)= $token ? "input: '$token'" : "symbol";

    _SyntaxError(1,"Unexpected $what",$$value[1]);
}

sub slurp_perl_code {
  my($level,$from,$code);

  $from=pos($$input);

  $level=1;
  while($$input=~/([{}])/gc) {
          substr($$input,pos($$input)-1,1) eq '\\' #Quoted
      and next;
          $level += ($1 eq '{' ? 1 : -1)
      or last;
  }
      $level
  and  _SyntaxError(2,"Unmatched { opened line $lineno[0]",-1);
  $code = substr($$input,$from,pos($$input)-$from-1);
  $lineno[1]+= $code=~tr/\n//;
  return [ $code, $lineno[0] ];
}

my $ID = qr{[A-Za-z_][A-Za-z0-9_]*};
my $LABEL = qr{:$ID};

sub _Lexer {
 
    #At EOF
        pos($$input) >= length($$input)
    and return('',[ undef, -1 ]);

    #In TAIL section
        $lexlevel > 1
    and do {
        my($pos)=pos($$input);

        $lineno[0]=$lineno[1];
        $lineno[1]=-1;
        pos($$input)=length($$input);
        return('TAILCODE',[ substr($$input,$pos), $lineno[0] ]);
    };

    #Skip blanks
            $lexlevel == 0
        ?   $$input=~m{\G((?:             # Head section: \n separates declarations
                                [\t\ ]+   # Any white space char but \n
                            |   \#[^\n]*  # Perl like comments
                            |   /\*.*?\*/ # C like comments
                            )+)}xsgc
        :   $$input=~m{\G((?:
                                \s+       # any white space char, including \n
                            |   \#[^\n]*  # Perl like comments
                            |   /\*.*?\*/ # C like comments
                            )+)}xsgc
    and do {
        my($blanks)=$1;

        #Maybe At EOF
            pos($$input) >= length($$input)
        and return('',[ undef, -1 ]);

        $lineno[1]+= $blanks=~tr/\n//;
    };

    $lineno[0]=$lineno[1];

        $$input=~/\G($ID)/gc
    and return('IDENT',[ $1, $lineno[0] ]);


        $$input=~/\G( '                # opening apostrophe
                         (?:[^'\\]|    # an ordinary character
                              \\\\|    # escaped \ i.e. \\
                               \\'|    # escaped apostrophe i.e. \'
                                \\     # escape i.e. \
                        )*?            # non greedy repetitions
                      '                # closing apostrophe
                    )/gxc
    and do {
        my $string = $1;

        # The string 'error' is reserved for the special token 'error'
            $string eq "'error'"
        and do {
            _SyntaxError(0,"Literal 'error' ".
                           "will be treated as error token",$lineno[0]);
            return('IDENT',[ 'error', $lineno[0] ]);
        };

        my $lines = $string =~ tr/\n//;
        _SyntaxError(2, "Constant string $string contains newlines",$lineno[0]) if $lines;
        $lineno[1] += $lines;
        $string = chr(0) if $string eq "''";
        return('LITERAL',[ $string, $lineno[0] ]);
    };

    # New section: body or tail
        $$input=~/\G(%%)/gc
    and do {
        ++$lexlevel;
        return($1, [ $1, $lineno[0] ]);
    };


        $$input=~/\G%begin\s*{/gc
    and do {
        return ('BEGINCODE', &slurp_perl_code());
    };

        $$input=~/\G{/gc
    and do {
        return ('CODE', &slurp_perl_code());
    };

    if($lexlevel == 0) {# In head section
            $$input=~/\G%(left|right|nonassoc)/gc
        and return('ASSOC',[ uc($1), $lineno[0] ]);

            $$input=~/\G%(start)/gc
        and return('START',[ undef, $lineno[0] ]);

            $$input=~/\G%(expect)/gc
        and return('EXPECT',[ undef, $lineno[0] ]);

            $$input=~/\G%{/gc
        and do {
            my($code);

                $$input=~/\G(.*?)%}/sgc
            or  _SyntaxError(2,"Unmatched %{ opened line $lineno[0]",-1);

            $code=$1;
            $lineno[1]+= $code=~tr/\n//;
            return('HEADCODE',[ $code, $lineno[0] ]);
        };
            $$input=~/\G%(token)/gc
        and return('TOKEN',[ undef, $lineno[0] ]);

            $$input=~/\G%(strict)/gc
        and return('STRICT',[ undef, $lineno[0] ]);

            $$input=~/\G%(nocompact)/gc
        and return('NOCOMPACT',[ undef, $lineno[0] ]);

            $$input=~/\G%(semantic\s+token)/gc
        and return('SEMANTIC',[ undef, $lineno[0] ]);

            $$input=~/\G%(syntactic\s+token)/gc
        and return('SYNTACTIC',[ undef, $lineno[0] ]);

            $$input=~/\G%(type)/gc
        and return('TYPE',[ undef, $lineno[0] ]);

            $$input=~/\G%prefix\s+([A-Za-z_][A-Za-z0-9_:]*::)/gc
        and return('PREFIX',[ $1, $lineno[0] ]);

            $$input=~/\G%(union)/gc
        and return('UNION',[ undef, $lineno[0] ]);

            $$input=~/\G%(defaultaction)/gc
        and return('DEFAULTACTION',[ undef, $lineno[0] ]);

            $$input=~/\G%(namingscheme)/gc
        and return('NAMINGSCHEME',[ undef, $lineno[0] ]);

            $$input=~/\G%(tree((?:\s+(?:bypass|alias)){0,2}))/gc
        and do {
          my $treeoptions =  defined($2)? $2 : '';
          return('TREE',[ $treeoptions, $lineno[0] ])
        };

            $$input=~/\G%(conflict)/gc
        and return('CONFLICT',[ undef, $lineno[0] ]);

            $$input=~/\G%metatree/gc
        and return('METATREE',[ undef, $lineno[0] ]);

            $$input=~/\G([0-9]+)/gc
        and return('NUMBER',[ $1, $lineno[0] ]);

    }
    else {# In rule section
            $$input=~/\G%(prec)/gc
        and return('PREC',[ undef, $lineno[0] ]);

            $$input=~/\G%(PREC)/gc
        and return('DPREC',[ undef, $lineno[0] ]);

            $$input=~/\G(<\s*%name\s*($ID)\s*)?\*\s*>/gc
        and return('STAR',[ $2, $lineno[0] ]);
            $$input=~/\G(%name\s*($ID)\s*)?\*/gc
        and return('STAR',[ $2, $lineno[0] ]);

            $$input=~/\G(<\s*%name\s*($ID)\s*)?\+\s*>/gc
        and return('PLUS',[ $2, $lineno[0] ]);
            $$input=~/\G(%name\s*($ID)\s*)?\+/gc
        and return('PLUS',[ $2, $lineno[0] ]);

            $$input=~/\G(<\s*%name\s*($ID)\s*)?\?\s*>/gc
        and return('OPTION',[ $2, $lineno[0] ]);
            $$input=~/\G(%name\s*($ID)\s*)?\?/gc
        and return('OPTION',[ $2, $lineno[0] ]);

            $$input=~/\G%no\s+bypass/gc
        and do {
          #my $bp = defined($1)?0:1; 
          return('NAME',[ 0, $lineno[0] ]);
        };

            $$input=~/\G%name/gc
        and do {
          return('NAME',[ $bypass, $lineno[0] ]);
        };

        $$input=~/\G($LABEL)/gc
    and return('LABEL',[ $1, $lineno[0] ]);

    }

    #Always return something
        $$input=~/\G(.)/sg
    or  die "Parse::Eyapp::Grammar::Parse: Match (.) failed: report as a BUG";

    my $char = $1;

    $char =~ s/\cM/\n/; # dos to unix

        $char eq "\n"
    and ++$lineno[1];

    ( $char ,[ $char, $lineno[0] ]);

}

sub _SyntaxError {
    my($level,$message,$lineno)=@_;

    $message= "*".
              [ 'Warning', 'Error', 'Fatal' ]->[$level].
              "* $message, at ".
              ($lineno < 0 ? "eof" : "line $lineno")." at file $filename\n";

        $level > 1
    and die $message;

    warn $message;

        $level > 0
    and ++$nberr;

        $nberr == 20 
    and die "*Fatal* Too many errors detected.\n"
}

# _AddRules
# There was a serious error I introduced between versions 171 and 172 (subversion
# numbers).  I delayed the instruction
#       my ($tmprule)=[ $lhs, [], splice(@$rhs,-3)];
# with catastrophic consequences for the resulting
# LALR tables.
# The splice of the ($precedence, $name, $code)
# must be done before this line, if not the counts of nullables 
# will no work!
#          @$rhs
#       or  do {
#           ++$$nullable{$lhs};
#           ++$epsrules;
#       };

sub _AddRules {
    my($lhs,$lineno)=@{$_[0]};
    my($rhss)=$_[1];

        ref($$nterm{$lhs})
    and do {
        _SyntaxError(1,"Non-terminal $lhs redefined: ".
                       "Previously declared line $$syms{$lhs}",$lineno);
        return;
    };

        ref($$term{$lhs})
    and do {
        my($where) = exists($$token{$lhs}) ? $$token{$lhs} : $$syms{$lhs};
        _SyntaxError(1,"Non-terminal $lhs previously ".
                       "declared as token line $where",$lineno);
        return;
    };

        ref($$nterm{$lhs})      #declared through %type
    or  do {
            $$syms{$lhs}=$lineno;   #Say it's declared here
            delete($$term{$lhs});   #No more a terminal
    };
    $$nterm{$lhs}=[];       #It's a non-terminal now
    
    # Hal Finkel's patch: a non terminal is a semantic child
    $$semantic{$lhs} = 1; 

    my($epsrules)=0;        #To issue a warning if more than one epsilon rule

    for my $rhs (@$rhss) {
        #               ($precedence, $name, $code)
        my ($tmprule)=[ $lhs, [], splice(@$rhs,-3)];

        # Warning! the splice of the ($precedence, $name, $code)
        # must be done before this line, if not the counts of nullables 
        # will no work!
            @$rhs
        or  do {
            ++$$nullable{$lhs};
            ++$epsrules;
        };

        # Reserve position for current rule
        push(@$rules, undef);
        my $position = $#$rules;

        # Expand to auxiliary productions all the intermediate codes
        $tmprule->[1] = process_production($rhs);
        $$rules[$position] = $tmprule; 
        push(@{$$nterm{$lhs}},$position);
    }

        $epsrules > 1
    and _SyntaxError(0,"More than one empty rule for symbol $lhs",$lineno);
}

sub Parse {
    my($self)=shift;

        @_ > 0
    or  croak("No input grammar\n");

    my($parsed)={};

    $input=\$_[0];

    $lexlevel=0;
    my $firstline = $_[1];
    $filename = $_[2] or croak "Unknown input file";
    @lineno= $firstline? ($firstline, $firstline) : (1,1);
    $nberr=0;
    $prec=0;
    $labelno=0;

    $head=();
    $tail="";

    $syms={};
    $token={};
    $term={};
    $nterm={};
    $rules=[ undef ];   #reserve slot 0 for start rule
    $precterm={};

    $start="";
    $nullable={};
    $expect=0;
    $semantic = {};

    pos($$input)=0;


    $self->YYParse(yylex => \&_Lexer, yyerror => \&_Error); #???

        $nberr
    and _SyntaxError(2,"Errors detected: No output",-1);

    @$parsed{ 'HEAD', 'TAIL', 'RULES', 'NTERM', 'TERM',
              'NULL', 'PREC', 'SYMS',  'START', 'EXPECT', 
              'SEMANTIC', 'BYPASS', 'ACCESSORS', 'BUILDINGTREE',
              'PREFIX',
              'NAMINGSCHEME',
              'NOCOMPACT',
            }
    =       (  $head,  $tail,  $rules,  $nterm,  $term,
               $nullable, $precterm, $syms, $start, $expect, 
               $semantic, $bypass, $accessors, $buildingtree,
               $prefix,
               $namingscheme,
               $nocompact,
            );

    undef($input);
    undef($lexlevel);
    undef(@lineno);
    undef($nberr);
    undef($prec);
    undef($labelno);

    undef($head);
    undef($tail);

    undef($syms);
    undef($token);
    undef($term);
    undef($nterm);
    undef($rules);
    undef($precterm);

    undef($start);
    undef($nullable);
    undef($expect);
    undef($defaultaction);
    undef($semantic);
    undef($buildingtree);

    $parsed
}



=for None

=cut



################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################

1;
