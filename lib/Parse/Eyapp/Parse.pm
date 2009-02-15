########################################################################################
#
#    This file was generated using Parse::Eyapp version 1.139.
#
# (c) Parse::Yapp Copyright 1998-2001 Francois Desarmenien.
# (c) Parse::Eyapp Copyright 2006-2008 Casiano Rodriguez-Leon. Universidad de La Laguna.
#        Don't edit this file, use source file "lib/Parse/Eyapp/Parse.yp" instead.
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
    yyversion => '1.139',
    yyGRAMMAR  =>
[
  [ _SUPERSTART => '$start', [ 'eyapp', '$end' ], 0 ],
  [ eyapp_1 => 'eyapp', [ 'head', 'body', 'tail' ], 0 ],
  [ symbol_2 => 'symbol', [ 'LITERAL' ], 0 ],
  [ symbol_3 => 'symbol', [ 'ident' ], 0 ],
  [ ident_4 => 'ident', [ 'IDENT' ], 0 ],
  [ head_5 => 'head', [ 'headsec', '%%' ], 0 ],
  [ headsec_6 => 'headsec', [  ], 0 ],
  [ headsec_7 => 'headsec', [ 'decls' ], 0 ],
  [ decls_8 => 'decls', [ 'decls', 'decl' ], 0 ],
  [ decls_9 => 'decls', [ 'decl' ], 0 ],
  [ decl_10 => 'decl', [ '\n' ], 0 ],
  [ decl_11 => 'decl', [ 'SEMANTIC', 'typedecl', 'symlist', '\n' ], 0 ],
  [ decl_12 => 'decl', [ 'SYNTACTIC', 'typedecl', 'symlist', '\n' ], 0 ],
  [ decl_13 => 'decl', [ 'TOKEN', 'typedecl', 'symlist', '\n' ], 0 ],
  [ decl_14 => 'decl', [ 'ASSOC', 'typedecl', 'symlist', '\n' ], 0 ],
  [ decl_15 => 'decl', [ 'START', 'ident', '\n' ], 0 ],
  [ decl_16 => 'decl', [ 'PREFIX', '\n' ], 0 ],
  [ decl_17 => 'decl', [ 'NAMINGSCHEME', 'CODE', '\n' ], 0 ],
  [ decl_18 => 'decl', [ 'HEADCODE', '\n' ], 0 ],
  [ decl_19 => 'decl', [ 'UNION', 'CODE', '\n' ], 0 ],
  [ decl_20 => 'decl', [ 'DEFAULTACTION', 'CODE', '\n' ], 0 ],
  [ decl_21 => 'decl', [ 'TREE', '\n' ], 0 ],
  [ decl_22 => 'decl', [ 'METATREE', '\n' ], 0 ],
  [ decl_23 => 'decl', [ 'STRICT', '\n' ], 0 ],
  [ decl_24 => 'decl', [ 'NOCOMPACT', '\n' ], 0 ],
  [ decl_25 => 'decl', [ 'TYPE', 'typedecl', 'identlist', '\n' ], 0 ],
  [ decl_26 => 'decl', [ 'EXPECT', 'NUMBER', '\n' ], 0 ],
  [ decl_27 => 'decl', [ 'error', '\n' ], 0 ],
  [ typedecl_28 => 'typedecl', [  ], 0 ],
  [ typedecl_29 => 'typedecl', [ '<', 'IDENT', '>' ], 0 ],
  [ symlist_30 => 'symlist', [ 'symlist', 'symbol' ], 0 ],
  [ symlist_31 => 'symlist', [ 'symbol' ], 0 ],
  [ identlist_32 => 'identlist', [ 'identlist', 'ident' ], 0 ],
  [ identlist_33 => 'identlist', [ 'ident' ], 0 ],
  [ body_34 => 'body', [ 'rulesec', '%%' ], 0 ],
  [ body_35 => 'body', [ '%%' ], 0 ],
  [ rulesec_36 => 'rulesec', [ 'rulesec', 'rules' ], 0 ],
  [ rulesec_37 => 'rulesec', [ 'startrules' ], 0 ],
  [ startrules_38 => 'startrules', [ 'IDENT', ':', '@38-2', 'rhss', ';' ], 0 ],
  [ _CODE => '@38-2', [  ], 0 ],
  [ startrules_40 => 'startrules', [ 'error', ';' ], 0 ],
  [ rules_41 => 'rules', [ 'IDENT', ':', 'rhss', ';' ], 0 ],
  [ rules_42 => 'rules', [ 'error', ';' ], 0 ],
  [ rhss_43 => 'rhss', [ 'rhss', '|', 'rule' ], 0 ],
  [ rhss_44 => 'rhss', [ 'rule' ], 0 ],
  [ rule_45 => 'rule', [ 'optname', 'rhs', 'prec', 'epscode' ], 0 ],
  [ rule_46 => 'rule', [ 'optname', 'rhs' ], 0 ],
  [ rhs_47 => 'rhs', [  ], 0 ],
  [ rhs_48 => 'rhs', [ 'rhselts' ], 0 ],
  [ rhselts_49 => 'rhselts', [ 'rhselts', 'rhseltwithid' ], 0 ],
  [ rhselts_50 => 'rhselts', [ 'rhseltwithid' ], 0 ],
  [ rhseltwithid_51 => 'rhseltwithid', [ 'rhselt', '.', 'IDENT' ], 0 ],
  [ rhseltwithid_52 => 'rhseltwithid', [ '$', 'rhselt' ], 0 ],
  [ rhseltwithid_53 => 'rhseltwithid', [ '$', 'error' ], 0 ],
  [ rhseltwithid_54 => 'rhseltwithid', [ 'rhselt' ], 0 ],
  [ rhselt_55 => 'rhselt', [ 'symbol' ], 0 ],
  [ rhselt_56 => 'rhselt', [ 'code' ], 0 ],
  [ rhselt_57 => 'rhselt', [ '(', 'optname', 'rhs', ')' ], 0 ],
  [ rhselt_58 => 'rhselt', [ 'rhselt', 'STAR' ], 0 ],
  [ rhselt_59 => 'rhselt', [ 'rhselt', '<', 'STAR', 'symbol', '>' ], 0 ],
  [ rhselt_60 => 'rhselt', [ 'rhselt', 'OPTION' ], 0 ],
  [ rhselt_61 => 'rhselt', [ 'rhselt', '<', 'PLUS', 'symbol', '>' ], 0 ],
  [ rhselt_62 => 'rhselt', [ 'rhselt', 'PLUS' ], 0 ],
  [ optname_63 => 'optname', [  ], 0 ],
  [ optname_64 => 'optname', [ 'NAME', 'IDENT' ], 0 ],
  [ prec_65 => 'prec', [ 'PREC', 'symbol' ], 0 ],
  [ epscode_66 => 'epscode', [  ], 0 ],
  [ epscode_67 => 'epscode', [ 'code' ], 0 ],
  [ code_68 => 'code', [ 'CODE' ], 0 ],
  [ code_69 => 'code', [ 'BEGINCODE' ], 0 ],
  [ tail_70 => 'tail', [  ], 0 ],
  [ tail_71 => 'tail', [ 'TAILCODE' ], 0 ],
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
	DEFAULTACTION => { ISSEMANTIC => 1 },
	EXPECT => { ISSEMANTIC => 1 },
	HEADCODE => { ISSEMANTIC => 1 },
	IDENT => { ISSEMANTIC => 1 },
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
    yyFILENAME  => "lib/Parse/Eyapp/Parse.yp",
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
			'TREE' => 12,
			'NOCOMPACT' => 13,
			"%%" => -6,
			'EXPECT' => 14,
			'METATREE' => 15,
			"\n" => 16,
			'SYNTACTIC' => 17,
			'TYPE' => 18,
			'PREFIX' => 20,
			'STRICT' => 21,
			'TOKEN' => 22,
			'HEADCODE' => 23
		},
		GOTOS => {
			'head' => 11,
			'decl' => 6,
			'headsec' => 2,
			'decls' => 19,
			'eyapp' => 3
		}
	},
	{#State 1
		ACTIONS => {
			"<" => 24
		},
		DEFAULT => -28,
		GOTOS => {
			'typedecl' => 25
		}
	},
	{#State 2
		ACTIONS => {
			"%%" => 26
		}
	},
	{#State 3
		ACTIONS => {
			'' => 27
		}
	},
	{#State 4
		ACTIONS => {
			'CODE' => 28
		}
	},
	{#State 5
		ACTIONS => {
			'IDENT' => 29
		},
		GOTOS => {
			'ident' => 30
		}
	},
	{#State 6
		DEFAULT => -9
	},
	{#State 7
		ACTIONS => {
			'CODE' => 31
		}
	},
	{#State 8
		ACTIONS => {
			"\n" => 32
		}
	},
	{#State 9
		ACTIONS => {
			'CODE' => 33
		}
	},
	{#State 10
		ACTIONS => {
			"<" => 24
		},
		DEFAULT => -28,
		GOTOS => {
			'typedecl' => 34
		}
	},
	{#State 11
		ACTIONS => {
			"%%" => 39,
			'error' => 37,
			'IDENT' => 35
		},
		GOTOS => {
			'body' => 36,
			'rulesec' => 40,
			'startrules' => 38
		}
	},
	{#State 12
		ACTIONS => {
			"\n" => 41
		}
	},
	{#State 13
		ACTIONS => {
			"\n" => 42
		}
	},
	{#State 14
		ACTIONS => {
			'NUMBER' => 43
		}
	},
	{#State 15
		ACTIONS => {
			"\n" => 44
		}
	},
	{#State 16
		DEFAULT => -10
	},
	{#State 17
		ACTIONS => {
			"<" => 24
		},
		DEFAULT => -28,
		GOTOS => {
			'typedecl' => 45
		}
	},
	{#State 18
		ACTIONS => {
			"<" => 24
		},
		DEFAULT => -28,
		GOTOS => {
			'typedecl' => 46
		}
	},
	{#State 19
		ACTIONS => {
			'SEMANTIC' => 1,
			'UNION' => 4,
			'START' => 5,
			'NAMINGSCHEME' => 7,
			'error' => 8,
			'DEFAULTACTION' => 9,
			'ASSOC' => 10,
			'TREE' => 12,
			'NOCOMPACT' => 13,
			"%%" => -7,
			'EXPECT' => 14,
			'METATREE' => 15,
			"\n" => 16,
			'SYNTACTIC' => 17,
			'TYPE' => 18,
			'PREFIX' => 20,
			'STRICT' => 21,
			'TOKEN' => 22,
			'HEADCODE' => 23
		},
		GOTOS => {
			'decl' => 47
		}
	},
	{#State 20
		ACTIONS => {
			"\n" => 48
		}
	},
	{#State 21
		ACTIONS => {
			"\n" => 49
		}
	},
	{#State 22
		ACTIONS => {
			"<" => 24
		},
		DEFAULT => -28,
		GOTOS => {
			'typedecl' => 50
		}
	},
	{#State 23
		ACTIONS => {
			"\n" => 51
		}
	},
	{#State 24
		ACTIONS => {
			'IDENT' => 52
		}
	},
	{#State 25
		ACTIONS => {
			'LITERAL' => 53,
			'IDENT' => 29
		},
		GOTOS => {
			'symlist' => 56,
			'symbol' => 54,
			'ident' => 55
		}
	},
	{#State 26
		DEFAULT => -5
	},
	{#State 27
		DEFAULT => 0
	},
	{#State 28
		ACTIONS => {
			"\n" => 57
		}
	},
	{#State 29
		DEFAULT => -4
	},
	{#State 30
		ACTIONS => {
			"\n" => 58
		}
	},
	{#State 31
		ACTIONS => {
			"\n" => 59
		}
	},
	{#State 32
		DEFAULT => -27
	},
	{#State 33
		ACTIONS => {
			"\n" => 60
		}
	},
	{#State 34
		ACTIONS => {
			'LITERAL' => 53,
			'IDENT' => 29
		},
		GOTOS => {
			'symlist' => 61,
			'symbol' => 54,
			'ident' => 55
		}
	},
	{#State 35
		ACTIONS => {
			":" => 62
		}
	},
	{#State 36
		ACTIONS => {
			'TAILCODE' => 64
		},
		DEFAULT => -70,
		GOTOS => {
			'tail' => 63
		}
	},
	{#State 37
		ACTIONS => {
			";" => 65
		}
	},
	{#State 38
		DEFAULT => -37
	},
	{#State 39
		DEFAULT => -35
	},
	{#State 40
		ACTIONS => {
			"%%" => 69,
			'error' => 68,
			'IDENT' => 66
		},
		GOTOS => {
			'rules' => 67
		}
	},
	{#State 41
		DEFAULT => -21
	},
	{#State 42
		DEFAULT => -24
	},
	{#State 43
		ACTIONS => {
			"\n" => 70
		}
	},
	{#State 44
		DEFAULT => -22
	},
	{#State 45
		ACTIONS => {
			'LITERAL' => 53,
			'IDENT' => 29
		},
		GOTOS => {
			'symlist' => 71,
			'symbol' => 54,
			'ident' => 55
		}
	},
	{#State 46
		ACTIONS => {
			'IDENT' => 29
		},
		GOTOS => {
			'identlist' => 72,
			'ident' => 73
		}
	},
	{#State 47
		DEFAULT => -8
	},
	{#State 48
		DEFAULT => -16
	},
	{#State 49
		DEFAULT => -23
	},
	{#State 50
		ACTIONS => {
			'LITERAL' => 53,
			'IDENT' => 29
		},
		GOTOS => {
			'symlist' => 74,
			'symbol' => 54,
			'ident' => 55
		}
	},
	{#State 51
		DEFAULT => -18
	},
	{#State 52
		ACTIONS => {
			">" => 75
		}
	},
	{#State 53
		DEFAULT => -2
	},
	{#State 54
		DEFAULT => -31
	},
	{#State 55
		DEFAULT => -3
	},
	{#State 56
		ACTIONS => {
			"\n" => 77,
			'LITERAL' => 53,
			'IDENT' => 29
		},
		GOTOS => {
			'symbol' => 76,
			'ident' => 55
		}
	},
	{#State 57
		DEFAULT => -19
	},
	{#State 58
		DEFAULT => -15
	},
	{#State 59
		DEFAULT => -17
	},
	{#State 60
		DEFAULT => -20
	},
	{#State 61
		ACTIONS => {
			"\n" => 78,
			'LITERAL' => 53,
			'IDENT' => 29
		},
		GOTOS => {
			'symbol' => 76,
			'ident' => 55
		}
	},
	{#State 62
		DEFAULT => -39,
		GOTOS => {
			'@38-2' => 79
		}
	},
	{#State 63
		DEFAULT => -1
	},
	{#State 64
		DEFAULT => -71
	},
	{#State 65
		DEFAULT => -40
	},
	{#State 66
		ACTIONS => {
			":" => 80
		}
	},
	{#State 67
		DEFAULT => -36
	},
	{#State 68
		ACTIONS => {
			";" => 81
		}
	},
	{#State 69
		DEFAULT => -34
	},
	{#State 70
		DEFAULT => -26
	},
	{#State 71
		ACTIONS => {
			"\n" => 82,
			'LITERAL' => 53,
			'IDENT' => 29
		},
		GOTOS => {
			'symbol' => 76,
			'ident' => 55
		}
	},
	{#State 72
		ACTIONS => {
			"\n" => 83,
			'IDENT' => 29
		},
		GOTOS => {
			'ident' => 84
		}
	},
	{#State 73
		DEFAULT => -33
	},
	{#State 74
		ACTIONS => {
			"\n" => 85,
			'LITERAL' => 53,
			'IDENT' => 29
		},
		GOTOS => {
			'symbol' => 76,
			'ident' => 55
		}
	},
	{#State 75
		DEFAULT => -29
	},
	{#State 76
		DEFAULT => -30
	},
	{#State 77
		DEFAULT => -11
	},
	{#State 78
		DEFAULT => -14
	},
	{#State 79
		ACTIONS => {
			'NAME' => 89
		},
		DEFAULT => -63,
		GOTOS => {
			'rule' => 86,
			'rhss' => 88,
			'optname' => 87
		}
	},
	{#State 80
		ACTIONS => {
			'NAME' => 89
		},
		DEFAULT => -63,
		GOTOS => {
			'rule' => 86,
			'rhss' => 90,
			'optname' => 87
		}
	},
	{#State 81
		DEFAULT => -42
	},
	{#State 82
		DEFAULT => -12
	},
	{#State 83
		DEFAULT => -25
	},
	{#State 84
		DEFAULT => -32
	},
	{#State 85
		DEFAULT => -13
	},
	{#State 86
		DEFAULT => -44
	},
	{#State 87
		ACTIONS => {
			'CODE' => 99,
			'LITERAL' => 53,
			'IDENT' => 29,
			'BEGINCODE' => 92,
			"(" => 100,
			"\$" => 94
		},
		DEFAULT => -47,
		GOTOS => {
			'symbol' => 98,
			'rhselt' => 93,
			'rhs' => 91,
			'rhselts' => 96,
			'rhseltwithid' => 95,
			'ident' => 55,
			'code' => 97
		}
	},
	{#State 88
		ACTIONS => {
			"|" => 102,
			";" => 101
		}
	},
	{#State 89
		ACTIONS => {
			'IDENT' => 103
		}
	},
	{#State 90
		ACTIONS => {
			"|" => 102,
			";" => 104
		}
	},
	{#State 91
		ACTIONS => {
			'PREC' => 105
		},
		DEFAULT => -46,
		GOTOS => {
			'prec' => 106
		}
	},
	{#State 92
		DEFAULT => -69
	},
	{#State 93
		ACTIONS => {
			"<" => 107,
			'PLUS' => 108,
			'OPTION' => 109,
			'STAR' => 110,
			"." => 111
		},
		DEFAULT => -54
	},
	{#State 94
		ACTIONS => {
			"(" => 100,
			'error' => 113,
			'CODE' => 99,
			'LITERAL' => 53,
			'IDENT' => 29,
			'BEGINCODE' => 92
		},
		GOTOS => {
			'symbol' => 98,
			'rhselt' => 112,
			'ident' => 55,
			'code' => 97
		}
	},
	{#State 95
		DEFAULT => -50
	},
	{#State 96
		ACTIONS => {
			'CODE' => 99,
			'LITERAL' => 53,
			'IDENT' => 29,
			'BEGINCODE' => 92,
			"(" => 100,
			"\$" => 94
		},
		DEFAULT => -48,
		GOTOS => {
			'symbol' => 98,
			'rhselt' => 93,
			'rhseltwithid' => 114,
			'ident' => 55,
			'code' => 97
		}
	},
	{#State 97
		DEFAULT => -56
	},
	{#State 98
		DEFAULT => -55
	},
	{#State 99
		DEFAULT => -68
	},
	{#State 100
		ACTIONS => {
			'NAME' => 89
		},
		DEFAULT => -63,
		GOTOS => {
			'optname' => 115
		}
	},
	{#State 101
		DEFAULT => -38
	},
	{#State 102
		ACTIONS => {
			'NAME' => 89
		},
		DEFAULT => -63,
		GOTOS => {
			'rule' => 116,
			'optname' => 87
		}
	},
	{#State 103
		DEFAULT => -64
	},
	{#State 104
		DEFAULT => -41
	},
	{#State 105
		ACTIONS => {
			'LITERAL' => 53,
			'IDENT' => 29
		},
		GOTOS => {
			'symbol' => 117,
			'ident' => 55
		}
	},
	{#State 106
		ACTIONS => {
			'CODE' => 99,
			'BEGINCODE' => 92
		},
		DEFAULT => -66,
		GOTOS => {
			'epscode' => 119,
			'code' => 118
		}
	},
	{#State 107
		ACTIONS => {
			'STAR' => 121,
			'PLUS' => 120
		}
	},
	{#State 108
		DEFAULT => -62
	},
	{#State 109
		DEFAULT => -60
	},
	{#State 110
		DEFAULT => -58
	},
	{#State 111
		ACTIONS => {
			'IDENT' => 122
		}
	},
	{#State 112
		ACTIONS => {
			'OPTION' => 109,
			"<" => 107,
			'PLUS' => 108,
			'STAR' => 110
		},
		DEFAULT => -52
	},
	{#State 113
		DEFAULT => -53
	},
	{#State 114
		DEFAULT => -49
	},
	{#State 115
		ACTIONS => {
			"(" => 100,
			"\$" => 94,
			'CODE' => 99,
			'LITERAL' => 53,
			'IDENT' => 29,
			'BEGINCODE' => 92
		},
		DEFAULT => -47,
		GOTOS => {
			'symbol' => 98,
			'rhselt' => 93,
			'rhs' => 123,
			'rhselts' => 96,
			'rhseltwithid' => 95,
			'ident' => 55,
			'code' => 97
		}
	},
	{#State 116
		DEFAULT => -43
	},
	{#State 117
		DEFAULT => -65
	},
	{#State 118
		DEFAULT => -67
	},
	{#State 119
		DEFAULT => -45
	},
	{#State 120
		ACTIONS => {
			'LITERAL' => 53,
			'IDENT' => 29
		},
		GOTOS => {
			'symbol' => 124,
			'ident' => 55
		}
	},
	{#State 121
		ACTIONS => {
			'LITERAL' => 53,
			'IDENT' => 29
		},
		GOTOS => {
			'symbol' => 125,
			'ident' => 55
		}
	},
	{#State 122
		DEFAULT => -51
	},
	{#State 123
		ACTIONS => {
			")" => 126
		}
	},
	{#State 124
		ACTIONS => {
			">" => 127
		}
	},
	{#State 125
		ACTIONS => {
			">" => 128
		}
	},
	{#State 126
		DEFAULT => -57
	},
	{#State 127
		DEFAULT => -61
	},
	{#State 128
		DEFAULT => -59
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
		 'decl', 3,
sub {  $expect=$_[2][0]; undef }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_27
		 'decl', 2,
sub {  $_[0]->YYErrok }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule typedecl_28
		 'typedecl', 0, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule typedecl_29
		 'typedecl', 3, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule symlist_30
		 'symlist', 2,
sub {  push(@{$_[1]},$_[2]); $_[1] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule symlist_31
		 'symlist', 1,
sub {  [ $_[1] ] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule identlist_32
		 'identlist', 2,
sub {  push(@{$_[1]},$_[2]); $_[1] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule identlist_33
		 'identlist', 1,
sub {  [ $_[1] ] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule body_34
		 'body', 2,
sub { 
                $start
            or  $start=$$rules[1][0];

                ref($$nterm{$start})
            or  _SyntaxError(2,"Start symbol $start not found ".
                                "in rules section",$_[2][1]);

            # # If exists an @identifiers that is not a nterm and not a term is a warn
            if ($strict) {
              for (keys %nondeclared) {
                  warn "Warning! Non declared token $_ at line $$syms{$_} of $filename\n" 
                unless ($_ eq 'error' || $$term{$_} || exists($$nterm{$_}));
              }
            }
            # Superstart rule
            # [ left hand side,   right hand side,  precedence, rulename, code, ]
            $$rules[0]=[ '$start', [ $start, chr(0) ], undef, undef, undef,]  
        }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule body_35
		 'body', 1,
sub {  _SyntaxError(2,"No rules in input grammar",$_[1][1]); }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rulesec_36
		 'rulesec', 2, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rulesec_37
		 'rulesec', 1, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule startrules_38
		 'startrules', 5,
sub {  _AddRules($_[1],$_[4]); undef }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule _CODE
		 '@38-2', 0,
sub {  $start = $_[1][0] unless $start; }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule startrules_40
		 'startrules', 2,
sub {  $_[0]->YYErrok }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rules_41
		 'rules', 4,
sub {  _AddRules($_[1],$_[3]); undef }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rules_42
		 'rules', 2,
sub {  $_[0]->YYErrok }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhss_43
		 'rhss', 3,
sub {  push(@{$_[1]},$_[3]); $_[1] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhss_44
		 'rhss', 1,
sub {  [ $_[1] ] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rule_45
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
	[#Rule rule_46
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
	[#Rule rhs_47
		 'rhs', 0, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhs_48
		 'rhs', 1, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhselts_49
		 'rhselts', 2,
sub {  
                push(@{$_[1]},$_[2]); 
                $_[1] 
              }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhselts_50
		 'rhselts', 1,
sub {  [ $_[1] ] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhseltwithid_51
		 'rhseltwithid', 3,
sub { 
          push @{$_[1][1]}, $_[3][0];
          $_[1]
        }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhseltwithid_52
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
	[#Rule rhseltwithid_53
		 'rhseltwithid', 2,
sub {  _SyntaxError(2,"\$ is allowed for identifiers only",$lineno[0]) }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhseltwithid_54
		 'rhseltwithid', 1, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhselt_55
		 'rhselt', 1,
sub {  [ 'SYMB', $_[1] ] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhselt_56
		 'rhselt', 1,
sub {  [ 'CODE', $_[1] ] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhselt_57
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
	[#Rule rhselt_58
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
	[#Rule rhselt_59
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
	[#Rule rhselt_60
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
	[#Rule rhselt_61
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
	[#Rule rhselt_62
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
	[#Rule optname_63
		 'optname', 0, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule optname_64
		 'optname', 2,
sub {  
           $_[2][2] = $_[1][0];
           $_[2] 
         }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule prec_65
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
	[#Rule epscode_66
		 'epscode', 0,
sub {  $defaultaction }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule epscode_67
		 'epscode', 1,
sub {  $_[1] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule code_68
		 'code', 1,
sub {  $_[1] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule code_69
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
	[#Rule tail_70
		 'tail', 0, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule tail_71
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

  $self->make_node_classes( qw{TERMINAL _OPTIONAL _STAR_LIST _PLUS_LIST 
         _SUPERSTART
         eyapp_1
         symbol_2
         symbol_3
         ident_4
         head_5
         headsec_6
         headsec_7
         decls_8
         decls_9
         decl_10
         decl_11
         decl_12
         decl_13
         decl_14
         decl_15
         decl_16
         decl_17
         decl_18
         decl_19
         decl_20
         decl_21
         decl_22
         decl_23
         decl_24
         decl_25
         decl_26
         decl_27
         typedecl_28
         typedecl_29
         symlist_30
         symlist_31
         identlist_32
         identlist_33
         body_34
         body_35
         rulesec_36
         rulesec_37
         startrules_38
         _CODE
         startrules_40
         rules_41
         rules_42
         rhss_43
         rhss_44
         rule_45
         rule_46
         rhs_47
         rhs_48
         rhselts_49
         rhselts_50
         rhseltwithid_51
         rhseltwithid_52
         rhseltwithid_53
         rhseltwithid_54
         rhselt_55
         rhselt_56
         rhselt_57
         rhselt_58
         rhselt_59
         rhselt_60
         rhselt_61
         rhselt_62
         optname_63
         optname_64
         prec_65
         epscode_66
         epscode_67
         code_68
         code_69
         tail_70
         tail_71} );
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

        $$input=~/\G([A-Za-z_][A-Za-z0-9_]*)/gc
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

            $$input=~/\G%metatree/gc
        and return('METATREE',[ undef, $lineno[0] ]);

            $$input=~/\G([0-9]+)/gc
        and return('NUMBER',[ $1, $lineno[0] ]);

    }
    else {# In rule section
            $$input=~/\G%(prec)/gc
        and return('PREC',[ undef, $lineno[0] ]);

            $$input=~/\G(<\s*%name\s*([A-Za-z_][A-Za-z0-9_]*)\s*)?\*\s*>/gc
        and return('STAR',[ $2, $lineno[0] ]);
            $$input=~/\G(%name\s*([A-Za-z_][A-Za-z0-9_]*)\s*)?\*/gc
        and return('STAR',[ $2, $lineno[0] ]);

            $$input=~/\G(<\s*%name\s*([A-Za-z_][A-Za-z0-9_]*)\s*)?\+\s*>/gc
        and return('PLUS',[ $2, $lineno[0] ]);
            $$input=~/\G(%name\s*([A-Za-z_][A-Za-z0-9_]*)\s*)?\+/gc
        and return('PLUS',[ $2, $lineno[0] ]);

            $$input=~/\G(<\s*%name\s*([A-Za-z_][A-Za-z0-9_]*)\s*)?\?\s*>/gc
        and return('OPTION',[ $2, $lineno[0] ]);
            $$input=~/\G(%name\s*([A-Za-z_][A-Za-z0-9_]*)\s*)?\?/gc
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
