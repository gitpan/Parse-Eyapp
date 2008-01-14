###################################################################################
#
#    This file was generated using Parse::Eyapp version 1.103.
#
# (c) Parse::Yapp Copyright 1998-2001 Francois Desarmenien.
# (c) Parse::Eyapp Copyright 2006-2007 Casiano Rodriguez-Leon. Universidad de La Laguna.
#        Don't edit this file, use source file "lib/Parse/Eyapp/Parse.yp" instead.
#
#             ANY CHANGE MADE HERE WILL BE LOST !
#
###################################################################################
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
# (c) Parse::Eyapp Copyright 2006-2007 Casiano Rodriguez Leon, all rights reserved.

require 5.004;

use Carp;
use List::Util qw(reduce);

my($input,$lexlevel,@lineno,$nberr,$prec,$labelno);
my($syms,$head,$tail,$token,$term,$nterm,$rules,$precterm,$start,$nullable,
   $semantic);
my($expect);
my ($defaultaction);
my $filename;
my $tree = 0; # true if %tree or %metatree
my $metatree = 0;
my $flatlists = 0; # true if flat list semantic for * + and ? operators
my $bypass = 0;
my $buildingtree = 0;
my $alias = 0;
my $accessors = ''; # code generated for named accessors when %tree or %metatree is active
my $strict = 0; # When true, all tokens must be declared or a warning will be issued

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

  # $index{a} is the index of the symbol associated with a in the right hand side
  # of the production. for ex. in "R: B.b A.a" $index{a} will be 2.
  $text .= reduce { $a.$b} (map { "my \$$_ = \$_[$index{$_}]; " } (keys(%index)));

  return $text;
}

# Computes the hash %index used in the previous sub
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
    next unless defined($id) and $$semantic{$symb};
    $index{$id} = $_;
    $_++ ;
  }

  return %index;
}

# This sub igives support to the "%tree alias" directive
# Builds the named accessors to the children
# for the current production. Uses child_index_in_AST
# to build the mapping between names and indices
sub make_accessors {
  my $name = shift;
  return unless ($tree and $alias and defined($name) and $name->[0] =~m{^[a-zA-Z_]\w*});

  my $rhs = shift;
  my %index = child_index_in_AST($rhs);
  for (keys(%index)) {
    $accessors .= <<"END_OF_ACCESSOR";
sub $name->[0]::$_ {
  my \$self = shift;

  return \$self->child($index{$_}, \@_)
}
END_OF_ACCESSOR
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
        ref($class)
    and $class=ref($class);

    warn $warnmessage unless __PACKAGE__->isa('Parse::Eyapp::Driver'); 
    my($self)=$class->SUPER::new( yyversion => '1.103',
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
  [ decl_16 => 'decl', [ 'HEADCODE', '\n' ], 0 ],
  [ decl_17 => 'decl', [ 'UNION', 'CODE', '\n' ], 0 ],
  [ decl_18 => 'decl', [ 'DEFAULTACTION', 'CODE', '\n' ], 0 ],
  [ decl_19 => 'decl', [ 'TREE', '\n' ], 0 ],
  [ decl_20 => 'decl', [ 'METATREE', '\n' ], 0 ],
  [ decl_21 => 'decl', [ 'STRICT', '\n' ], 0 ],
  [ decl_22 => 'decl', [ 'TYPE', 'typedecl', 'identlist', '\n' ], 0 ],
  [ decl_23 => 'decl', [ 'EXPECT', 'NUMBER', '\n' ], 0 ],
  [ decl_24 => 'decl', [ 'error', '\n' ], 0 ],
  [ typedecl_25 => 'typedecl', [  ], 0 ],
  [ typedecl_26 => 'typedecl', [ '<', 'IDENT', '>' ], 0 ],
  [ symlist_27 => 'symlist', [ 'symlist', 'symbol' ], 0 ],
  [ symlist_28 => 'symlist', [ 'symbol' ], 0 ],
  [ identlist_29 => 'identlist', [ 'identlist', 'ident' ], 0 ],
  [ identlist_30 => 'identlist', [ 'ident' ], 0 ],
  [ body_31 => 'body', [ 'rulesec', '%%' ], 0 ],
  [ body_32 => 'body', [ '%%' ], 0 ],
  [ rulesec_33 => 'rulesec', [ 'rulesec', 'rules' ], 0 ],
  [ rulesec_34 => 'rulesec', [ 'startrules' ], 0 ],
  [ startrules_35 => 'startrules', [ 'IDENT', ':', '@35-2', 'rhss', ';' ], 0 ],
  [ _CODE => '@35-2', [  ], 0 ],
  [ startrules_37 => 'startrules', [ 'error', ';' ], 0 ],
  [ rules_38 => 'rules', [ 'IDENT', ':', 'rhss', ';' ], 0 ],
  [ rules_39 => 'rules', [ 'error', ';' ], 0 ],
  [ rhss_40 => 'rhss', [ 'rhss', '|', 'rule' ], 0 ],
  [ rhss_41 => 'rhss', [ 'rule' ], 0 ],
  [ rule_42 => 'rule', [ 'optname', 'rhs', 'prec', 'epscode' ], 0 ],
  [ rule_43 => 'rule', [ 'optname', 'rhs' ], 0 ],
  [ rhs_44 => 'rhs', [  ], 0 ],
  [ rhs_45 => 'rhs', [ 'rhselts' ], 0 ],
  [ rhselts_46 => 'rhselts', [ 'rhselts', 'rhseltwithid' ], 0 ],
  [ rhselts_47 => 'rhselts', [ 'rhseltwithid' ], 0 ],
  [ rhseltwithid_48 => 'rhseltwithid', [ 'rhselt', '.', 'IDENT' ], 0 ],
  [ rhseltwithid_49 => 'rhseltwithid', [ '$', 'rhselt' ], 0 ],
  [ rhseltwithid_50 => 'rhseltwithid', [ '$', 'error' ], 0 ],
  [ rhseltwithid_51 => 'rhseltwithid', [ 'rhselt' ], 0 ],
  [ rhselt_52 => 'rhselt', [ 'symbol' ], 0 ],
  [ rhselt_53 => 'rhselt', [ 'code' ], 0 ],
  [ rhselt_54 => 'rhselt', [ '(', 'optname', 'rhs', ')' ], 0 ],
  [ rhselt_55 => 'rhselt', [ 'rhselt', 'STAR' ], 0 ],
  [ rhselt_56 => 'rhselt', [ 'rhselt', '<', 'STAR', 'symbol', '>' ], 0 ],
  [ rhselt_57 => 'rhselt', [ 'rhselt', 'OPTION' ], 0 ],
  [ rhselt_58 => 'rhselt', [ 'rhselt', '<', 'PLUS', 'symbol', '>' ], 0 ],
  [ rhselt_59 => 'rhselt', [ 'rhselt', 'PLUS' ], 0 ],
  [ optname_60 => 'optname', [  ], 0 ],
  [ optname_61 => 'optname', [ 'NAME', 'IDENT' ], 0 ],
  [ prec_62 => 'prec', [ 'PREC', 'symbol' ], 0 ],
  [ epscode_63 => 'epscode', [  ], 0 ],
  [ epscode_64 => 'epscode', [ 'code' ], 0 ],
  [ code_65 => 'code', [ 'CODE' ], 0 ],
  [ code_66 => 'code', [ 'BEGINCODE' ], 0 ],
  [ tail_67 => 'tail', [  ], 0 ],
  [ tail_68 => 'tail', [ 'TAILCODE' ], 0 ],
],
                                  yyTERMS  =>
{ '$end' => 0, '$' => 0, '%%' => 0, '(' => 0, ')' => 0, '.' => 0, ':' => 0, ';' => 0, '<' => 0, '>' => 0, '\n' => 0, '|' => 0, ASSOC => 1, BEGINCODE => 1, CODE => 1, DEFAULTACTION => 1, EXPECT => 1, HEADCODE => 1, IDENT => 1, LITERAL => 1, METATREE => 1, NAME => 1, NUMBER => 1, OPTION => 1, PLUS => 1, PREC => 1, SEMANTIC => 1, STAR => 1, START => 1, STRICT => 1, SYNTACTIC => 1, TAILCODE => 1, TOKEN => 1, TREE => 1, TYPE => 1, UNION => 1, error => 1 },
                                  yyFILENAME  => "lib/Parse/Eyapp/Parse.yp",
                                  yystates =>
[
	{#State 0
		ACTIONS => {
			'SEMANTIC' => 1,
			'UNION' => 4,
			'START' => 5,
			'error' => 7,
			'DEFAULTACTION' => 8,
			'ASSOC' => 9,
			'TREE' => 11,
			"%%" => -6,
			'EXPECT' => 12,
			"\n" => 13,
			'METATREE' => 14,
			'SYNTACTIC' => 15,
			'TYPE' => 16,
			'STRICT' => 18,
			'TOKEN' => 19,
			'HEADCODE' => 20
		},
		GOTOS => {
			'head' => 10,
			'decl' => 6,
			'headsec' => 2,
			'decls' => 17,
			'eyapp' => 3
		}
	},
	{#State 1
		ACTIONS => {
			"<" => 21
		},
		DEFAULT => -25,
		GOTOS => {
			'typedecl' => 22
		}
	},
	{#State 2
		ACTIONS => {
			"%%" => 23
		}
	},
	{#State 3
		ACTIONS => {
			'' => 24
		}
	},
	{#State 4
		ACTIONS => {
			'CODE' => 25
		}
	},
	{#State 5
		ACTIONS => {
			'IDENT' => 26
		},
		GOTOS => {
			'ident' => 27
		}
	},
	{#State 6
		DEFAULT => -9
	},
	{#State 7
		ACTIONS => {
			"\n" => 28
		}
	},
	{#State 8
		ACTIONS => {
			'CODE' => 29
		}
	},
	{#State 9
		ACTIONS => {
			"<" => 21
		},
		DEFAULT => -25,
		GOTOS => {
			'typedecl' => 30
		}
	},
	{#State 10
		ACTIONS => {
			"%%" => 35,
			'error' => 33,
			'IDENT' => 31
		},
		GOTOS => {
			'body' => 32,
			'rulesec' => 36,
			'startrules' => 34
		}
	},
	{#State 11
		ACTIONS => {
			"\n" => 37
		}
	},
	{#State 12
		ACTIONS => {
			'NUMBER' => 38
		}
	},
	{#State 13
		DEFAULT => -10
	},
	{#State 14
		ACTIONS => {
			"\n" => 39
		}
	},
	{#State 15
		ACTIONS => {
			"<" => 21
		},
		DEFAULT => -25,
		GOTOS => {
			'typedecl' => 40
		}
	},
	{#State 16
		ACTIONS => {
			"<" => 21
		},
		DEFAULT => -25,
		GOTOS => {
			'typedecl' => 41
		}
	},
	{#State 17
		ACTIONS => {
			'SEMANTIC' => 1,
			'UNION' => 4,
			'START' => 5,
			'error' => 7,
			'DEFAULTACTION' => 8,
			'ASSOC' => 9,
			'TREE' => 11,
			"%%" => -7,
			'EXPECT' => 12,
			'METATREE' => 14,
			"\n" => 13,
			'SYNTACTIC' => 15,
			'TYPE' => 16,
			'STRICT' => 18,
			'TOKEN' => 19,
			'HEADCODE' => 20
		},
		GOTOS => {
			'decl' => 42
		}
	},
	{#State 18
		ACTIONS => {
			"\n" => 43
		}
	},
	{#State 19
		ACTIONS => {
			"<" => 21
		},
		DEFAULT => -25,
		GOTOS => {
			'typedecl' => 44
		}
	},
	{#State 20
		ACTIONS => {
			"\n" => 45
		}
	},
	{#State 21
		ACTIONS => {
			'IDENT' => 46
		}
	},
	{#State 22
		ACTIONS => {
			'LITERAL' => 47,
			'IDENT' => 26
		},
		GOTOS => {
			'symlist' => 50,
			'symbol' => 48,
			'ident' => 49
		}
	},
	{#State 23
		DEFAULT => -5
	},
	{#State 24
		DEFAULT => 0
	},
	{#State 25
		ACTIONS => {
			"\n" => 51
		}
	},
	{#State 26
		DEFAULT => -4
	},
	{#State 27
		ACTIONS => {
			"\n" => 52
		}
	},
	{#State 28
		DEFAULT => -24
	},
	{#State 29
		ACTIONS => {
			"\n" => 53
		}
	},
	{#State 30
		ACTIONS => {
			'LITERAL' => 47,
			'IDENT' => 26
		},
		GOTOS => {
			'symlist' => 54,
			'symbol' => 48,
			'ident' => 49
		}
	},
	{#State 31
		ACTIONS => {
			":" => 55
		}
	},
	{#State 32
		ACTIONS => {
			'TAILCODE' => 57
		},
		DEFAULT => -67,
		GOTOS => {
			'tail' => 56
		}
	},
	{#State 33
		ACTIONS => {
			";" => 58
		}
	},
	{#State 34
		DEFAULT => -34
	},
	{#State 35
		DEFAULT => -32
	},
	{#State 36
		ACTIONS => {
			"%%" => 62,
			'error' => 61,
			'IDENT' => 59
		},
		GOTOS => {
			'rules' => 60
		}
	},
	{#State 37
		DEFAULT => -19
	},
	{#State 38
		ACTIONS => {
			"\n" => 63
		}
	},
	{#State 39
		DEFAULT => -20
	},
	{#State 40
		ACTIONS => {
			'LITERAL' => 47,
			'IDENT' => 26
		},
		GOTOS => {
			'symlist' => 64,
			'symbol' => 48,
			'ident' => 49
		}
	},
	{#State 41
		ACTIONS => {
			'IDENT' => 26
		},
		GOTOS => {
			'identlist' => 65,
			'ident' => 66
		}
	},
	{#State 42
		DEFAULT => -8
	},
	{#State 43
		DEFAULT => -21
	},
	{#State 44
		ACTIONS => {
			'LITERAL' => 47,
			'IDENT' => 26
		},
		GOTOS => {
			'symlist' => 67,
			'symbol' => 48,
			'ident' => 49
		}
	},
	{#State 45
		DEFAULT => -16
	},
	{#State 46
		ACTIONS => {
			">" => 68
		}
	},
	{#State 47
		DEFAULT => -2
	},
	{#State 48
		DEFAULT => -28
	},
	{#State 49
		DEFAULT => -3
	},
	{#State 50
		ACTIONS => {
			"\n" => 70,
			'LITERAL' => 47,
			'IDENT' => 26
		},
		GOTOS => {
			'symbol' => 69,
			'ident' => 49
		}
	},
	{#State 51
		DEFAULT => -17
	},
	{#State 52
		DEFAULT => -15
	},
	{#State 53
		DEFAULT => -18
	},
	{#State 54
		ACTIONS => {
			"\n" => 71,
			'LITERAL' => 47,
			'IDENT' => 26
		},
		GOTOS => {
			'symbol' => 69,
			'ident' => 49
		}
	},
	{#State 55
		DEFAULT => -36,
		GOTOS => {
			'@35-2' => 72
		}
	},
	{#State 56
		DEFAULT => -1
	},
	{#State 57
		DEFAULT => -68
	},
	{#State 58
		DEFAULT => -37
	},
	{#State 59
		ACTIONS => {
			":" => 73
		}
	},
	{#State 60
		DEFAULT => -33
	},
	{#State 61
		ACTIONS => {
			";" => 74
		}
	},
	{#State 62
		DEFAULT => -31
	},
	{#State 63
		DEFAULT => -23
	},
	{#State 64
		ACTIONS => {
			"\n" => 75,
			'LITERAL' => 47,
			'IDENT' => 26
		},
		GOTOS => {
			'symbol' => 69,
			'ident' => 49
		}
	},
	{#State 65
		ACTIONS => {
			"\n" => 76,
			'IDENT' => 26
		},
		GOTOS => {
			'ident' => 77
		}
	},
	{#State 66
		DEFAULT => -30
	},
	{#State 67
		ACTIONS => {
			"\n" => 78,
			'LITERAL' => 47,
			'IDENT' => 26
		},
		GOTOS => {
			'symbol' => 69,
			'ident' => 49
		}
	},
	{#State 68
		DEFAULT => -26
	},
	{#State 69
		DEFAULT => -27
	},
	{#State 70
		DEFAULT => -11
	},
	{#State 71
		DEFAULT => -14
	},
	{#State 72
		ACTIONS => {
			'NAME' => 82
		},
		DEFAULT => -60,
		GOTOS => {
			'rule' => 79,
			'rhss' => 81,
			'optname' => 80
		}
	},
	{#State 73
		ACTIONS => {
			'NAME' => 82
		},
		DEFAULT => -60,
		GOTOS => {
			'rule' => 79,
			'rhss' => 83,
			'optname' => 80
		}
	},
	{#State 74
		DEFAULT => -39
	},
	{#State 75
		DEFAULT => -12
	},
	{#State 76
		DEFAULT => -22
	},
	{#State 77
		DEFAULT => -29
	},
	{#State 78
		DEFAULT => -13
	},
	{#State 79
		DEFAULT => -41
	},
	{#State 80
		ACTIONS => {
			'CODE' => 92,
			'LITERAL' => 47,
			'IDENT' => 26,
			'BEGINCODE' => 85,
			"(" => 93,
			"\$" => 87
		},
		DEFAULT => -44,
		GOTOS => {
			'symbol' => 91,
			'rhselt' => 86,
			'rhs' => 84,
			'rhselts' => 89,
			'rhseltwithid' => 88,
			'ident' => 49,
			'code' => 90
		}
	},
	{#State 81
		ACTIONS => {
			"|" => 95,
			";" => 94
		}
	},
	{#State 82
		ACTIONS => {
			'IDENT' => 96
		}
	},
	{#State 83
		ACTIONS => {
			"|" => 95,
			";" => 97
		}
	},
	{#State 84
		ACTIONS => {
			'PREC' => 98
		},
		DEFAULT => -43,
		GOTOS => {
			'prec' => 99
		}
	},
	{#State 85
		DEFAULT => -66
	},
	{#State 86
		ACTIONS => {
			"<" => 100,
			'PLUS' => 101,
			'OPTION' => 102,
			'STAR' => 103,
			"." => 104
		},
		DEFAULT => -51
	},
	{#State 87
		ACTIONS => {
			"(" => 93,
			'error' => 106,
			'CODE' => 92,
			'LITERAL' => 47,
			'IDENT' => 26,
			'BEGINCODE' => 85
		},
		GOTOS => {
			'symbol' => 91,
			'rhselt' => 105,
			'ident' => 49,
			'code' => 90
		}
	},
	{#State 88
		DEFAULT => -47
	},
	{#State 89
		ACTIONS => {
			'CODE' => 92,
			'LITERAL' => 47,
			'IDENT' => 26,
			'BEGINCODE' => 85,
			"(" => 93,
			"\$" => 87
		},
		DEFAULT => -45,
		GOTOS => {
			'symbol' => 91,
			'rhselt' => 86,
			'rhseltwithid' => 107,
			'ident' => 49,
			'code' => 90
		}
	},
	{#State 90
		DEFAULT => -53
	},
	{#State 91
		DEFAULT => -52
	},
	{#State 92
		DEFAULT => -65
	},
	{#State 93
		ACTIONS => {
			'NAME' => 82
		},
		DEFAULT => -60,
		GOTOS => {
			'optname' => 108
		}
	},
	{#State 94
		DEFAULT => -35
	},
	{#State 95
		ACTIONS => {
			'NAME' => 82
		},
		DEFAULT => -60,
		GOTOS => {
			'rule' => 109,
			'optname' => 80
		}
	},
	{#State 96
		DEFAULT => -61
	},
	{#State 97
		DEFAULT => -38
	},
	{#State 98
		ACTIONS => {
			'LITERAL' => 47,
			'IDENT' => 26
		},
		GOTOS => {
			'symbol' => 110,
			'ident' => 49
		}
	},
	{#State 99
		ACTIONS => {
			'CODE' => 92,
			'BEGINCODE' => 85
		},
		DEFAULT => -63,
		GOTOS => {
			'epscode' => 112,
			'code' => 111
		}
	},
	{#State 100
		ACTIONS => {
			'STAR' => 114,
			'PLUS' => 113
		}
	},
	{#State 101
		DEFAULT => -59
	},
	{#State 102
		DEFAULT => -57
	},
	{#State 103
		DEFAULT => -55
	},
	{#State 104
		ACTIONS => {
			'IDENT' => 115
		}
	},
	{#State 105
		ACTIONS => {
			'OPTION' => 102,
			"<" => 100,
			'PLUS' => 101,
			'STAR' => 103
		},
		DEFAULT => -49
	},
	{#State 106
		DEFAULT => -50
	},
	{#State 107
		DEFAULT => -46
	},
	{#State 108
		ACTIONS => {
			"(" => 93,
			"\$" => 87,
			'CODE' => 92,
			'LITERAL' => 47,
			'IDENT' => 26,
			'BEGINCODE' => 85
		},
		DEFAULT => -44,
		GOTOS => {
			'symbol' => 91,
			'rhselt' => 86,
			'rhs' => 116,
			'rhselts' => 89,
			'rhseltwithid' => 88,
			'ident' => 49,
			'code' => 90
		}
	},
	{#State 109
		DEFAULT => -40
	},
	{#State 110
		DEFAULT => -62
	},
	{#State 111
		DEFAULT => -64
	},
	{#State 112
		DEFAULT => -42
	},
	{#State 113
		ACTIONS => {
			'LITERAL' => 47,
			'IDENT' => 26
		},
		GOTOS => {
			'symbol' => 117,
			'ident' => 49
		}
	},
	{#State 114
		ACTIONS => {
			'LITERAL' => 47,
			'IDENT' => 26
		},
		GOTOS => {
			'symbol' => 118,
			'ident' => 49
		}
	},
	{#State 115
		DEFAULT => -48
	},
	{#State 116
		ACTIONS => {
			")" => 119
		}
	},
	{#State 117
		ACTIONS => {
			">" => 120
		}
	},
	{#State 118
		ACTIONS => {
			">" => 121
		}
	},
	{#State 119
		DEFAULT => -54
	},
	{#State 120
		DEFAULT => -58
	},
	{#State 121
		DEFAULT => -56
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
sub {  $start=$_[2][0]; undef }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_16
		 'decl', 2,
sub {  push(@$head,$_[1]); undef }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_17
		 'decl', 3,
sub {  undef }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_18
		 'decl', 3,
sub {  $defaultaction = $_[2]; undef }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_19
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
	[#Rule decl_20
		 'decl', 2,
sub {  
            $metatree = $tree = $buildingtree = 1;
            undef 
          }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_21
		 'decl', 2,
sub {  
            $strict = 1;
            undef 
          }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_22
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
	[#Rule decl_23
		 'decl', 3,
sub {  $expect=$_[2][0]; undef }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_24
		 'decl', 2,
sub {  $_[0]->YYErrok }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule typedecl_25
		 'typedecl', 0, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule typedecl_26
		 'typedecl', 3, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule symlist_27
		 'symlist', 2,
sub {  push(@{$_[1]},$_[2]); $_[1] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule symlist_28
		 'symlist', 1,
sub {  [ $_[1] ] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule identlist_29
		 'identlist', 2,
sub {  push(@{$_[1]},$_[2]); $_[1] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule identlist_30
		 'identlist', 1,
sub {  [ $_[1] ] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule body_31
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
                unless ($$term{$_} || exists($$nterm{$_}));
              }
            }
            # [ left hand side,   right hand side,  precedence, rulename, code, ??prefixofcode ]
            $$rules[0]=[ '$start', [ $start, chr(0) ], undef, undef, undef,] # prefixofcode???  undef ];
        }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule body_32
		 'body', 1,
sub {  _SyntaxError(2,"No rules in input grammar",$_[1][1]); }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rulesec_33
		 'rulesec', 2, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rulesec_34
		 'rulesec', 1, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule startrules_35
		 'startrules', 5,
sub {  _AddRules($_[1],$_[4]); undef }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule _CODE
		 '@35-2', 0,
sub {  $start = $_[1][0]; }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule startrules_37
		 'startrules', 2,
sub {  $_[0]->YYErrok }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rules_38
		 'rules', 4,
sub {  _AddRules($_[1],$_[3]); undef }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rules_39
		 'rules', 2,
sub {  $_[0]->YYErrok }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhss_40
		 'rhss', 3,
sub {  push(@{$_[1]},$_[3]); $_[1] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhss_41
		 'rhss', 1,
sub {  [ $_[1] ] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rule_42
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
	[#Rule rule_43
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
	[#Rule rhs_44
		 'rhs', 0, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhs_45
		 'rhs', 1, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhselts_46
		 'rhselts', 2,
sub {  
                push(@{$_[1]},$_[2]); 
                $_[1] 
              }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhselts_47
		 'rhselts', 1,
sub {  [ $_[1] ] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhseltwithid_48
		 'rhseltwithid', 3,
sub { 
          push @{$_[1][1]}, $_[3][0];
          $_[1]
        }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhseltwithid_49
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
	[#Rule rhseltwithid_50
		 'rhseltwithid', 2,
sub {  _SyntaxError(2,"\$ is allowed for identifiers only",$lineno[0]) }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhseltwithid_51
		 'rhseltwithid', 1, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhselt_52
		 'rhselt', 1,
sub {  [ 'SYMB', $_[1] ] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhselt_53
		 'rhselt', 1,
sub {  [ 'CODE', $_[1] ] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhselt_54
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
	[#Rule rhselt_55
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
	[#Rule rhselt_56
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
	[#Rule rhselt_57
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
	[#Rule rhselt_58
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
	[#Rule rhselt_59
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
	[#Rule optname_60
		 'optname', 0, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule optname_61
		 'optname', 2,
sub {  
           $_[2][2] = $_[1][0];
           $_[2] 
         }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule prec_62
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
	[#Rule epscode_63
		 'epscode', 0,
sub {  $defaultaction }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule epscode_64
		 'epscode', 1,
sub {  $_[1] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule code_65
		 'code', 1,
sub {  $_[1] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule code_66
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
	[#Rule tail_67
		 'tail', 0, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule tail_68
		 'tail', 1,
sub {  $tail=$_[1] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	]
],
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
                                  yybypass => 0,
                                  yybuildingtree => 0,
                                  @_,);
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
         typedecl_25
         typedecl_26
         symlist_27
         symlist_28
         identlist_29
         identlist_30
         body_31
         body_32
         rulesec_33
         rulesec_34
         startrules_35
         _CODE
         startrules_37
         rules_38
         rules_39
         rhss_40
         rhss_41
         rule_42
         rule_43
         rhs_44
         rhs_45
         rhselts_46
         rhselts_47
         rhseltwithid_48
         rhseltwithid_49
         rhseltwithid_50
         rhseltwithid_51
         rhselt_52
         rhselt_53
         rhselt_54
         rhselt_55
         rhselt_56
         rhselt_57
         rhselt_58
         rhselt_59
         optname_60
         optname_61
         prec_62
         epscode_63
         epscode_64
         code_65
         code_66
         tail_67
         tail_68} );
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
        ?   $$input=~m{\G((?:
                                [\t\ ]+    # Any white space char but \n
                            |   \#[^\n]*  # Perl like comments
                            |   /\*.*?\*/ # C like comments
                            )+)}xsgc
        :   $$input=~m{\G((?:
                                \s+       # any white space char
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
                              \\\\|    # escaped \
                               \\'|    # escaped apostrophe
                                \\     # escape
                        )+?            # non greedy repetitions
                      '                # closing apostrophe
                    )/gxc
    and do {
        my $string = $1;
            $string eq "'error'"
        and do {
            _SyntaxError(0,"Literal 'error' ".
                           "will be treated as error token",$lineno[0]);
            return('IDENT',[ 'error', $lineno[0] ]);
        };
        my $lines = $string =~ tr/\n//;
        _SyntaxError(2, "Constant string $string contains newlines",$lineno[0]) if $lines;
        $lineno[1] += $lines;
        return('LITERAL',[ $string, $lineno[0] ]);
    };

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

            $$input=~/\G%(semantic\s+token)/gc
        and return('SEMANTIC',[ undef, $lineno[0] ]);

            $$input=~/\G%(syntactic\s+token)/gc
        and return('SYNTACTIC',[ undef, $lineno[0] ]);

            $$input=~/\G%(type)/gc
        and return('TYPE',[ undef, $lineno[0] ]);

            $$input=~/\G%(union)/gc
        and return('UNION',[ undef, $lineno[0] ]);

            $$input=~/\G%(defaultaction)/gc
        and return('DEFAULTACTION',[ undef, $lineno[0] ]);

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

        $1 eq "\n"
    and ++$lineno[1];

    ( $1 ,[ $1, $lineno[0] ]);

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
              'SEMANTIC', 'BYPASS', 'ACCESSORS', 'BUILDINGTREE' }
    =       (  $head,  $tail,  $rules,  $nterm,  $term,
               $nullable, $precterm, $syms, $start, $expect, 
               $semantic, $bypass, $accessors, $buildingtree);

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


################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################

1;
