########################################################################################
#
#    This file was generated using Parse::Eyapp version 1.154.
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

my $syms;
my $head;    # list of texts inside header sections
my $tail;
my $token;
my $term;
my $nterm;
my $rules;
my $precterm;
my $start;
my $nullable;
my $semantic;

my ($expect);
my $namingscheme;
my $defaultaction;
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

# To be used with the %lexer directive
sub make_lexer {
  my ($code, $line) = @_;

  my $errline = $line + ($code =~ tr/\n//);

my $lexertemplate = << 'ENDOFLEXER';
__PACKAGE__->YYLexer( 
  sub { # lexical analyzer
    my $self = shift; 
    for (${$self->input()}) {  # contextualize
#line <<line>> "<<filename>>"
      <<code>>       
<<end_user_code>>
      return ('', undef) if ($_ eq '') || (defined(pos($_)) && (pos($_) >= length($_)));
      die("Error at the end of lexical analyzer. Line: <<errline>>. File: <<filename>>. No regexp matched.\n");
    } 
  } # end lexical analyzer
);
ENDOFLEXER

  $lexertemplate =~ s/<<code>>/$code/g;
  $lexertemplate =~ s/<<line>>/$line/g;
  $lexertemplate =~ s/<<errline>>/$errline/g;
  $lexertemplate =~ s/<<filename>>/$filename/g;
  $lexertemplate =~ s/<<end_user_code>>/################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################/g;

  return $lexertemplate;
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
    yyversion => '1.154',
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
  [ 'decl_21' => 'decl', [ 'LEXER', 'CODE', '\n' ], 0 ],
  [ 'decl_22' => 'decl', [ 'TREE', '\n' ], 0 ],
  [ 'decl_23' => 'decl', [ 'METATREE', '\n' ], 0 ],
  [ 'decl_24' => 'decl', [ 'STRICT', '\n' ], 0 ],
  [ 'decl_25' => 'decl', [ 'NOCOMPACT', '\n' ], 0 ],
  [ 'decl_26' => 'decl', [ 'TYPE', 'typedecl', 'identlist', '\n' ], 0 ],
  [ 'decl_27' => 'decl', [ 'CONFLICT', 'ident', 'CODE', '\n' ], 0 ],
  [ 'decl_28' => 'decl', [ 'EXPECT', 'NUMBER', '\n' ], 0 ],
  [ 'decl_29' => 'decl', [ 'EXPECT', 'NUMBER', 'NUMBER', '\n' ], 0 ],
  [ 'decl_30' => 'decl', [ 'EXPECTRR', 'NUMBER', '\n' ], 0 ],
  [ 'decl_31' => 'decl', [ 'error', '\n' ], 0 ],
  [ 'typedecl_32' => 'typedecl', [  ], 0 ],
  [ 'typedecl_33' => 'typedecl', [ '<', 'IDENT', '>' ], 0 ],
  [ 'symlist_34' => 'symlist', [ 'symlist', 'symbol' ], 0 ],
  [ 'symlist_35' => 'symlist', [ 'symbol' ], 0 ],
  [ 'identlist_36' => 'identlist', [ 'identlist', 'ident' ], 0 ],
  [ 'identlist_37' => 'identlist', [ 'ident' ], 0 ],
  [ 'body_38' => 'body', [ 'rulesec', '%%' ], 0 ],
  [ 'body_39' => 'body', [ '%%' ], 0 ],
  [ 'rulesec_40' => 'rulesec', [ 'rulesec', 'rules' ], 0 ],
  [ 'rulesec_41' => 'rulesec', [ 'startrules' ], 0 ],
  [ 'startrules_42' => 'startrules', [ 'IDENT', ':', '@42-2', 'rhss', ';' ], 0 ],
  [ '_CODE' => '@42-2', [  ], 0 ],
  [ 'startrules_44' => 'startrules', [ 'error', ';' ], 0 ],
  [ 'rules_45' => 'rules', [ 'IDENT', ':', 'rhss', ';' ], 0 ],
  [ 'rules_46' => 'rules', [ 'error', ';' ], 0 ],
  [ 'rhss_47' => 'rhss', [ 'rhss', '|', 'rule' ], 0 ],
  [ 'rhss_48' => 'rhss', [ 'rule' ], 0 ],
  [ 'rule_49' => 'rule', [ 'optname', 'rhs', 'prec', 'epscode' ], 0 ],
  [ 'rule_50' => 'rule', [ 'optname', 'rhs' ], 0 ],
  [ 'rhs_51' => 'rhs', [  ], 0 ],
  [ 'rhs_52' => 'rhs', [ 'rhselts' ], 0 ],
  [ 'rhselts_53' => 'rhselts', [ 'rhselts', 'rhseltwithid' ], 0 ],
  [ 'rhselts_54' => 'rhselts', [ 'rhseltwithid' ], 0 ],
  [ 'rhseltwithid_55' => 'rhseltwithid', [ 'rhselt', '.', 'IDENT' ], 0 ],
  [ 'rhseltwithid_56' => 'rhseltwithid', [ '$', 'rhselt' ], 0 ],
  [ 'rhseltwithid_57' => 'rhseltwithid', [ '$', 'error' ], 0 ],
  [ 'rhseltwithid_58' => 'rhseltwithid', [ 'rhselt' ], 0 ],
  [ 'rhselt_59' => 'rhselt', [ 'symbol' ], 0 ],
  [ 'rhselt_60' => 'rhselt', [ 'code' ], 0 ],
  [ 'rhselt_61' => 'rhselt', [ 'DPREC', 'ident' ], 0 ],
  [ 'rhselt_62' => 'rhselt', [ '(', 'optname', 'rhs', ')' ], 0 ],
  [ 'rhselt_63' => 'rhselt', [ 'rhselt', 'STAR' ], 0 ],
  [ 'rhselt_64' => 'rhselt', [ 'rhselt', '<', 'STAR', 'symbol', '>' ], 0 ],
  [ 'rhselt_65' => 'rhselt', [ 'rhselt', 'OPTION' ], 0 ],
  [ 'rhselt_66' => 'rhselt', [ 'rhselt', '<', 'PLUS', 'symbol', '>' ], 0 ],
  [ 'rhselt_67' => 'rhselt', [ 'rhselt', 'PLUS' ], 0 ],
  [ 'optname_68' => 'optname', [  ], 0 ],
  [ 'optname_69' => 'optname', [ 'NAME', 'IDENT' ], 0 ],
  [ 'optname_70' => 'optname', [ 'NAME', 'IDENT', 'LABEL' ], 0 ],
  [ 'prec_71' => 'prec', [ 'PREC', 'symbol' ], 0 ],
  [ 'epscode_72' => 'epscode', [  ], 0 ],
  [ 'epscode_73' => 'epscode', [ 'code' ], 0 ],
  [ 'code_74' => 'code', [ 'CODE' ], 0 ],
  [ 'code_75' => 'code', [ 'BEGINCODE' ], 0 ],
  [ 'tail_76' => 'tail', [  ], 0 ],
  [ 'tail_77' => 'tail', [ 'TAILCODE' ], 0 ],
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
	EXPECTRR => { ISSEMANTIC => 1 },
	HEADCODE => { ISSEMANTIC => 1 },
	IDENT => { ISSEMANTIC => 1 },
	LABEL => { ISSEMANTIC => 1 },
	LEXER => { ISSEMANTIC => 1 },
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
			'LEXER' => 2,
			'UNION' => 5,
			'START' => 6,
			'NAMINGSCHEME' => 8,
			'error' => 9,
			'DEFAULTACTION' => 10,
			'ASSOC' => 11,
			'CONFLICT' => 12,
			'TREE' => 13,
			'NOCOMPACT' => 15,
			"%%" => -6,
			'EXPECT' => 16,
			'METATREE' => 17,
			"\n" => 18,
			'SYNTACTIC' => 19,
			'TYPE' => 20,
			'PREFIX' => 22,
			'STRICT' => 23,
			'TOKEN' => 24,
			'EXPECTRR' => 25,
			'HEADCODE' => 26
		},
		GOTOS => {
			'head' => 14,
			'decl' => 7,
			'headsec' => 3,
			'decls' => 21,
			'eyapp' => 4
		}
	},
	{#State 1
		ACTIONS => {
			"<" => 27
		},
		DEFAULT => -32,
		GOTOS => {
			'typedecl' => 28
		}
	},
	{#State 2
		ACTIONS => {
			'CODE' => 29
		}
	},
	{#State 3
		ACTIONS => {
			"%%" => 30
		}
	},
	{#State 4
		ACTIONS => {
			'' => 31
		}
	},
	{#State 5
		ACTIONS => {
			'CODE' => 32
		}
	},
	{#State 6
		ACTIONS => {
			'IDENT' => 33
		},
		GOTOS => {
			'ident' => 34
		}
	},
	{#State 7
		DEFAULT => -9
	},
	{#State 8
		ACTIONS => {
			'CODE' => 35
		}
	},
	{#State 9
		ACTIONS => {
			"\n" => 36
		}
	},
	{#State 10
		ACTIONS => {
			'CODE' => 37
		}
	},
	{#State 11
		ACTIONS => {
			"<" => 27
		},
		DEFAULT => -32,
		GOTOS => {
			'typedecl' => 38
		}
	},
	{#State 12
		ACTIONS => {
			'IDENT' => 33
		},
		GOTOS => {
			'ident' => 39
		}
	},
	{#State 13
		ACTIONS => {
			"\n" => 40
		}
	},
	{#State 14
		ACTIONS => {
			"%%" => 45,
			'error' => 43,
			'IDENT' => 41
		},
		GOTOS => {
			'body' => 42,
			'rulesec' => 46,
			'startrules' => 44
		}
	},
	{#State 15
		ACTIONS => {
			"\n" => 47
		}
	},
	{#State 16
		ACTIONS => {
			'NUMBER' => 48
		}
	},
	{#State 17
		ACTIONS => {
			"\n" => 49
		}
	},
	{#State 18
		DEFAULT => -10
	},
	{#State 19
		ACTIONS => {
			"<" => 27
		},
		DEFAULT => -32,
		GOTOS => {
			'typedecl' => 50
		}
	},
	{#State 20
		ACTIONS => {
			"<" => 27
		},
		DEFAULT => -32,
		GOTOS => {
			'typedecl' => 51
		}
	},
	{#State 21
		ACTIONS => {
			'SEMANTIC' => 1,
			'LEXER' => 2,
			'UNION' => 5,
			'START' => 6,
			'NAMINGSCHEME' => 8,
			'error' => 9,
			'DEFAULTACTION' => 10,
			'ASSOC' => 11,
			'CONFLICT' => 12,
			'TREE' => 13,
			'NOCOMPACT' => 15,
			"%%" => -7,
			'EXPECT' => 16,
			'METATREE' => 17,
			"\n" => 18,
			'SYNTACTIC' => 19,
			'TYPE' => 20,
			'PREFIX' => 22,
			'STRICT' => 23,
			'TOKEN' => 24,
			'EXPECTRR' => 25,
			'HEADCODE' => 26
		},
		GOTOS => {
			'decl' => 52
		}
	},
	{#State 22
		ACTIONS => {
			"\n" => 53
		}
	},
	{#State 23
		ACTIONS => {
			"\n" => 54
		}
	},
	{#State 24
		ACTIONS => {
			"<" => 27
		},
		DEFAULT => -32,
		GOTOS => {
			'typedecl' => 55
		}
	},
	{#State 25
		ACTIONS => {
			'NUMBER' => 56
		}
	},
	{#State 26
		ACTIONS => {
			"\n" => 57
		}
	},
	{#State 27
		ACTIONS => {
			'IDENT' => 58
		}
	},
	{#State 28
		ACTIONS => {
			'LITERAL' => 59,
			'IDENT' => 33
		},
		GOTOS => {
			'symlist' => 62,
			'symbol' => 60,
			'ident' => 61
		}
	},
	{#State 29
		ACTIONS => {
			"\n" => 63
		}
	},
	{#State 30
		DEFAULT => -5
	},
	{#State 31
		DEFAULT => 0
	},
	{#State 32
		ACTIONS => {
			"\n" => 64
		}
	},
	{#State 33
		DEFAULT => -4
	},
	{#State 34
		ACTIONS => {
			"\n" => 65
		}
	},
	{#State 35
		ACTIONS => {
			"\n" => 66
		}
	},
	{#State 36
		DEFAULT => -31
	},
	{#State 37
		ACTIONS => {
			"\n" => 67
		}
	},
	{#State 38
		ACTIONS => {
			'LITERAL' => 59,
			'IDENT' => 33
		},
		GOTOS => {
			'symlist' => 68,
			'symbol' => 60,
			'ident' => 61
		}
	},
	{#State 39
		ACTIONS => {
			'CODE' => 69
		}
	},
	{#State 40
		DEFAULT => -22
	},
	{#State 41
		ACTIONS => {
			":" => 70
		}
	},
	{#State 42
		ACTIONS => {
			'TAILCODE' => 72
		},
		DEFAULT => -76,
		GOTOS => {
			'tail' => 71
		}
	},
	{#State 43
		ACTIONS => {
			";" => 73
		}
	},
	{#State 44
		DEFAULT => -41
	},
	{#State 45
		DEFAULT => -39
	},
	{#State 46
		ACTIONS => {
			"%%" => 77,
			'error' => 76,
			'IDENT' => 74
		},
		GOTOS => {
			'rules' => 75
		}
	},
	{#State 47
		DEFAULT => -25
	},
	{#State 48
		ACTIONS => {
			"\n" => 79,
			'NUMBER' => 78
		}
	},
	{#State 49
		DEFAULT => -23
	},
	{#State 50
		ACTIONS => {
			'LITERAL' => 59,
			'IDENT' => 33
		},
		GOTOS => {
			'symlist' => 80,
			'symbol' => 60,
			'ident' => 61
		}
	},
	{#State 51
		ACTIONS => {
			'IDENT' => 33
		},
		GOTOS => {
			'identlist' => 81,
			'ident' => 82
		}
	},
	{#State 52
		DEFAULT => -8
	},
	{#State 53
		DEFAULT => -16
	},
	{#State 54
		DEFAULT => -24
	},
	{#State 55
		ACTIONS => {
			'LITERAL' => 59,
			'IDENT' => 33
		},
		GOTOS => {
			'symlist' => 83,
			'symbol' => 60,
			'ident' => 61
		}
	},
	{#State 56
		ACTIONS => {
			"\n" => 84
		}
	},
	{#State 57
		DEFAULT => -18
	},
	{#State 58
		ACTIONS => {
			">" => 85
		}
	},
	{#State 59
		DEFAULT => -2
	},
	{#State 60
		DEFAULT => -35
	},
	{#State 61
		DEFAULT => -3
	},
	{#State 62
		ACTIONS => {
			"\n" => 87,
			'LITERAL' => 59,
			'IDENT' => 33
		},
		GOTOS => {
			'symbol' => 86,
			'ident' => 61
		}
	},
	{#State 63
		DEFAULT => -21
	},
	{#State 64
		DEFAULT => -19
	},
	{#State 65
		DEFAULT => -15
	},
	{#State 66
		DEFAULT => -17
	},
	{#State 67
		DEFAULT => -20
	},
	{#State 68
		ACTIONS => {
			"\n" => 88,
			'LITERAL' => 59,
			'IDENT' => 33
		},
		GOTOS => {
			'symbol' => 86,
			'ident' => 61
		}
	},
	{#State 69
		ACTIONS => {
			"\n" => 89
		}
	},
	{#State 70
		DEFAULT => -43,
		GOTOS => {
			'@42-2' => 90
		}
	},
	{#State 71
		DEFAULT => -1
	},
	{#State 72
		DEFAULT => -77
	},
	{#State 73
		DEFAULT => -44
	},
	{#State 74
		ACTIONS => {
			":" => 91
		}
	},
	{#State 75
		DEFAULT => -40
	},
	{#State 76
		ACTIONS => {
			";" => 92
		}
	},
	{#State 77
		DEFAULT => -38
	},
	{#State 78
		ACTIONS => {
			"\n" => 93
		}
	},
	{#State 79
		DEFAULT => -28
	},
	{#State 80
		ACTIONS => {
			"\n" => 94,
			'LITERAL' => 59,
			'IDENT' => 33
		},
		GOTOS => {
			'symbol' => 86,
			'ident' => 61
		}
	},
	{#State 81
		ACTIONS => {
			"\n" => 95,
			'IDENT' => 33
		},
		GOTOS => {
			'ident' => 96
		}
	},
	{#State 82
		DEFAULT => -37
	},
	{#State 83
		ACTIONS => {
			"\n" => 97,
			'LITERAL' => 59,
			'IDENT' => 33
		},
		GOTOS => {
			'symbol' => 86,
			'ident' => 61
		}
	},
	{#State 84
		DEFAULT => -30
	},
	{#State 85
		DEFAULT => -33
	},
	{#State 86
		DEFAULT => -34
	},
	{#State 87
		DEFAULT => -11
	},
	{#State 88
		DEFAULT => -14
	},
	{#State 89
		DEFAULT => -27
	},
	{#State 90
		ACTIONS => {
			'NAME' => 101
		},
		DEFAULT => -68,
		GOTOS => {
			'rule' => 98,
			'rhss' => 100,
			'optname' => 99
		}
	},
	{#State 91
		ACTIONS => {
			'NAME' => 101
		},
		DEFAULT => -68,
		GOTOS => {
			'rule' => 98,
			'rhss' => 102,
			'optname' => 99
		}
	},
	{#State 92
		DEFAULT => -46
	},
	{#State 93
		DEFAULT => -29
	},
	{#State 94
		DEFAULT => -12
	},
	{#State 95
		DEFAULT => -26
	},
	{#State 96
		DEFAULT => -36
	},
	{#State 97
		DEFAULT => -13
	},
	{#State 98
		DEFAULT => -48
	},
	{#State 99
		ACTIONS => {
			'CODE' => 111,
			'LITERAL' => 59,
			'IDENT' => 33,
			'BEGINCODE' => 104,
			"(" => 112,
			'DPREC' => 113,
			"\$" => 106
		},
		DEFAULT => -51,
		GOTOS => {
			'symbol' => 110,
			'rhselt' => 105,
			'rhs' => 103,
			'rhselts' => 108,
			'rhseltwithid' => 107,
			'ident' => 61,
			'code' => 109
		}
	},
	{#State 100
		ACTIONS => {
			"|" => 115,
			";" => 114
		}
	},
	{#State 101
		ACTIONS => {
			'IDENT' => 116
		}
	},
	{#State 102
		ACTIONS => {
			"|" => 115,
			";" => 117
		}
	},
	{#State 103
		ACTIONS => {
			'PREC' => 118
		},
		DEFAULT => -50,
		GOTOS => {
			'prec' => 119
		}
	},
	{#State 104
		DEFAULT => -75
	},
	{#State 105
		ACTIONS => {
			"<" => 120,
			'PLUS' => 121,
			'OPTION' => 122,
			'STAR' => 123,
			"." => 124
		},
		DEFAULT => -58
	},
	{#State 106
		ACTIONS => {
			"(" => 112,
			'DPREC' => 113,
			'error' => 126,
			'CODE' => 111,
			'LITERAL' => 59,
			'IDENT' => 33,
			'BEGINCODE' => 104
		},
		GOTOS => {
			'symbol' => 110,
			'rhselt' => 125,
			'ident' => 61,
			'code' => 109
		}
	},
	{#State 107
		DEFAULT => -54
	},
	{#State 108
		ACTIONS => {
			'CODE' => 111,
			'LITERAL' => 59,
			'IDENT' => 33,
			'BEGINCODE' => 104,
			"(" => 112,
			'DPREC' => 113,
			"\$" => 106
		},
		DEFAULT => -52,
		GOTOS => {
			'symbol' => 110,
			'rhselt' => 105,
			'rhseltwithid' => 127,
			'ident' => 61,
			'code' => 109
		}
	},
	{#State 109
		DEFAULT => -60
	},
	{#State 110
		DEFAULT => -59
	},
	{#State 111
		DEFAULT => -74
	},
	{#State 112
		ACTIONS => {
			'NAME' => 101
		},
		DEFAULT => -68,
		GOTOS => {
			'optname' => 128
		}
	},
	{#State 113
		ACTIONS => {
			'IDENT' => 33
		},
		GOTOS => {
			'ident' => 129
		}
	},
	{#State 114
		DEFAULT => -42
	},
	{#State 115
		ACTIONS => {
			'NAME' => 101
		},
		DEFAULT => -68,
		GOTOS => {
			'rule' => 130,
			'optname' => 99
		}
	},
	{#State 116
		ACTIONS => {
			'LABEL' => 131
		},
		DEFAULT => -69
	},
	{#State 117
		DEFAULT => -45
	},
	{#State 118
		ACTIONS => {
			'LITERAL' => 59,
			'IDENT' => 33
		},
		GOTOS => {
			'symbol' => 132,
			'ident' => 61
		}
	},
	{#State 119
		ACTIONS => {
			'CODE' => 111,
			'BEGINCODE' => 104
		},
		DEFAULT => -72,
		GOTOS => {
			'epscode' => 134,
			'code' => 133
		}
	},
	{#State 120
		ACTIONS => {
			'STAR' => 136,
			'PLUS' => 135
		}
	},
	{#State 121
		DEFAULT => -67
	},
	{#State 122
		DEFAULT => -65
	},
	{#State 123
		DEFAULT => -63
	},
	{#State 124
		ACTIONS => {
			'IDENT' => 137
		}
	},
	{#State 125
		ACTIONS => {
			'OPTION' => 122,
			"<" => 120,
			'PLUS' => 121,
			'STAR' => 123
		},
		DEFAULT => -56
	},
	{#State 126
		DEFAULT => -57
	},
	{#State 127
		DEFAULT => -53
	},
	{#State 128
		ACTIONS => {
			"(" => 112,
			'DPREC' => 113,
			"\$" => 106,
			'CODE' => 111,
			'LITERAL' => 59,
			'IDENT' => 33,
			'BEGINCODE' => 104
		},
		DEFAULT => -51,
		GOTOS => {
			'symbol' => 110,
			'rhselt' => 105,
			'rhs' => 138,
			'rhselts' => 108,
			'rhseltwithid' => 107,
			'ident' => 61,
			'code' => 109
		}
	},
	{#State 129
		DEFAULT => -61
	},
	{#State 130
		DEFAULT => -47
	},
	{#State 131
		DEFAULT => -70
	},
	{#State 132
		DEFAULT => -71
	},
	{#State 133
		DEFAULT => -73
	},
	{#State 134
		DEFAULT => -49
	},
	{#State 135
		ACTIONS => {
			'LITERAL' => 59,
			'IDENT' => 33
		},
		GOTOS => {
			'symbol' => 139,
			'ident' => 61
		}
	},
	{#State 136
		ACTIONS => {
			'LITERAL' => 59,
			'IDENT' => 33
		},
		GOTOS => {
			'symbol' => 140,
			'ident' => 61
		}
	},
	{#State 137
		DEFAULT => -55
	},
	{#State 138
		ACTIONS => {
			")" => 141
		}
	},
	{#State 139
		ACTIONS => {
			">" => 142
		}
	},
	{#State 140
		ACTIONS => {
			">" => 143
		}
	},
	{#State 141
		DEFAULT => -62
	},
	{#State 142
		DEFAULT => -66
	},
	{#State 143
		DEFAULT => -64
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
		 'decl', 3,
sub {  
                                           my ($code, $line) = @{$_[2]};
                                           push @$head, [ make_lexer($code, $line), $line]; 
                                           undef 
                                         }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_22
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
	[#Rule decl_23
		 'decl', 2,
sub {  
            $metatree = $tree = $buildingtree = 1;
            undef 
          }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_24
		 'decl', 2,
sub {  
            $strict = 1;
            undef 
          }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_25
		 'decl', 2,
sub {  
            $nocompact = 1;
            undef 
          }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_26
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
	[#Rule decl_27
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
	[#Rule decl_28
		 'decl', 3,
sub {  $expect=$_[2][0]; undef }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_29
		 'decl', 4,
sub {  $expect= [ $_[2][0], $_[3][0]]; undef }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_30
		 'decl', 3,
sub {  
                                          $expect = 0 unless defined($expect);
                                          croak "Number of reduce-reduce conflicts is redefined (line $_[2][1], file: $filename)\n" if ref($expect);
                                          $expect= [ $expect, $_[2][0]]; 
                                          undef 
                                        }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_31
		 'decl', 2,
sub {  $_[0]->YYErrok }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule typedecl_32
		 'typedecl', 0, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule typedecl_33
		 'typedecl', 3, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule symlist_34
		 'symlist', 2,
sub {  push(@{$_[1]},$_[2]); $_[1] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule symlist_35
		 'symlist', 1,
sub {  [ $_[1] ] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule identlist_36
		 'identlist', 2,
sub {  push(@{$_[1]},$_[2]); $_[1] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule identlist_37
		 'identlist', 1,
sub {  [ $_[1] ] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule body_38
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
	[#Rule body_39
		 'body', 1,
sub {  _SyntaxError(2,"No rules in input grammar",$_[1][1]); }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rulesec_40
		 'rulesec', 2, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rulesec_41
		 'rulesec', 1, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule startrules_42
		 'startrules', 5,
sub {  _AddRules($_[1],$_[4]); undef }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule _CODE
		 '@42-2', 0,
sub {  $start = $_[1][0] unless $start; }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule startrules_44
		 'startrules', 2,
sub {  $_[0]->YYErrok }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rules_45
		 'rules', 4,
sub {  _AddRules($_[1],$_[3]); undef }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rules_46
		 'rules', 2,
sub {  $_[0]->YYErrok }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhss_47
		 'rhss', 3,
sub {  push(@{$_[1]},$_[3]); $_[1] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhss_48
		 'rhss', 1,
sub {  [ $_[1] ] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rule_49
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
	[#Rule rule_50
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
	[#Rule rhs_51
		 'rhs', 0, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhs_52
		 'rhs', 1, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhselts_53
		 'rhselts', 2,
sub {  
                push(@{$_[1]},$_[2]); 
                $_[1] 
              }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhselts_54
		 'rhselts', 1,
sub {  [ $_[1] ] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhseltwithid_55
		 'rhseltwithid', 3,
sub { 
          push @{$_[1][1]}, $_[3][0];
          $_[1]
        }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhseltwithid_56
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
	[#Rule rhseltwithid_57
		 'rhseltwithid', 2,
sub {  _SyntaxError(2,"\$ is allowed for identifiers only",$lineno[0]) }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhseltwithid_58
		 'rhseltwithid', 1, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhselt_59
		 'rhselt', 1,
sub {  [ 'SYMB', $_[1] ] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhselt_60
		 'rhselt', 1,
sub {  [ 'CODE', $_[1] ] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhselt_61
		 'rhselt', 2,
sub {  [ 'SYMB', $_[2] ] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhselt_62
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
	[#Rule rhselt_63
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
	[#Rule rhselt_64
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
	[#Rule rhselt_65
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
	[#Rule rhselt_66
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
	[#Rule rhselt_67
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
	[#Rule optname_68
		 'optname', 0, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule optname_69
		 'optname', 2,
sub {  
                      # save bypass status
           $_[2][2] = $_[1][0];
           $_[2] 
         }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule optname_70
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
	[#Rule prec_71
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
	[#Rule epscode_72
		 'epscode', 0,
sub {  $defaultaction }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule epscode_73
		 'epscode', 1,
sub {  $_[1] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule code_74
		 'code', 1,
sub {  $_[1] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule code_75
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
	[#Rule tail_76
		 'tail', 0, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule tail_77
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
         'decl_30', 
         'decl_31', 
         'typedecl_32', 
         'typedecl_33', 
         'symlist_34', 
         'symlist_35', 
         'identlist_36', 
         'identlist_37', 
         'body_38', 
         'body_39', 
         'rulesec_40', 
         'rulesec_41', 
         'startrules_42', 
         '_CODE', 
         'startrules_44', 
         'rules_45', 
         'rules_46', 
         'rhss_47', 
         'rhss_48', 
         'rule_49', 
         'rule_50', 
         'rhs_51', 
         'rhs_52', 
         'rhselts_53', 
         'rhselts_54', 
         'rhseltwithid_55', 
         'rhseltwithid_56', 
         'rhseltwithid_57', 
         'rhseltwithid_58', 
         'rhselt_59', 
         'rhselt_60', 
         'rhselt_61', 
         'rhselt_62', 
         'rhselt_63', 
         'rhselt_64', 
         'rhselt_65', 
         'rhselt_66', 
         'rhselt_67', 
         'optname_68', 
         'optname_69', 
         'optname_70', 
         'prec_71', 
         'epscode_72', 
         'epscode_73', 
         'code_74', 
         'code_75', 
         'tail_76', 
         'tail_77', );
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

my %headertoken = (
  start => 'START',
  expect => 'EXPECT',
  token => 'TOKEN',
  strict => 'STRICT',
  type => 'TYPE',
  union => 'UNION',
  namingscheme => 'NAMINGSCHEME',
  metatree => 'METATREE',
  nocompact => 'NOCOMPACT',
  conflict => 'CONFLICT',
);

# Used for <%name LIST_of_STH +>, <%name OPT_STH ?>
my %listtoken = (
  '*' => 'STAR',
  '+' => 'PLUS',
  '?' => 'OPTION',
);

my $ID = qr{[A-Za-z_][A-Za-z0-9_]*};
my $LABEL = qr{:[A-Za-z0-9_]+};
my $STRING = qr {
   '             # opening apostrophe
   (?:[^'\\]|    # an ordinary character
        \\\\|    # escaped \ i.e. \\
         \\'|    # escaped apostrophe i.e. \'
          \\     # escape i.e. \
  )*?            # non greedy repetitions
  '              # closing apostrophe
}x;

# Head section: \n separates declarations
my $HEADERWHITESPACES = qr{ 
  (?:  
      [\t\ ]+     # Any white space char but \n
    | \#[^\n]*    # Perl like comments
    |   /\*.*?\*/ # C like comments
  )+
}xs;

# Head section: \n is not significant
my $BODYWHITESPACES = qr{
  (?:
      \s+        # Any white space char, including \n
    | \#[^\n]*   # Perl like comments
    |  /\*.*?\*/ # C like comments
  )+
}xs;

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
        ?   $$input=~m{\G($HEADERWHITESPACES)}gc
        :   $$input=~m{\G($BODYWHITESPACES)}gc
    and do {
        my($blanks)=$1;

        #Maybe At EOF
        pos($$input) >= length($$input) and return('',[ undef, -1 ]);

        $lineno[1]+= $blanks=~tr/\n//;
    };

    $lineno[0]=$lineno[1];

        $$input=~/\G($ID)/gc
    and return('IDENT',[ $1, $lineno[0] ]);


        $$input=~/\G($STRING)/gc
    and do {
        my $string = $1;

        # The string 'error' is reserved for the special token 'error'
        $string eq "'error'" and do {
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

        $$input=~/\G\s*{/gc   # }
    and do {
        return ('CODE', &slurp_perl_code());
    };

    if($lexlevel == 0) {# In head section
            $$input=~/\G%(left|right|nonassoc)/gc
        and return('ASSOC',[ uc($1), $lineno[0] ]);

            $$input=~/\G%{/gc
        and do {
            my($code);

            $$input=~/\G(.*?)%}/sgc or  _SyntaxError(2,"Unmatched %{ opened line $lineno[0]",-1);

            $code=$1;
            $lineno[1]+= $code=~tr/\n//;
            return('HEADCODE',[ $code, $lineno[0] ]);
        };

            $$input=~/\G%prefix\s+([A-Za-z_][A-Za-z0-9_:]*::)/gc
        and return('PREFIX',[ $1, $lineno[0] ]);

            $$input=~/\G%(tree((?:\s+(?:bypass|alias)){0,2}))/gc
        and do {
          my $treeoptions =  defined($2)? $2 : '';
          return('TREE',[ $treeoptions, $lineno[0] ])
        };

            $$input=~/\G%(?:(semantic|syntactic)\s+token)/gc
        and return(uc($1),[ undef, $lineno[0] ]);

            $$input=~/\G%(lexer|defaultaction|union)\s*/gc
        and return(uc($1),[ undef, $lineno[0] ]);

            $$input=~/\G([0-9]+)/gc
        and return('NUMBER',[ $1, $lineno[0] ]);

            $$input=~/\G%expect-rr/gc
        and do {
           return('EXPECTRR',[ undef, $lineno[0] ]);
        };

            $$input=~/\G%($ID)/gc
        and do {
           return($headertoken{$1},[ undef, $lineno[0] ]);
        };
    }
    else {  # In rule section

            # like in <%name LIST_of_STH *>
            # like in <%name LIST_of_STH +>
            # like in <%name OPT_STH ?>
            $$input=~/\G(?:<\s*%name\s*($ID)\s*)?([*+?])\s*>/gc
        and return($listtoken{$2},[ $1, $lineno[0] ]);

            # like in %name LIST_of_STH *
            # like in %name LIST_of_STH +
            # like in %name OPT_STH ?
            $$input=~/\G(?:%name\s*($ID)\s*)?([*+?])/gc
        and return($listtoken{$2},[ $1, $lineno[0] ]);

            $$input=~/\G%no\s+bypass/gc
        and do {
          #my $bp = defined($1)?0:1; 
          return('NAME',[ 0, $lineno[0] ]);
        };

            $$input=~/\G%(prec)/gc
        and return('PREC',[ undef, $lineno[0] ]);

            $$input=~/\G%(PREC)/gc
        and return('DPREC',[ undef, $lineno[0] ]);

            $$input=~/\G%name/gc
        and do {
          # return current bypass status
          return('NAME',[ $bypass, $lineno[0] ]);
        };

            $$input=~/\G($LABEL)/gc
        and return('LABEL',[ $1, $lineno[0] ]);

            $$input=~/\G%begin\s*{/gc  # }
        and return ('BEGINCODE', &slurp_perl_code());

    }

    #Always return something
        $$input=~/\G(.)/sg
    or  die "Parse::Eyapp::Grammar::Parse: Match (.) failed: report as a BUG";

    my $char = $1;

    $char =~ s/\cM/\n/; # dos to unix

    $char eq "\n" and ++$lineno[1];

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

    $head=[];
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
