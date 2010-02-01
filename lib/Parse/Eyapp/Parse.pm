########################################################################################
#
#    This file was generated using Parse::Eyapp version 1.155.
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
  

# Default lexical analyzer
our $LEX = sub {
    my $self = shift;

    for (${$self->input}) {
      m{\G(\s+)}gc and $self->tokenline($1 =~ tr{\n}{});;

      m{\G(DEFAULTACTION|NAMINGSCHEME|SYNTACTIC|BEGINCODE|NOCOMPACT|CONFLICT|SEMANTIC|METATREE|TAILCODE|EXPECTRR|HEADCODE|LITERAL|WHITES|OPTION|EXPECT|PREFIX|REGEXP|STRICT|NUMBER|LEXER|IDENT|START|LABEL|UNION|ASSOC|DPREC|TOKEN|PLUS|PREC|TREE|STAR|NAME|TYPE|CODE|\%\%|\n|\:|\<|\$|\||\(|\.|\>|\;|\)|\=)}gc and return ($1, $1);



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
my $term;    # hash ref. key: token, value: an array describing the assoc and priority { '-' => [ 'LEFT' 1 ], '*' => [ 'LEFT' 2 ], }
my $termdef; # token definitions. key is token, value is regexp
my $whites;  # string with the code for white spaces (when automatic generated lexer)
my $lexer;   # boolean: true if %lexer was used
my $nterm;
my $rules;
my $precterm; # hash ref. key token used in %prec. value: priority
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
my %conflict;    # Hash of conflict name => { codeh => 'code handler', line => #line, #prodnumber1 => [pos1, pos2], #prodnumber2 => [pos1,pos2,pos3], ... }

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

      if ($what eq 'CODE') { # TODO: modify name scheme: RULE_POSITION
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
      }
      elsif ($what eq 'CONFLICTHANDLER') {
        my $ch = $value->[0];
        push @{$conflict{$ch}{production}{-$position}}, $s; 
      }
      
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
    my $self = $_[0]; 
    for (${$self->input()}) {  # contextualize
#line <<line>> "<<filename>>"
      <<code>>       
<<end_user_code>>
      return ('', undef) if ($_ eq '') || (defined(pos($_)) && (pos($_) >= length($_)));
      die("Error inside the lexical analyzer. Line: <<errline>>. File: <<filename>>. No regexp matched.\n");
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
    yyversion => '1.155',
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
  [ 'decl_11' => 'decl', [ 'SEMANTIC', 'typedecl', 'toklist', '\n' ], 0 ],
  [ 'decl_12' => 'decl', [ 'SYNTACTIC', 'typedecl', 'toklist', '\n' ], 0 ],
  [ 'decl_13' => 'decl', [ 'TOKEN', 'typedecl', 'toklist', '\n' ], 0 ],
  [ 'decl_14' => 'decl', [ 'ASSOC', 'typedecl', 'symlist', '\n' ], 0 ],
  [ 'decl_15' => 'decl', [ 'START', 'ident', '\n' ], 0 ],
  [ 'decl_16' => 'decl', [ 'PREFIX', '\n' ], 0 ],
  [ 'decl_17' => 'decl', [ 'WHITES', 'CODE', '\n' ], 0 ],
  [ 'decl_18' => 'decl', [ 'WHITES', 'REGEXP', '\n' ], 0 ],
  [ 'decl_19' => 'decl', [ 'WHITES', '=', 'CODE', '\n' ], 0 ],
  [ 'decl_20' => 'decl', [ 'WHITES', '=', 'REGEXP', '\n' ], 0 ],
  [ 'decl_21' => 'decl', [ 'NAMINGSCHEME', 'CODE', '\n' ], 0 ],
  [ 'decl_22' => 'decl', [ 'HEADCODE', '\n' ], 0 ],
  [ 'decl_23' => 'decl', [ 'UNION', 'CODE', '\n' ], 0 ],
  [ 'decl_24' => 'decl', [ 'DEFAULTACTION', 'CODE', '\n' ], 0 ],
  [ 'decl_25' => 'decl', [ 'LEXER', 'CODE', '\n' ], 0 ],
  [ 'decl_26' => 'decl', [ 'TREE', '\n' ], 0 ],
  [ 'decl_27' => 'decl', [ 'METATREE', '\n' ], 0 ],
  [ 'decl_28' => 'decl', [ 'STRICT', '\n' ], 0 ],
  [ 'decl_29' => 'decl', [ 'NOCOMPACT', '\n' ], 0 ],
  [ 'decl_30' => 'decl', [ 'TYPE', 'typedecl', 'identlist', '\n' ], 0 ],
  [ 'decl_31' => 'decl', [ 'CONFLICT', 'ident', 'CODE', '\n' ], 0 ],
  [ 'decl_32' => 'decl', [ 'EXPECT', 'NUMBER', '\n' ], 0 ],
  [ 'decl_33' => 'decl', [ 'EXPECT', 'NUMBER', 'NUMBER', '\n' ], 0 ],
  [ 'decl_34' => 'decl', [ 'EXPECTRR', 'NUMBER', '\n' ], 0 ],
  [ 'decl_35' => 'decl', [ 'error', '\n' ], 0 ],
  [ 'typedecl_36' => 'typedecl', [  ], 0 ],
  [ 'typedecl_37' => 'typedecl', [ '<', 'IDENT', '>' ], 0 ],
  [ 'symlist_38' => 'symlist', [ 'symlist', 'symbol' ], 0 ],
  [ 'symlist_39' => 'symlist', [ 'symbol' ], 0 ],
  [ 'toklist_40' => 'toklist', [ 'toklist', 'tokendef' ], 0 ],
  [ 'toklist_41' => 'toklist', [ 'tokendef' ], 0 ],
  [ 'tokendef_42' => 'tokendef', [ 'symbol', '=', 'REGEXP' ], 0 ],
  [ 'tokendef_43' => 'tokendef', [ 'symbol', '=', 'CODE' ], 0 ],
  [ 'tokendef_44' => 'tokendef', [ 'symbol' ], 0 ],
  [ 'identlist_45' => 'identlist', [ 'identlist', 'ident' ], 0 ],
  [ 'identlist_46' => 'identlist', [ 'ident' ], 0 ],
  [ 'body_47' => 'body', [ 'rulesec', '%%' ], 0 ],
  [ 'body_48' => 'body', [ '%%' ], 0 ],
  [ 'rulesec_49' => 'rulesec', [ 'rulesec', 'rules' ], 0 ],
  [ 'rulesec_50' => 'rulesec', [ 'startrules' ], 0 ],
  [ 'startrules_51' => 'startrules', [ 'IDENT', ':', '@51-2', 'rhss', ';' ], 0 ],
  [ '_CODE' => '@51-2', [  ], 0 ],
  [ 'startrules_53' => 'startrules', [ 'error', ';' ], 0 ],
  [ 'rules_54' => 'rules', [ 'IDENT', ':', 'rhss', ';' ], 0 ],
  [ 'rules_55' => 'rules', [ 'error', ';' ], 0 ],
  [ 'rhss_56' => 'rhss', [ 'rhss', '|', 'rule' ], 0 ],
  [ 'rhss_57' => 'rhss', [ 'rule' ], 0 ],
  [ 'rule_58' => 'rule', [ 'optname', 'rhs', 'prec', 'epscode' ], 0 ],
  [ 'rule_59' => 'rule', [ 'optname', 'rhs' ], 0 ],
  [ 'rhs_60' => 'rhs', [  ], 0 ],
  [ 'rhs_61' => 'rhs', [ 'rhselts' ], 0 ],
  [ 'rhselts_62' => 'rhselts', [ 'rhselts', 'rhseltwithid' ], 0 ],
  [ 'rhselts_63' => 'rhselts', [ 'rhseltwithid' ], 0 ],
  [ 'rhseltwithid_64' => 'rhseltwithid', [ 'rhselt', '.', 'IDENT' ], 0 ],
  [ 'rhseltwithid_65' => 'rhseltwithid', [ '$', 'rhselt' ], 0 ],
  [ 'rhseltwithid_66' => 'rhseltwithid', [ '$', 'error' ], 0 ],
  [ 'rhseltwithid_67' => 'rhseltwithid', [ 'rhselt' ], 0 ],
  [ 'rhselt_68' => 'rhselt', [ 'symbol' ], 0 ],
  [ 'rhselt_69' => 'rhselt', [ 'code' ], 0 ],
  [ 'rhselt_70' => 'rhselt', [ 'DPREC', 'ident' ], 0 ],
  [ 'rhselt_71' => 'rhselt', [ '(', 'optname', 'rhs', ')' ], 0 ],
  [ 'rhselt_72' => 'rhselt', [ 'rhselt', 'STAR' ], 0 ],
  [ 'rhselt_73' => 'rhselt', [ 'rhselt', '<', 'STAR', 'symbol', '>' ], 0 ],
  [ 'rhselt_74' => 'rhselt', [ 'rhselt', 'OPTION' ], 0 ],
  [ 'rhselt_75' => 'rhselt', [ 'rhselt', '<', 'PLUS', 'symbol', '>' ], 0 ],
  [ 'rhselt_76' => 'rhselt', [ 'rhselt', 'PLUS' ], 0 ],
  [ 'optname_77' => 'optname', [  ], 0 ],
  [ 'optname_78' => 'optname', [ 'NAME', 'IDENT' ], 0 ],
  [ 'optname_79' => 'optname', [ 'NAME', 'IDENT', 'LABEL' ], 0 ],
  [ 'optname_80' => 'optname', [ 'NAME', 'LABEL' ], 0 ],
  [ 'prec_81' => 'prec', [ 'PREC', 'symbol' ], 0 ],
  [ 'epscode_82' => 'epscode', [  ], 0 ],
  [ 'epscode_83' => 'epscode', [ 'code' ], 0 ],
  [ 'code_84' => 'code', [ 'CODE' ], 0 ],
  [ 'code_85' => 'code', [ 'BEGINCODE' ], 0 ],
  [ 'tail_86' => 'tail', [  ], 0 ],
  [ 'tail_87' => 'tail', [ 'TAILCODE' ], 0 ],
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
	'=' => { ISSEMANTIC => 0 },
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
	REGEXP => { ISSEMANTIC => 1 },
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
	WHITES => { ISSEMANTIC => 1 },
	error => { ISSEMANTIC => 1 },
	error => { ISSEMANTIC => 0 },
},
    yyFILENAME  => 'lib/Parse/Eyapp/Parse.yp',
    yystates =>
[
	{#State 0
		ACTIONS => {
			'SEMANTIC' => 1,
			'WHITES' => 2,
			'LEXER' => 3,
			'UNION' => 6,
			'START' => 7,
			'NAMINGSCHEME' => 9,
			'error' => 10,
			'DEFAULTACTION' => 11,
			'ASSOC' => 12,
			'CONFLICT' => 13,
			'TREE' => 14,
			'NOCOMPACT' => 16,
			"%%" => -6,
			'EXPECT' => 17,
			'METATREE' => 18,
			"\n" => 19,
			'SYNTACTIC' => 20,
			'TYPE' => 21,
			'PREFIX' => 23,
			'STRICT' => 24,
			'TOKEN' => 25,
			'EXPECTRR' => 26,
			'HEADCODE' => 27
		},
		GOTOS => {
			'head' => 15,
			'decl' => 8,
			'headsec' => 4,
			'decls' => 22,
			'eyapp' => 5
		}
	},
	{#State 1
		ACTIONS => {
			"<" => 28
		},
		DEFAULT => -36,
		GOTOS => {
			'typedecl' => 29
		}
	},
	{#State 2
		ACTIONS => {
			'REGEXP' => 32,
			'CODE' => 30,
			"=" => 31
		}
	},
	{#State 3
		ACTIONS => {
			'CODE' => 33
		}
	},
	{#State 4
		ACTIONS => {
			"%%" => 34
		}
	},
	{#State 5
		ACTIONS => {
			'' => 35
		}
	},
	{#State 6
		ACTIONS => {
			'CODE' => 36
		}
	},
	{#State 7
		ACTIONS => {
			'IDENT' => 37
		},
		GOTOS => {
			'ident' => 38
		}
	},
	{#State 8
		DEFAULT => -9
	},
	{#State 9
		ACTIONS => {
			'CODE' => 39
		}
	},
	{#State 10
		ACTIONS => {
			"\n" => 40
		}
	},
	{#State 11
		ACTIONS => {
			'CODE' => 41
		}
	},
	{#State 12
		ACTIONS => {
			"<" => 28
		},
		DEFAULT => -36,
		GOTOS => {
			'typedecl' => 42
		}
	},
	{#State 13
		ACTIONS => {
			'IDENT' => 37
		},
		GOTOS => {
			'ident' => 43
		}
	},
	{#State 14
		ACTIONS => {
			"\n" => 44
		}
	},
	{#State 15
		ACTIONS => {
			"%%" => 49,
			'error' => 47,
			'IDENT' => 45
		},
		GOTOS => {
			'body' => 46,
			'rulesec' => 50,
			'startrules' => 48
		}
	},
	{#State 16
		ACTIONS => {
			"\n" => 51
		}
	},
	{#State 17
		ACTIONS => {
			'NUMBER' => 52
		}
	},
	{#State 18
		ACTIONS => {
			"\n" => 53
		}
	},
	{#State 19
		DEFAULT => -10
	},
	{#State 20
		ACTIONS => {
			"<" => 28
		},
		DEFAULT => -36,
		GOTOS => {
			'typedecl' => 54
		}
	},
	{#State 21
		ACTIONS => {
			"<" => 28
		},
		DEFAULT => -36,
		GOTOS => {
			'typedecl' => 55
		}
	},
	{#State 22
		ACTIONS => {
			'SEMANTIC' => 1,
			'WHITES' => 2,
			'LEXER' => 3,
			'UNION' => 6,
			'START' => 7,
			'NAMINGSCHEME' => 9,
			'error' => 10,
			'DEFAULTACTION' => 11,
			'ASSOC' => 12,
			'CONFLICT' => 13,
			'TREE' => 14,
			'NOCOMPACT' => 16,
			"%%" => -7,
			'EXPECT' => 17,
			'METATREE' => 18,
			"\n" => 19,
			'SYNTACTIC' => 20,
			'TYPE' => 21,
			'PREFIX' => 23,
			'STRICT' => 24,
			'TOKEN' => 25,
			'EXPECTRR' => 26,
			'HEADCODE' => 27
		},
		GOTOS => {
			'decl' => 56
		}
	},
	{#State 23
		ACTIONS => {
			"\n" => 57
		}
	},
	{#State 24
		ACTIONS => {
			"\n" => 58
		}
	},
	{#State 25
		ACTIONS => {
			"<" => 28
		},
		DEFAULT => -36,
		GOTOS => {
			'typedecl' => 59
		}
	},
	{#State 26
		ACTIONS => {
			'NUMBER' => 60
		}
	},
	{#State 27
		ACTIONS => {
			"\n" => 61
		}
	},
	{#State 28
		ACTIONS => {
			'IDENT' => 62
		}
	},
	{#State 29
		ACTIONS => {
			'LITERAL' => 64,
			'IDENT' => 37
		},
		GOTOS => {
			'tokendef' => 63,
			'toklist' => 66,
			'symbol' => 65,
			'ident' => 67
		}
	},
	{#State 30
		ACTIONS => {
			"\n" => 68
		}
	},
	{#State 31
		ACTIONS => {
			'REGEXP' => 70,
			'CODE' => 69
		}
	},
	{#State 32
		ACTIONS => {
			"\n" => 71
		}
	},
	{#State 33
		ACTIONS => {
			"\n" => 72
		}
	},
	{#State 34
		DEFAULT => -5
	},
	{#State 35
		DEFAULT => 0
	},
	{#State 36
		ACTIONS => {
			"\n" => 73
		}
	},
	{#State 37
		DEFAULT => -4
	},
	{#State 38
		ACTIONS => {
			"\n" => 74
		}
	},
	{#State 39
		ACTIONS => {
			"\n" => 75
		}
	},
	{#State 40
		DEFAULT => -35
	},
	{#State 41
		ACTIONS => {
			"\n" => 76
		}
	},
	{#State 42
		ACTIONS => {
			'LITERAL' => 64,
			'IDENT' => 37
		},
		GOTOS => {
			'symlist' => 78,
			'symbol' => 77,
			'ident' => 67
		}
	},
	{#State 43
		ACTIONS => {
			'CODE' => 79
		}
	},
	{#State 44
		DEFAULT => -26
	},
	{#State 45
		ACTIONS => {
			":" => 80
		}
	},
	{#State 46
		ACTIONS => {
			'TAILCODE' => 82
		},
		DEFAULT => -86,
		GOTOS => {
			'tail' => 81
		}
	},
	{#State 47
		ACTIONS => {
			";" => 83
		}
	},
	{#State 48
		DEFAULT => -50
	},
	{#State 49
		DEFAULT => -48
	},
	{#State 50
		ACTIONS => {
			"%%" => 87,
			'error' => 86,
			'IDENT' => 84
		},
		GOTOS => {
			'rules' => 85
		}
	},
	{#State 51
		DEFAULT => -29
	},
	{#State 52
		ACTIONS => {
			"\n" => 89,
			'NUMBER' => 88
		}
	},
	{#State 53
		DEFAULT => -27
	},
	{#State 54
		ACTIONS => {
			'LITERAL' => 64,
			'IDENT' => 37
		},
		GOTOS => {
			'tokendef' => 63,
			'toklist' => 90,
			'symbol' => 65,
			'ident' => 67
		}
	},
	{#State 55
		ACTIONS => {
			'IDENT' => 37
		},
		GOTOS => {
			'identlist' => 91,
			'ident' => 92
		}
	},
	{#State 56
		DEFAULT => -8
	},
	{#State 57
		DEFAULT => -16
	},
	{#State 58
		DEFAULT => -28
	},
	{#State 59
		ACTIONS => {
			'LITERAL' => 64,
			'IDENT' => 37
		},
		GOTOS => {
			'tokendef' => 63,
			'toklist' => 93,
			'symbol' => 65,
			'ident' => 67
		}
	},
	{#State 60
		ACTIONS => {
			"\n" => 94
		}
	},
	{#State 61
		DEFAULT => -22
	},
	{#State 62
		ACTIONS => {
			">" => 95
		}
	},
	{#State 63
		DEFAULT => -41
	},
	{#State 64
		DEFAULT => -2
	},
	{#State 65
		ACTIONS => {
			"=" => 96
		},
		DEFAULT => -44
	},
	{#State 66
		ACTIONS => {
			"\n" => 98,
			'LITERAL' => 64,
			'IDENT' => 37
		},
		GOTOS => {
			'tokendef' => 97,
			'symbol' => 65,
			'ident' => 67
		}
	},
	{#State 67
		DEFAULT => -3
	},
	{#State 68
		DEFAULT => -17
	},
	{#State 69
		ACTIONS => {
			"\n" => 99
		}
	},
	{#State 70
		ACTIONS => {
			"\n" => 100
		}
	},
	{#State 71
		DEFAULT => -18
	},
	{#State 72
		DEFAULT => -25
	},
	{#State 73
		DEFAULT => -23
	},
	{#State 74
		DEFAULT => -15
	},
	{#State 75
		DEFAULT => -21
	},
	{#State 76
		DEFAULT => -24
	},
	{#State 77
		DEFAULT => -39
	},
	{#State 78
		ACTIONS => {
			"\n" => 102,
			'LITERAL' => 64,
			'IDENT' => 37
		},
		GOTOS => {
			'symbol' => 101,
			'ident' => 67
		}
	},
	{#State 79
		ACTIONS => {
			"\n" => 103
		}
	},
	{#State 80
		DEFAULT => -52,
		GOTOS => {
			'@51-2' => 104
		}
	},
	{#State 81
		DEFAULT => -1
	},
	{#State 82
		DEFAULT => -87
	},
	{#State 83
		DEFAULT => -53
	},
	{#State 84
		ACTIONS => {
			":" => 105
		}
	},
	{#State 85
		DEFAULT => -49
	},
	{#State 86
		ACTIONS => {
			";" => 106
		}
	},
	{#State 87
		DEFAULT => -47
	},
	{#State 88
		ACTIONS => {
			"\n" => 107
		}
	},
	{#State 89
		DEFAULT => -32
	},
	{#State 90
		ACTIONS => {
			"\n" => 108,
			'LITERAL' => 64,
			'IDENT' => 37
		},
		GOTOS => {
			'tokendef' => 97,
			'symbol' => 65,
			'ident' => 67
		}
	},
	{#State 91
		ACTIONS => {
			"\n" => 109,
			'IDENT' => 37
		},
		GOTOS => {
			'ident' => 110
		}
	},
	{#State 92
		DEFAULT => -46
	},
	{#State 93
		ACTIONS => {
			"\n" => 111,
			'LITERAL' => 64,
			'IDENT' => 37
		},
		GOTOS => {
			'tokendef' => 97,
			'symbol' => 65,
			'ident' => 67
		}
	},
	{#State 94
		DEFAULT => -34
	},
	{#State 95
		DEFAULT => -37
	},
	{#State 96
		ACTIONS => {
			'REGEXP' => 113,
			'CODE' => 112
		}
	},
	{#State 97
		DEFAULT => -40
	},
	{#State 98
		DEFAULT => -11
	},
	{#State 99
		DEFAULT => -19
	},
	{#State 100
		DEFAULT => -20
	},
	{#State 101
		DEFAULT => -38
	},
	{#State 102
		DEFAULT => -14
	},
	{#State 103
		DEFAULT => -31
	},
	{#State 104
		ACTIONS => {
			'NAME' => 117
		},
		DEFAULT => -77,
		GOTOS => {
			'rule' => 114,
			'rhss' => 116,
			'optname' => 115
		}
	},
	{#State 105
		ACTIONS => {
			'NAME' => 117
		},
		DEFAULT => -77,
		GOTOS => {
			'rule' => 114,
			'rhss' => 118,
			'optname' => 115
		}
	},
	{#State 106
		DEFAULT => -55
	},
	{#State 107
		DEFAULT => -33
	},
	{#State 108
		DEFAULT => -12
	},
	{#State 109
		DEFAULT => -30
	},
	{#State 110
		DEFAULT => -45
	},
	{#State 111
		DEFAULT => -13
	},
	{#State 112
		DEFAULT => -43
	},
	{#State 113
		DEFAULT => -42
	},
	{#State 114
		DEFAULT => -57
	},
	{#State 115
		ACTIONS => {
			'CODE' => 127,
			'LITERAL' => 64,
			'IDENT' => 37,
			'BEGINCODE' => 120,
			"(" => 128,
			'DPREC' => 129,
			"\$" => 122
		},
		DEFAULT => -60,
		GOTOS => {
			'symbol' => 126,
			'rhselt' => 121,
			'rhs' => 119,
			'rhselts' => 124,
			'rhseltwithid' => 123,
			'ident' => 67,
			'code' => 125
		}
	},
	{#State 116
		ACTIONS => {
			"|" => 131,
			";" => 130
		}
	},
	{#State 117
		ACTIONS => {
			'LABEL' => 132,
			'IDENT' => 133
		}
	},
	{#State 118
		ACTIONS => {
			"|" => 131,
			";" => 134
		}
	},
	{#State 119
		ACTIONS => {
			'PREC' => 135
		},
		DEFAULT => -59,
		GOTOS => {
			'prec' => 136
		}
	},
	{#State 120
		DEFAULT => -85
	},
	{#State 121
		ACTIONS => {
			"<" => 137,
			'PLUS' => 138,
			'OPTION' => 139,
			'STAR' => 140,
			"." => 141
		},
		DEFAULT => -67
	},
	{#State 122
		ACTIONS => {
			"(" => 128,
			'DPREC' => 129,
			'error' => 143,
			'CODE' => 127,
			'LITERAL' => 64,
			'IDENT' => 37,
			'BEGINCODE' => 120
		},
		GOTOS => {
			'symbol' => 126,
			'rhselt' => 142,
			'ident' => 67,
			'code' => 125
		}
	},
	{#State 123
		DEFAULT => -63
	},
	{#State 124
		ACTIONS => {
			'CODE' => 127,
			'LITERAL' => 64,
			'IDENT' => 37,
			'BEGINCODE' => 120,
			"(" => 128,
			'DPREC' => 129,
			"\$" => 122
		},
		DEFAULT => -61,
		GOTOS => {
			'symbol' => 126,
			'rhselt' => 121,
			'rhseltwithid' => 144,
			'ident' => 67,
			'code' => 125
		}
	},
	{#State 125
		DEFAULT => -69
	},
	{#State 126
		DEFAULT => -68
	},
	{#State 127
		DEFAULT => -84
	},
	{#State 128
		ACTIONS => {
			'NAME' => 117
		},
		DEFAULT => -77,
		GOTOS => {
			'optname' => 145
		}
	},
	{#State 129
		ACTIONS => {
			'IDENT' => 37
		},
		GOTOS => {
			'ident' => 146
		}
	},
	{#State 130
		DEFAULT => -51
	},
	{#State 131
		ACTIONS => {
			'NAME' => 117
		},
		DEFAULT => -77,
		GOTOS => {
			'rule' => 147,
			'optname' => 115
		}
	},
	{#State 132
		DEFAULT => -80
	},
	{#State 133
		ACTIONS => {
			'LABEL' => 148
		},
		DEFAULT => -78
	},
	{#State 134
		DEFAULT => -54
	},
	{#State 135
		ACTIONS => {
			'LITERAL' => 64,
			'IDENT' => 37
		},
		GOTOS => {
			'symbol' => 149,
			'ident' => 67
		}
	},
	{#State 136
		ACTIONS => {
			'CODE' => 127,
			'BEGINCODE' => 120
		},
		DEFAULT => -82,
		GOTOS => {
			'epscode' => 151,
			'code' => 150
		}
	},
	{#State 137
		ACTIONS => {
			'STAR' => 153,
			'PLUS' => 152
		}
	},
	{#State 138
		DEFAULT => -76
	},
	{#State 139
		DEFAULT => -74
	},
	{#State 140
		DEFAULT => -72
	},
	{#State 141
		ACTIONS => {
			'IDENT' => 154
		}
	},
	{#State 142
		ACTIONS => {
			'OPTION' => 139,
			"<" => 137,
			'PLUS' => 138,
			'STAR' => 140
		},
		DEFAULT => -65
	},
	{#State 143
		DEFAULT => -66
	},
	{#State 144
		DEFAULT => -62
	},
	{#State 145
		ACTIONS => {
			"(" => 128,
			'DPREC' => 129,
			"\$" => 122,
			'CODE' => 127,
			'LITERAL' => 64,
			'IDENT' => 37,
			'BEGINCODE' => 120
		},
		DEFAULT => -60,
		GOTOS => {
			'symbol' => 126,
			'rhselt' => 121,
			'rhs' => 155,
			'rhselts' => 124,
			'rhseltwithid' => 123,
			'ident' => 67,
			'code' => 125
		}
	},
	{#State 146
		DEFAULT => -70
	},
	{#State 147
		DEFAULT => -56
	},
	{#State 148
		DEFAULT => -79
	},
	{#State 149
		DEFAULT => -81
	},
	{#State 150
		DEFAULT => -83
	},
	{#State 151
		DEFAULT => -58
	},
	{#State 152
		ACTIONS => {
			'LITERAL' => 64,
			'IDENT' => 37
		},
		GOTOS => {
			'symbol' => 156,
			'ident' => 67
		}
	},
	{#State 153
		ACTIONS => {
			'LITERAL' => 64,
			'IDENT' => 37
		},
		GOTOS => {
			'symbol' => 157,
			'ident' => 67
		}
	},
	{#State 154
		DEFAULT => -64
	},
	{#State 155
		ACTIONS => {
			")" => 158
		}
	},
	{#State 156
		ACTIONS => {
			">" => 159
		}
	},
	{#State 157
		ACTIONS => {
			">" => 160
		}
	},
	{#State 158
		DEFAULT => -71
	},
	{#State 159
		DEFAULT => -75
	},
	{#State 160
		DEFAULT => -73
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
                    my($symbol,$lineno, $def)=@$_;

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
                    my($symbol,$lineno, $def)=@$_;

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
                    $$termdef{$symbol} = $def if $def;
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
              push @{$_[2]}, 'CODE';
              $whites = $_[2]; 
            }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_18
		 'decl', 3,
sub { 
              push @{$_[2]}, 'REGEXP';
              $whites = $_[2]; 
            }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_19
		 'decl', 4,
sub { 
              push @{$_[3]}, 'CODE';
              $whites = $_[3]; 
            }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_20
		 'decl', 4,
sub { 
              push @{$_[3]}, 'REGEXP';
              $whites = $_[3]; 
            }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_21
		 'decl', 3,
sub { 
              $namingscheme = $_[2];
              undef  
            }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_22
		 'decl', 2,
sub {  push(@$head,$_[1]); undef }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_23
		 'decl', 3,
sub {  undef }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_24
		 'decl', 3,
sub {  $defaultaction = $_[2]; undef }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_25
		 'decl', 3,
sub {  
                                           my ($code, $line) = @{$_[2]};
                                           push @$head, [ make_lexer($code, $line), $line]; 
                                           $lexer = 1;
                                           undef 
                                         }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_26
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
	[#Rule decl_27
		 'decl', 2,
sub {  
            $metatree = $tree = $buildingtree = 1;
            undef 
          }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_28
		 'decl', 2,
sub {  
            $strict = 1;
            undef 
          }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_29
		 'decl', 2,
sub {  
            $nocompact = 1;
            undef 
          }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_30
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
	[#Rule decl_31
		 'decl', 4,
sub { 
              my ($name, $code) = @_[2,3];
              my ($cn, $line) = @$name;


              my ($c, $li) = @$code;

              # TODO: this must be in Output
              my $conflict_header = <<"CONFLICT_HEADER";
  my \$self = \$_[0];
  for (\${\$self->input()}) {  
#line $li "$filename" 
CONFLICT_HEADER
              $c =~ s/^/$conflict_header/; # }

              # {
              # follows the closing curly bracket of the for .. to contextualize!!!!!!                 v
              $c =~ s/$/\n################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################\n  }\n/;
              #$code->[0] = $c;
              $conflict{$cn}{codeh} = $c;
              $conflict{$cn}{line} = $line;

              $$syms{$cn} = $line;
              $$nterm{$cn} = undef;

              undef;
            }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_32
		 'decl', 3,
sub {  $expect=$_[2][0]; undef }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_33
		 'decl', 4,
sub {  $expect= [ $_[2][0], $_[3][0]]; undef }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_34
		 'decl', 3,
sub {  
                                          $expect = 0 unless defined($expect);
                                          croak "Number of reduce-reduce conflicts is redefined (line $_[2][1], file: $filename)\n" if ref($expect);
                                          $expect= [ $expect, $_[2][0]]; 
                                          undef 
                                        }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule decl_35
		 'decl', 2,
sub {  $_[0]->YYErrok }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule typedecl_36
		 'typedecl', 0, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule typedecl_37
		 'typedecl', 3, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule symlist_38
		 'symlist', 2,
sub {  push(@{$_[1]},$_[2]); $_[1] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule symlist_39
		 'symlist', 1,
sub {  [ $_[1] ] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule toklist_40
		 'toklist', 2,
sub {  push(@{$_[1]},$_[2]); $_[1] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule toklist_41
		 'toklist', 1,
sub {  [ $_[1] ] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule tokendef_42
		 'tokendef', 3,
sub {  
                                    push @{$_[3]}, 'REGEXP';
                                    push @{$_[1]}, $_[3]; 
                                    $_[1] 
                                 }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule tokendef_43
		 'tokendef', 3,
sub {  
                                    push @{$_[3]}, 'CODE';
                                    push @{$_[1]}, $_[3]; 
                                    $_[1] 
                                 }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule tokendef_44
		 'tokendef', 1, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule identlist_45
		 'identlist', 2,
sub {  push(@{$_[1]},$_[2]); $_[1] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule identlist_46
		 'identlist', 1,
sub {  [ $_[1] ] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule body_47
		 'body', 2,
sub { 
                $start
            or  $start=$$rules[1][0];

                ref($$nterm{$start})
            or  _SyntaxError(2,"Start symbol $start not found ".
                                "in rules section",$_[2][1]);

            # Add conflict handlers
            # [ left hand side,   right hand side,  precedence, rulename, code, ]
            for my $A (keys %conflict) { 
              my $lhs = [$A, $conflict{$A}{line}];
              my $code = $conflict{$A}{codeh};
              my $rhss = [ rhs([], name => $lhs, code => $code), ];
              _AddRules($lhs, $rhss);
              delete $conflict{$A}{codeh};
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
	[#Rule body_48
		 'body', 1,
sub {  _SyntaxError(2,"No rules in input grammar",$_[1][1]); }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rulesec_49
		 'rulesec', 2, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rulesec_50
		 'rulesec', 1, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule startrules_51
		 'startrules', 5,
sub {  _AddRules($_[1],$_[4]); undef }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule _CODE
		 '@51-2', 0,
sub {  $start = $_[1][0] unless $start; }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule startrules_53
		 'startrules', 2,
sub {  $_[0]->YYErrok }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rules_54
		 'rules', 4,
sub {  _AddRules($_[1],$_[3]); undef }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rules_55
		 'rules', 2,
sub {  $_[0]->YYErrok }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhss_56
		 'rhss', 3,
sub {  push(@{$_[1]},$_[3]); $_[1] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhss_57
		 'rhss', 1,
sub {  [ $_[1] ] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rule_58
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
	[#Rule rule_59
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
	[#Rule rhs_60
		 'rhs', 0, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhs_61
		 'rhs', 1, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhselts_62
		 'rhselts', 2,
sub {  
                push(@{$_[1]},$_[2]); 
                $_[1] 
              }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhselts_63
		 'rhselts', 1,
sub {  [ $_[1] ] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhseltwithid_64
		 'rhseltwithid', 3,
sub { 
          push @{$_[1][1]}, $_[3][0];
          $_[1]
        }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhseltwithid_65
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
	[#Rule rhseltwithid_66
		 'rhseltwithid', 2,
sub {  _SyntaxError(2,"\$ is allowed for identifiers only",$lineno[0]) }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhseltwithid_67
		 'rhseltwithid', 1, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhselt_68
		 'rhselt', 1,
sub {  [ 'SYMB', $_[1] ] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhselt_69
		 'rhselt', 1,
sub {  [ 'CODE', $_[1] ] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhselt_70
		 'rhselt', 2,
sub {  [ 'CONFLICTHANDLER', $_[2] ] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule rhselt_71
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
	[#Rule rhselt_72
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
	[#Rule rhselt_73
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
	[#Rule rhselt_74
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
	[#Rule rhselt_75
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
	[#Rule rhselt_76
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
	[#Rule optname_77
		 'optname', 0, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule optname_78
		 'optname', 2,
sub {  
                      # save bypass status
           $_[2][2] = $_[1][0];
           $_[2] 
         }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule optname_79
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
	[#Rule optname_80
		 'optname', 2,
sub {   # LABELs are used for dynamic conflict resolution
                      # save bypass status
           $_[2][2] = $_[1][0];
           $_[2] 
         }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule prec_81
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
	[#Rule epscode_82
		 'epscode', 0,
sub {  $defaultaction }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule epscode_83
		 'epscode', 1,
sub {  $_[1] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule code_84
		 'code', 1,
sub {  $_[1] }
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule code_85
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
	[#Rule tail_86
		 'tail', 0, undef
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
	],
	[#Rule tail_87
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
    yyconflicthandlers => {}
,
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
         'decl_32', 
         'decl_33', 
         'decl_34', 
         'decl_35', 
         'typedecl_36', 
         'typedecl_37', 
         'symlist_38', 
         'symlist_39', 
         'toklist_40', 
         'toklist_41', 
         'tokendef_42', 
         'tokendef_43', 
         'tokendef_44', 
         'identlist_45', 
         'identlist_46', 
         'body_47', 
         'body_48', 
         'rulesec_49', 
         'rulesec_50', 
         'startrules_51', 
         '_CODE', 
         'startrules_53', 
         'rules_54', 
         'rules_55', 
         'rhss_56', 
         'rhss_57', 
         'rule_58', 
         'rule_59', 
         'rhs_60', 
         'rhs_61', 
         'rhselts_62', 
         'rhselts_63', 
         'rhseltwithid_64', 
         'rhseltwithid_65', 
         'rhseltwithid_66', 
         'rhseltwithid_67', 
         'rhselt_68', 
         'rhselt_69', 
         'rhselt_70', 
         'rhselt_71', 
         'rhselt_72', 
         'rhselt_73', 
         'rhselt_74', 
         'rhselt_75', 
         'rhselt_76', 
         'optname_77', 
         'optname_78', 
         'optname_79', 
         'optname_80', 
         'prec_81', 
         'epscode_82', 
         'epscode_83', 
         'code_84', 
         'code_85', 
         'tail_86', 
         'tail_87', );
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
  whites    => 'WHITES',
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

my $REGEXP = qr{
   /             # opening slash
   (?:[^/\\]|    # an ordinary character
        \\\\|    # escaped \ i.e. \\
         \\/|    # escaped slash i.e. \/
          \\     # escape i.e. \
  )*?            # non greedy repetitions
  /              # closing slash
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

        $$input=~/\G\s*{/gc and return ('CODE', &slurp_perl_code());  # }

    if($lexlevel == 0) {# In head section

        $$input=~/\G%(left|right|nonassoc)/gc and return('ASSOC',[ uc($1), $lineno[0] ]);

            $$input=~/\G%{/gc
        and do {
            my($code);

            $$input=~/\G(.*?)%}/sgc or  _SyntaxError(2,"Unmatched %{ opened line $lineno[0]",-1);

            $code=$1;
            $lineno[1]+= $code=~tr/\n//;
            return('HEADCODE',[ $code, $lineno[0] ]);
        };

        $$input=~/\G%prefix\s+([A-Za-z_][A-Za-z0-9_:]*::)/gc and return('PREFIX',[ $1, $lineno[0] ]);

            $$input=~/\G%(tree((?:\s+(?:bypass|alias)){0,2}))/gc
        and do {
          my $treeoptions =  defined($2)? $2 : '';
          return('TREE',[ $treeoptions, $lineno[0] ])
        };

        $$input=~/\G%(?:(semantic|syntactic)\s+token)\b/gc and return(uc($1),[ undef, $lineno[0] ]);

        $$input=~/\G%(lexer|defaultaction|union)\b\s*/gc   and return(uc($1),[ undef, $lineno[0] ]);

        $$input=~/\G([0-9]+)/gc   and return('NUMBER',[ $1, $lineno[0] ]);

        $$input=~/\G%expect-rr/gc and return('EXPECTRR',[ undef, $lineno[0] ]);

        $$input=~/\G%($ID)/gc     and return($headertoken{$1},[ undef, $lineno[0] ]);

        $$input=~/\G($REGEXP)/gc  and return('REGEXP',[ $1, $lineno[0] ]);

    }
    else {  # In rule section

            # like in <%name LIST_of_STH *>
            # like in <%name LIST_of_STH +>
            # like in <%name OPT_STH ?>
            # returns STAR or PLUS or OPTION
            $$input=~/\G(?:<\s*%name\s*($ID)\s*)?([*+?])\s*>/gc
        and return($listtoken{$2},[ $1, $lineno[0] ]);

            # like in %name LIST_of_STH *
            # like in %name LIST_of_STH +
            # like in %name OPT_STH ?
            # returns STAR or PLUS or OPTION
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

# This sub is called fro Parse::Eyapp::Grammar::new
#                0        1         2       3    4
# Args: object, input, fistline, filename, tree, nocompact
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

    $tree = $_[3];
    if ($tree) { # factorize!
      $buildingtree = 1;
      $bypass = 0;
      $alias = 0;
      $defaultaction = [ ' goto &Parse::Eyapp::Driver::YYBuildAST ', 0]; 
      $namingscheme = [ '\&give_rhs_name', 0];
    }

    $nocompact = $_[4];

    $nberr=0;
    $prec=0;
    $labelno=0;

    $head=[];
    $tail="";

    $syms={};
    $token={};
    $term={};
    $termdef={};
    $nterm={};
    $rules=[ undef ];   #reserve slot 0 for start rule
    $precterm={};

    $start="";
    $nullable={};
    $expect=0;
    $semantic = {};
    $strict = 0;

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
              'CONFLICTHANDLERS',
              'TERMDEF',
              'WHITES',
              'LEXERISDEFINED',
              'STRICT',
            }
    =       (  $head,  $tail,  $rules,  $nterm,  $term,
               $nullable, $precterm, $syms, $start, $expect, 
               $semantic, $bypass, $accessors, $buildingtree,
               $prefix,
               $namingscheme,
               $nocompact,
               \%conflict,
               $termdef,
               $whites,
               $lexer,
               $strict,
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
    undef($termdef);
    undef($whites);
    undef($nterm);
    undef($rules);
    undef($precterm);

    undef($start);
    undef($nullable);
    undef($expect);
    undef($defaultaction);
    undef($semantic);
    undef($buildingtree);
    undef($strict);

    $parsed
}



=for None

=cut


################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################



1;
