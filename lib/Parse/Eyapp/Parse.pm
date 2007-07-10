####################################################################
#
#    This file was generated using Parse::Yapp version 1.05.
#
#        Don't edit this file, use source file instead.
#
#             ANY CHANGE MADE HERE WILL BE LOST !
#
####################################################################
package Parse::Eyapp::Parse;
use vars qw ( @ISA );
use strict;

@ISA= qw ( Parse::Yapp::Driver );
use Parse::Yapp::Driver;

# (c) Copyright Casiano Rodriguez-Leon 
# Based on the original yapp by Francois Desarmenien 1998-2001
# (c) Parse::Yapp Copyright 1998-2001 Francois Desarmenien, all rights reserved.
# (c) Parse::Eyapp Copyright 2006 Casiano Rodriguez Leon, all rights reserved.

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
my $bypass = 0;
my $alias = 0;
my $accessors = ''; # code generated for named accessors when %tree or %metatree is active

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


sub new {
        my($class)=shift;
        ref($class)
    and $class=ref($class);

    my($self)=$class->SUPER::new( yyversion => '1.05',
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
			'EXPECT' => 12,
			"\n" => 13,
			'METATREE' => 14,
			"%%" => -6,
			'SYNTACTIC' => 15,
			'TYPE' => 16,
			'TOKEN' => 18,
			'HEADCODE' => 19
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
			"<" => 20
		},
		DEFAULT => -24,
		GOTOS => {
			'typedecl' => 21
		}
	},
	{#State 2
		ACTIONS => {
			"%%" => 22
		}
	},
	{#State 3
		ACTIONS => {
			'' => 23
		}
	},
	{#State 4
		ACTIONS => {
			'CODE' => 24
		}
	},
	{#State 5
		ACTIONS => {
			'IDENT' => 25
		},
		GOTOS => {
			'ident' => 26
		}
	},
	{#State 6
		DEFAULT => -9
	},
	{#State 7
		ACTIONS => {
			"\n" => 27
		}
	},
	{#State 8
		ACTIONS => {
			'CODE' => 28
		}
	},
	{#State 9
		ACTIONS => {
			"<" => 20
		},
		DEFAULT => -24,
		GOTOS => {
			'typedecl' => 29
		}
	},
	{#State 10
		ACTIONS => {
			"%%" => 34,
			'error' => 32,
			'IDENT' => 30
		},
		GOTOS => {
			'body' => 31,
			'rulesec' => 35,
			'startrules' => 33
		}
	},
	{#State 11
		ACTIONS => {
			"\n" => 36
		}
	},
	{#State 12
		ACTIONS => {
			'NUMBER' => 37
		}
	},
	{#State 13
		DEFAULT => -10
	},
	{#State 14
		ACTIONS => {
			"\n" => 38
		}
	},
	{#State 15
		ACTIONS => {
			"<" => 20
		},
		DEFAULT => -24,
		GOTOS => {
			'typedecl' => 39
		}
	},
	{#State 16
		ACTIONS => {
			"<" => 20
		},
		DEFAULT => -24,
		GOTOS => {
			'typedecl' => 40
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
			'EXPECT' => 12,
			'METATREE' => 14,
			"\n" => 13,
			"%%" => -7,
			'SYNTACTIC' => 15,
			'TYPE' => 16,
			'TOKEN' => 18,
			'HEADCODE' => 19
		},
		GOTOS => {
			'decl' => 41
		}
	},
	{#State 18
		ACTIONS => {
			"<" => 20
		},
		DEFAULT => -24,
		GOTOS => {
			'typedecl' => 42
		}
	},
	{#State 19
		ACTIONS => {
			"\n" => 43
		}
	},
	{#State 20
		ACTIONS => {
			'IDENT' => 44
		}
	},
	{#State 21
		ACTIONS => {
			'LITERAL' => 45,
			'IDENT' => 25
		},
		GOTOS => {
			'symlist' => 48,
			'symbol' => 46,
			'ident' => 47
		}
	},
	{#State 22
		DEFAULT => -5
	},
	{#State 23
		DEFAULT => 0
	},
	{#State 24
		ACTIONS => {
			"\n" => 49
		}
	},
	{#State 25
		DEFAULT => -4
	},
	{#State 26
		ACTIONS => {
			"\n" => 50
		}
	},
	{#State 27
		DEFAULT => -23
	},
	{#State 28
		ACTIONS => {
			"\n" => 51
		}
	},
	{#State 29
		ACTIONS => {
			'LITERAL' => 45,
			'IDENT' => 25
		},
		GOTOS => {
			'symlist' => 52,
			'symbol' => 46,
			'ident' => 47
		}
	},
	{#State 30
		ACTIONS => {
			":" => 53
		}
	},
	{#State 31
		ACTIONS => {
			'TAILCODE' => 55
		},
		DEFAULT => -66,
		GOTOS => {
			'tail' => 54
		}
	},
	{#State 32
		ACTIONS => {
			";" => 56
		}
	},
	{#State 33
		DEFAULT => -33
	},
	{#State 34
		DEFAULT => -31
	},
	{#State 35
		ACTIONS => {
			"%%" => 60,
			'error' => 59,
			'IDENT' => 57
		},
		GOTOS => {
			'rules' => 58
		}
	},
	{#State 36
		DEFAULT => -19
	},
	{#State 37
		ACTIONS => {
			"\n" => 61
		}
	},
	{#State 38
		DEFAULT => -20
	},
	{#State 39
		ACTIONS => {
			'LITERAL' => 45,
			'IDENT' => 25
		},
		GOTOS => {
			'symlist' => 62,
			'symbol' => 46,
			'ident' => 47
		}
	},
	{#State 40
		ACTIONS => {
			'IDENT' => 25
		},
		GOTOS => {
			'identlist' => 63,
			'ident' => 64
		}
	},
	{#State 41
		DEFAULT => -8
	},
	{#State 42
		ACTIONS => {
			'LITERAL' => 45,
			'IDENT' => 25
		},
		GOTOS => {
			'symlist' => 65,
			'symbol' => 46,
			'ident' => 47
		}
	},
	{#State 43
		DEFAULT => -16
	},
	{#State 44
		ACTIONS => {
			">" => 66
		}
	},
	{#State 45
		DEFAULT => -2
	},
	{#State 46
		DEFAULT => -27
	},
	{#State 47
		DEFAULT => -3
	},
	{#State 48
		ACTIONS => {
			"\n" => 68,
			'LITERAL' => 45,
			'IDENT' => 25
		},
		GOTOS => {
			'symbol' => 67,
			'ident' => 47
		}
	},
	{#State 49
		DEFAULT => -17
	},
	{#State 50
		DEFAULT => -15
	},
	{#State 51
		DEFAULT => -18
	},
	{#State 52
		ACTIONS => {
			"\n" => 69,
			'LITERAL' => 45,
			'IDENT' => 25
		},
		GOTOS => {
			'symbol' => 67,
			'ident' => 47
		}
	},
	{#State 53
		DEFAULT => -34,
		GOTOS => {
			'@1-2' => 70
		}
	},
	{#State 54
		DEFAULT => -1
	},
	{#State 55
		DEFAULT => -67
	},
	{#State 56
		DEFAULT => -36
	},
	{#State 57
		ACTIONS => {
			":" => 71
		}
	},
	{#State 58
		DEFAULT => -32
	},
	{#State 59
		ACTIONS => {
			";" => 72
		}
	},
	{#State 60
		DEFAULT => -30
	},
	{#State 61
		DEFAULT => -22
	},
	{#State 62
		ACTIONS => {
			"\n" => 73,
			'LITERAL' => 45,
			'IDENT' => 25
		},
		GOTOS => {
			'symbol' => 67,
			'ident' => 47
		}
	},
	{#State 63
		ACTIONS => {
			"\n" => 74,
			'IDENT' => 25
		},
		GOTOS => {
			'ident' => 75
		}
	},
	{#State 64
		DEFAULT => -29
	},
	{#State 65
		ACTIONS => {
			"\n" => 76,
			'LITERAL' => 45,
			'IDENT' => 25
		},
		GOTOS => {
			'symbol' => 67,
			'ident' => 47
		}
	},
	{#State 66
		DEFAULT => -25
	},
	{#State 67
		DEFAULT => -26
	},
	{#State 68
		DEFAULT => -11
	},
	{#State 69
		DEFAULT => -14
	},
	{#State 70
		ACTIONS => {
			'NAME' => 80
		},
		DEFAULT => -59,
		GOTOS => {
			'rule' => 77,
			'rhss' => 79,
			'optname' => 78
		}
	},
	{#State 71
		ACTIONS => {
			'NAME' => 80
		},
		DEFAULT => -59,
		GOTOS => {
			'rule' => 77,
			'rhss' => 81,
			'optname' => 78
		}
	},
	{#State 72
		DEFAULT => -38
	},
	{#State 73
		DEFAULT => -12
	},
	{#State 74
		DEFAULT => -21
	},
	{#State 75
		DEFAULT => -28
	},
	{#State 76
		DEFAULT => -13
	},
	{#State 77
		DEFAULT => -40
	},
	{#State 78
		ACTIONS => {
			'CODE' => 90,
			'LITERAL' => 45,
			'IDENT' => 25,
			'BEGINCODE' => 83,
			"(" => 91,
			"\$" => 85
		},
		DEFAULT => -43,
		GOTOS => {
			'symbol' => 89,
			'rhselt' => 84,
			'rhs' => 82,
			'rhselts' => 87,
			'rhseltwithid' => 86,
			'ident' => 47,
			'code' => 88
		}
	},
	{#State 79
		ACTIONS => {
			"|" => 93,
			";" => 92
		}
	},
	{#State 80
		ACTIONS => {
			'IDENT' => 94
		}
	},
	{#State 81
		ACTIONS => {
			"|" => 93,
			";" => 95
		}
	},
	{#State 82
		ACTIONS => {
			'PREC' => 96
		},
		DEFAULT => -42,
		GOTOS => {
			'prec' => 97
		}
	},
	{#State 83
		DEFAULT => -65
	},
	{#State 84
		ACTIONS => {
			"<" => 98,
			'PLUS' => 99,
			'OPTION' => 100,
			'STAR' => 101,
			"." => 102
		},
		DEFAULT => -50
	},
	{#State 85
		ACTIONS => {
			"(" => 91,
			'error' => 104,
			'CODE' => 90,
			'LITERAL' => 45,
			'IDENT' => 25,
			'BEGINCODE' => 83
		},
		GOTOS => {
			'symbol' => 89,
			'rhselt' => 103,
			'ident' => 47,
			'code' => 88
		}
	},
	{#State 86
		DEFAULT => -46
	},
	{#State 87
		ACTIONS => {
			'CODE' => 90,
			'LITERAL' => 45,
			'IDENT' => 25,
			'BEGINCODE' => 83,
			"(" => 91,
			"\$" => 85
		},
		DEFAULT => -44,
		GOTOS => {
			'symbol' => 89,
			'rhselt' => 84,
			'rhseltwithid' => 105,
			'ident' => 47,
			'code' => 88
		}
	},
	{#State 88
		DEFAULT => -52
	},
	{#State 89
		DEFAULT => -51
	},
	{#State 90
		DEFAULT => -64
	},
	{#State 91
		ACTIONS => {
			'NAME' => 80
		},
		DEFAULT => -59,
		GOTOS => {
			'optname' => 106
		}
	},
	{#State 92
		DEFAULT => -35
	},
	{#State 93
		ACTIONS => {
			'NAME' => 80
		},
		DEFAULT => -59,
		GOTOS => {
			'rule' => 107,
			'optname' => 78
		}
	},
	{#State 94
		DEFAULT => -60
	},
	{#State 95
		DEFAULT => -37
	},
	{#State 96
		ACTIONS => {
			'LITERAL' => 45,
			'IDENT' => 25
		},
		GOTOS => {
			'symbol' => 108,
			'ident' => 47
		}
	},
	{#State 97
		ACTIONS => {
			'CODE' => 90,
			'BEGINCODE' => 83
		},
		DEFAULT => -62,
		GOTOS => {
			'epscode' => 110,
			'code' => 109
		}
	},
	{#State 98
		ACTIONS => {
			'STAR' => 112,
			'PLUS' => 111
		}
	},
	{#State 99
		DEFAULT => -58
	},
	{#State 100
		DEFAULT => -56
	},
	{#State 101
		DEFAULT => -54
	},
	{#State 102
		ACTIONS => {
			'IDENT' => 113
		}
	},
	{#State 103
		ACTIONS => {
			'OPTION' => 100,
			"<" => 98,
			'PLUS' => 99,
			'STAR' => 101
		},
		DEFAULT => -48
	},
	{#State 104
		DEFAULT => -49
	},
	{#State 105
		DEFAULT => -45
	},
	{#State 106
		ACTIONS => {
			"(" => 91,
			"\$" => 85,
			'CODE' => 90,
			'LITERAL' => 45,
			'IDENT' => 25,
			'BEGINCODE' => 83
		},
		DEFAULT => -43,
		GOTOS => {
			'symbol' => 89,
			'rhselt' => 84,
			'rhs' => 114,
			'rhselts' => 87,
			'rhseltwithid' => 86,
			'ident' => 47,
			'code' => 88
		}
	},
	{#State 107
		DEFAULT => -39
	},
	{#State 108
		DEFAULT => -61
	},
	{#State 109
		DEFAULT => -63
	},
	{#State 110
		DEFAULT => -41
	},
	{#State 111
		ACTIONS => {
			'LITERAL' => 45,
			'IDENT' => 25
		},
		GOTOS => {
			'symbol' => 115,
			'ident' => 47
		}
	},
	{#State 112
		ACTIONS => {
			'LITERAL' => 45,
			'IDENT' => 25
		},
		GOTOS => {
			'symbol' => 116,
			'ident' => 47
		}
	},
	{#State 113
		DEFAULT => -47
	},
	{#State 114
		ACTIONS => {
			")" => 117
		}
	},
	{#State 115
		ACTIONS => {
			">" => 118
		}
	},
	{#State 116
		ACTIONS => {
			">" => 119
		}
	},
	{#State 117
		DEFAULT => -53
	},
	{#State 118
		DEFAULT => -57
	},
	{#State 119
		DEFAULT => -55
	}
],
                                  yyrules  =>
[
	[#Rule 0
		 '$start', 2, undef
	],
	[#Rule 1
		 'eyapp', 3, undef
	],
	[#Rule 2
		 'symbol', 1,
sub {
                    my($symbol,$lineno)=@{$_[1]};
                        exists($$syms{$symbol})
                    or  do {
                        $$syms{$symbol} = $lineno;
                        $$term{$symbol} = undef;
                    };
                    $$semantic{$symbol} = 0 unless exists($$semantic{$symbol});
                    $_[1]
                }
	],
	[#Rule 3
		 'symbol', 1, undef
	],
	[#Rule 4
		 'ident', 1,
sub {
                    my($symbol,$lineno)=@{$_[1]};
                        exists($$syms{$symbol})
                    or  do {
                        $$syms{$symbol} = $lineno;
                        $$term{$symbol} = undef;
                    };
                    $$semantic{$symbol} = 1 unless exists($$semantic{$symbol});
                    $_[1]
                }
	],
	[#Rule 5
		 'head', 2, undef
	],
	[#Rule 6
		 'headsec', 0, undef
	],
	[#Rule 7
		 'headsec', 1, undef
	],
	[#Rule 8
		 'decls', 2, undef
	],
	[#Rule 9
		 'decls', 1, undef
	],
	[#Rule 10
		 'decl', 1, undef
	],
	[#Rule 11
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
	],
	[#Rule 12
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
	],
	[#Rule 13
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
	],
	[#Rule 14
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
	],
	[#Rule 15
		 'decl', 3,
sub { $start=$_[2][0]; undef }
	],
	[#Rule 16
		 'decl', 2,
sub { push(@$head,$_[1]); undef }
	],
	[#Rule 17
		 'decl', 3,
sub { undef }
	],
	[#Rule 18
		 'decl', 3,
sub { $defaultaction = $_[2]; undef }
	],
	[#Rule 19
		 'decl', 2,
sub { 
            $tree = 1;
            $bypass = ($_[1][0] =~m{bypass})? 1 : 0;
            $alias = ($_[1][0] =~m{alias})? 1 : 0;
            $defaultaction = [ ' goto &Parse::Eyapp::Driver::YYBuildAST ', $lineno[0]]; 
            undef 
          }
	],
	[#Rule 20
		 'decl', 2,
sub { 
            $metatree = $tree = 1;
            undef 
          }
	],
	[#Rule 21
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
	],
	[#Rule 22
		 'decl', 3,
sub { $expect=$_[2][0]; undef }
	],
	[#Rule 23
		 'decl', 2,
sub { $_[0]->YYErrok }
	],
	[#Rule 24
		 'typedecl', 0, undef
	],
	[#Rule 25
		 'typedecl', 3, undef
	],
	[#Rule 26
		 'symlist', 2,
sub { push(@{$_[1]},$_[2]); $_[1] }
	],
	[#Rule 27
		 'symlist', 1,
sub { [ $_[1] ] }
	],
	[#Rule 28
		 'identlist', 2,
sub { push(@{$_[1]},$_[2]); $_[1] }
	],
	[#Rule 29
		 'identlist', 1,
sub { [ $_[1] ] }
	],
	[#Rule 30
		 'body', 2,
sub {
                $start
            or  $start=$$rules[1][0];

                ref($$nterm{$start})
            or  _SyntaxError(2,"Start symbol $start not found ".
                                "in rules section",$_[2][1]);

            # [ left hand side,   right hand side,  precedence, rulename, code, ??prefixofcode ]
            $$rules[0]=[ '$start', [ $start, chr(0) ], undef, undef, undef,] # prefixofcode???  undef ];
        }
	],
	[#Rule 31
		 'body', 1,
sub { _SyntaxError(2,"No rules in input grammar",$_[1][1]); }
	],
	[#Rule 32
		 'rulesec', 2, undef
	],
	[#Rule 33
		 'rulesec', 1, undef
	],
	[#Rule 34
		 '@1-2', 0,
sub { $start = $_[1][0]; }
	],
	[#Rule 35
		 'startrules', 5,
sub { _AddRules($_[1],$_[4]); undef }
	],
	[#Rule 36
		 'startrules', 2,
sub { $_[0]->YYErrok }
	],
	[#Rule 37
		 'rules', 4,
sub { _AddRules($_[1],$_[3]); undef }
	],
	[#Rule 38
		 'rules', 2,
sub { $_[0]->YYErrok }
	],
	[#Rule 39
		 'rhss', 3,
sub { push(@{$_[1]},$_[3]); $_[1] }
	],
	[#Rule 40
		 'rhss', 1,
sub { [ $_[1] ] }
	],
	[#Rule 41
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
	],
	[#Rule 42
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
	],
	[#Rule 43
		 'rhs', 0, undef
	],
	[#Rule 44
		 'rhs', 1, undef
	],
	[#Rule 45
		 'rhselts', 2,
sub { 
                push(@{$_[1]},$_[2]); 
                $_[1] 
              }
	],
	[#Rule 46
		 'rhselts', 1,
sub { [ $_[1] ] }
	],
	[#Rule 47
		 'rhseltwithid', 3,
sub {
          push @{$_[1][1]}, $_[3][0];
          $_[1]
        }
	],
	[#Rule 48
		 'rhseltwithid', 2,
sub {
          # check that is an identifier
            _SyntaxError(2,"\$ is allowed for identifiers only (Use dot notation instead)",$lineno[0]) 
          if not_an_id($_[2][1][0]);
          push @{$_[2][1]}, $_[2][1][0];
          $_[2]
        }
	],
	[#Rule 49
		 'rhseltwithid', 2,
sub { _SyntaxError(2,"\$ is allowed for identifiers only",$lineno[0]) }
	],
	[#Rule 50
		 'rhseltwithid', 1, undef
	],
	[#Rule 51
		 'rhselt', 1,
sub { [ 'SYMB', $_[1] ] }
	],
	[#Rule 52
		 'rhselt', 1,
sub { [ 'CODE', $_[1] ] }
	],
	[#Rule 53
		 'rhselt', 4,
sub { 
           my ($name, $rhs) = @_[2, 3];


           my $code = $defaultaction && [ @$defaultaction ];
           $code =[ ' goto &Parse::Eyapp::Driver::YYBuildAST ', $lineno[0]] unless $metatree;

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
	],
	[#Rule 54
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
	],
	[#Rule 55
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
	],
	[#Rule 56
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
	],
	[#Rule 57
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
	],
	[#Rule 58
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
	],
	[#Rule 59
		 'optname', 0, undef
	],
	[#Rule 60
		 'optname', 2,
sub { 
           $_[2][2] = $_[1][0];
           $_[2] 
         }
	],
	[#Rule 61
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
	],
	[#Rule 62
		 'epscode', 0,
sub { $defaultaction }
	],
	[#Rule 63
		 'epscode', 1,
sub { $_[1] }
	],
	[#Rule 64
		 'code', 1,
sub { $_[1] }
	],
	[#Rule 65
		 'code', 1,
sub {
        _SyntaxError(2, "%begin code is allowed only when metatree is active\n", $lineno[0])
          unless $metatree;
        my $code = $_[1];
        push @$code, 'BEGINCODE';
        return $code;
      }
	],
	[#Rule 66
		 'tail', 0, undef
	],
	[#Rule 67
		 'tail', 1,
sub { $tail=$_[1] }
	]
],
                                  @_);
    bless($self,$class);
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

            $$input=~/\G%name\s+semantic\s+actions/gc
        and return('NAMESEMANTICS',[ undef, $lineno[0] ]);

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
              'SEMANTIC', 'BYPASS', 'ACCESSORS' }
    =       (  $head,  $tail,  $rules,  $nterm,  $term,
               $nullable, $precterm, $syms, $start, $expect, 
               $semantic, $bypass, $accessors);

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

    $parsed
}

=head1 NAME
 
Parse::Eyapp::Parse - Implements parser objects
 
=head1 SEE ALSO
  

=over

=item * L<Parse::Eyapp>,

=item * L<http://nereida.deioc.ull.es/~pl/perlexamples/section_eyappts.html> (Spanish),

=item * L<eyapp>,

=item * L<treereg>,

=item * L<Parse::Yapp>,

=item * yacc(1),

=item * bison(1),

=item * The classic book "Compilers: Principles, Techniques, and Tools" by Alfred V. Aho, Ravi Sethi and

Jeffrey D. Ullman (Addison-Wesley 1986)


=item * L<Parse::RecDescent>.

=back

=head1 AUTHOR
 
Casiano Rodriguez-Leon (casiano@ull.es)
 
=head1 ACKNOWLEDGMENTS

This work has been supported by CEE (FEDER) and the Spanish Ministry of
Educación y Ciencia through Plan Nacional I+D+I number TIN2005-08818-C04-04
(ULL::OPLINK project). Support from Gobierno de Canarias was through GC02210601
(Grupos Consolidados).
The University of La Laguna has also supported my work in many ways
and for many years.
I wish to thank Francois Desarmenien for his C<Parse::Yapp> module,
to my students at La Laguna and to the Perl Community. Special thanks to
my family and Larry Wall.

=head1 LICENCE AND COPYRIGHT

Copyright (c) 2006 Casiano Rodriguez-Leon (casiano@ull.es). All rights reserved.

This module is free software; you can redistribute it and/or
modify it under the same terms as Perl itself. See L<perlartistic>.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

=cut



1;
