#
# Module Parse::Eyapp::Grammar
#
# (c) Copyright 1998-2001 Francois Desarmenien, all rights reserved.
# (c) Copyright 2006 Casiano Rodriguez-Leon, all rights reserved.
# 
package Parse::Eyapp::Grammar;
@ISA=qw( Parse::Eyapp::Options );

require 5.004;

use Carp;
use strict;
use Parse::Eyapp::Options;
use Parse::Eyapp::Parse;

###############
# Constructor #
###############
sub new {
    my($class)=shift;
    my($values);

    my($self)=$class->SUPER::new(@_);

    my($parser)=new Parse::Eyapp::Parse;

        defined($self->Option('input'))
    or  croak "No input grammar";

    $values = $parser->Parse($self->Option('input'), 
                             $self->Option('firstline'), # Line where the grammar source starts
                             $self->Option('inputfile'),  # The file or program containing the grammar
                             #$self->Option('buildingtree')  # If building AST
                            );

    undef($parser);

    $$self{GRAMMAR}=_ReduceGrammar($values);

        ref($class)
    and $class=ref($class);

    bless($self, $class);
}

###########
# Methods #
###########
##########################
# Method To View Grammar #
##########################
sub ShowRules {
    my($self)=shift;
    my($rules)=$$self{GRAMMAR}{RULES};
    my($ruleno)=-1;
    my($text);

    for (@$rules) {
        my($lhs,$rhs)=@$_;

        $text.=++$ruleno.":\t".$lhs." -> ";
        if(@$rhs) {
            $text.=join(' ',map { $_ eq chr(0) ? '$end' : $_ } @$rhs);
        }
        else {
            $text.="/* empty */";
        }
        $text.="\n";
    }
    $text;
}

sub classname {
  my ($name, $index, $lhs) = @_;

  $name = $name->[0];

  $name = "_SUPERSTART" if ($lhs =~ /\$start/ and !defined($name));
  $name = "_CODE" if ($lhs =~ /\@(\d+)-(\d+)/ and !defined($name));
  $name = "_PAREN" if ($lhs =~ /PAREN-(\d+)/ and !defined($name));
  $name = "_STAR_LIST_$1" if ($lhs =~ /STAR-(\d+)/ and !defined($name));
  $name = "_PLUS_LIST_$1" if ($lhs =~ /PLUS-(\d+)/ and !defined($name));
  $name = "_OPTIONAL_$1" if ($lhs =~ /OPTIONAL-(\d+)/ and !defined($name));
  $name = "$lhs"."_$index" unless $name;

  return $name;
}

# Added by Casiano
#####################################
# Method To Return the Grammar Rules#
#####################################
sub Rules { # TODO: find proper names
    my($self)=shift;
    my($rules)=$$self{GRAMMAR}{RULES};
    my($text) = "[\n";
    my $packages = ' qw{TERMINAL _OPTIONAL _STAR_LIST _PLUS_LIST ';

    my $index = 0;
    for (@$rules) {
        my($lhs,$rhs,$prec,$name)=@$_;

        my $bypass = $name->[2];
        $bypass = $self->Bypass unless defined($bypass);
        # find an acceptable perl identifier as name
        $name = classname($name, $index, $lhs);
        $packages .= "\n".(" "x9).$name unless $packages =~ m{\b$name\b};

        $text.= "  [ $name => '$lhs', [ ";
        $text.=join(', ',map { $_ eq chr(0) ? "'\$end'" : $_ =~ m{^'} ? $_ : "'$_'" } @$rhs);
        $text.=" ], $bypass ],\n";
        $index++;
    }
    $text .= ']';
    $packages .= '} ';
    return ($text, $packages);
}

# Added by Casiano
#####################################
# Method To Return the Grammar Terms#
#####################################
sub Terms {
    my($self)=shift;
    my(@terms)= sort(keys(%{$$self{GRAMMAR}{TERM}}));
    my %semantic = %{$self->{GRAMMAR}{SEMANTIC}};

    my $text = "{ ";
    $text .= join(', ',
                         map { $_ eq chr(0) ? "'\$end' => 0" : "$_ => $semantic{$_}"} @terms);
    $text .= " }";
}

#####################################
# Method To Return the Bypass Option#
#####################################
sub Bypass {
  my($self)=shift;
    
  return  $$self{GRAMMAR}{BYPASS}
}

sub Buildingtree {
  my($self)=shift;
    
  return  $$self{GRAMMAR}{BUILDINGTREE}
}

#####################################
# Method To Return the ACCESSORS
#####################################
sub Accessors {
  my($self)=shift;
    
  return  $$self{GRAMMAR}{ACCESSORS}
}

###########################
# Method To View Warnings #
###########################
sub Warnings {
    my($self)=shift;
    my($text);
    my($grammar)=$$self{GRAMMAR};

        exists($$grammar{UUTERM})
    and    do {
            $text="Unused terminals:\n\n";
            for (@{$$grammar{UUTERM}}) {
                $text.="\t$$_[0], declared line $$_[1]\n";    
            }
        $text.="\n";
        };
        exists($$grammar{UUNTERM})
    and    do {
            $text.="Useless non-terminals:\n\n";
            for (@{$$grammar{UUNTERM}}) {
                $text.="\t$$_[0], declared line $$_[1]\n";    
            }
        $text.="\n";
        };
        exists($$grammar{UURULES})
    and    do {
            $text.="Useless rules:\n\n";
            for (@{$$grammar{UURULES}}) {
                $text.="\t$$_[0] -> ".join(' ',@{$$_[1]})."\n";    
            }
        $text.="\n";
        };
    $text;
}

######################################
# Method to get summary about parser #
######################################
sub Summary {
    my($self)=shift;
    my($text);

    $text ="Number of rules         : ".
            scalar(@{$$self{GRAMMAR}{RULES}})."\n";
    $text.="Number of terminals     : ".
            scalar(keys(%{$$self{GRAMMAR}{TERM}}))."\n";
    $text.="Number of non-terminals : ".
            scalar(keys(%{$$self{GRAMMAR}{NTERM}}))."\n";
    $text;
}

###############################
# Method to Ouput rules table #
###############################
sub RulesTable {
    my($self)=shift;
    my($inputfile)=$self->Option('inputfile');
    my($linenums)=$self->Option('linenumbers');
    my($rules)=$$self{GRAMMAR}{RULES};
    my $ruleno = 0;
    my($text);

        defined($inputfile)
    or  $inputfile = 'unknown';

    $text="[\n\t";

    $text.=join(",\n\t",
                map {
                    my($lhs,$rhs,$rname,$code)=@$_[0,1,3,4];
                    my($len)=scalar(@$rhs);

                    my($text);

                    $rname = classname($rname, $ruleno, $lhs);

                    $ruleno++;
                    $text.="[#Rule $rname\n\t\t '$lhs', $len,";
                    if($code) {
                        $text.= "\nsub {".
                                (  $linenums
                                 ? qq(\n#line $$code[1] "$inputfile"\n)
                                 : " ").
                                "$$code[0]}";
                    }
                    else {
                        $text.=' undef';
                    }
                    $text.="\n$Parse::Eyapp::Output::pattern\n\t]";

                    $text;
                } @$rules);

    $text.="\n]";

    $text;
}

################################
# Methods to get HEAD and TAIL #
################################
sub Head {
    my($self)=shift;
    my($inputfile)=$self->Option('inputfile');
    my($linenums)=$self->Option('linenumbers');
    my($text);

        $$self{GRAMMAR}{HEAD}[0]
    or  return '';

        defined($inputfile)
    or  $inputfile = 'unkown';

    for (@{$$self{GRAMMAR}{HEAD}}) {
            $linenums
        and $text.=qq(#line $$_[1] "$inputfile"\n);
        $text.=$$_[0];
    }
    $text
}

sub Tail {
    my($self)=shift;
    my($inputfile)=$self->Option('inputfile');
    my($linenums)=$self->Option('linenumbers');
    my($text);

        $$self{GRAMMAR}{TAIL}[0]
    or  return '';

        defined($inputfile)
    or  $inputfile = 'unkown';

        $linenums
    and $text=qq(#line $$self{GRAMMAR}{TAIL}[1] "$inputfile"\n);
    $text.=$$self{GRAMMAR}{TAIL}[0];

    $text
}


#################
# Private Stuff #
#################

sub _UsefulRules {
    my($rules,$nterm) = @_;
    my($ufrules,$ufnterm);
    my($done);

    $ufrules=pack('b'.@$rules);
    $ufnterm={};

    vec($ufrules,0,1)=1;    #start rules IS always useful

    RULE:
    for (1..$#$rules) { # Ignore start rule
        for my $sym (@{$$rules[$_][1]}) {
                exists($$nterm{$sym})
            and next RULE;
        }
        vec($ufrules,$_,1)=1;
        ++$$ufnterm{$$rules[$_][0]};
    }

    do {
        $done=1;

        RULE:
        for (grep { vec($ufrules,$_,1) == 0 } 1..$#$rules) {
            for my $sym (@{$$rules[$_][1]}) {
                    exists($$nterm{$sym})
                and not exists($$ufnterm{$sym})
                and next RULE;
            }
            vec($ufrules,$_,1)=1;
                exists($$ufnterm{$$rules[$_][0]})
            or  do {
                $done=0;
                ++$$ufnterm{$$rules[$_][0]};
            };
        }

    }until($done);

    ($ufrules,$ufnterm)

}#_UsefulRules

sub _Reachable {
    my($rules,$nterm,$term,$ufrules,$ufnterm)=@_;
    my($reachable);
    my(@fifo)=( 0 );

    $reachable={ '$start' => 1 }; #$start is always reachable

    while(@fifo) {
        my($ruleno)=shift(@fifo);

        for my $sym (@{$$rules[$ruleno][1]}) {

                exists($$term{$sym})
            and do {
                ++$$reachable{$sym};
                next;
            };

                (   not exists($$ufnterm{$sym})
                 or exists($$reachable{$sym}) )
            and next;

            ++$$reachable{$sym};
            push(@fifo, grep { vec($ufrules,$_,1) } @{$$nterm{$sym}});
        }
    }

    $reachable

}#_Reachable

sub _SetNullable {
    my($rules,$term,$nullable) = @_;
    my(@nrules);
    my($done);

    RULE:
    for (@$rules) {
        my($lhs,$rhs)=@$_;

            exists($$nullable{$lhs})
        and next;

        for (@$rhs) {
                exists($$term{$_})
            and next RULE;
        }
        push(@nrules,[$lhs,$rhs]);
    }

    do {
        $done=1;

        RULE:
        for (@nrules) {
            my($lhs,$rhs)=@$_;

                    exists($$nullable{$lhs})
                and next;

                for (@$rhs) {
                        exists($$nullable{$_})
                    or  next RULE;
                }
            $done=0;
            ++$$nullable{$lhs};
        }

    }until($done);
}

sub _ReduceGrammar {
    my($values)=@_;
    my($ufrules,$ufnterm,$reachable);
    my($grammar)={ HEAD => $values->{HEAD},
                   TAIL => $values->{TAIL},
                   EXPECT => $values->{EXPECT},
                   # Casiano modifications
                   SEMANTIC       => $values->{SEMANTIC}, # added to simplify AST
                   BYPASS         => $values->{BYPASS},   # added to simplify AST
                   BUILDINGTREE   => $values->{BUILDINGTREE},   # influences the semantic of lists * + ?
                   ACCESSORS      => $values->{ACCESSORS}, # getter-setter for %tree and %metatree
                 };
    my($rules,$nterm,$term) =  @$values {'RULES', 'NTERM', 'TERM'};

    ($ufrules,$ufnterm) = _UsefulRules($rules,$nterm);

        exists($$ufnterm{$values->{START}})
    or  die "*Fatal* Start symbol $values->{START} derives nothing, at eof\n";

    $reachable = _Reachable($rules,$nterm,$term,$ufrules,$ufnterm);

    $$grammar{TERM}{chr(0)}=undef;
    for my $sym (keys %$term) {
            (   exists($$reachable{$sym})
             or exists($values->{PREC}{$sym}) )
        and do {
            $$grammar{TERM}{$sym}
                = defined($$term{$sym}[0]) ? $$term{$sym} : undef;
            next;
        };
        push(@{$$grammar{UUTERM}},[ $sym, $values->{SYMS}{$sym} ]);
    }

    $$grammar{NTERM}{'$start'}=[];
    for my $sym (keys %$nterm) {
            exists($$reachable{$sym})
        and do {
                exists($values->{NULL}{$sym})
            and ++$$grammar{NULLABLE}{$sym};
            $$grammar{NTERM}{$sym}=[];
            next;
        };
        push(@{$$grammar{UUNTERM}},[ $sym, $values->{SYMS}{$sym} ]);
    }

    for my $ruleno (0..$#$rules) {
            vec($ufrules,$ruleno,1)
        and exists($$grammar{NTERM}{$$rules[$ruleno][0]})
        and do {
            push(@{$$grammar{RULES}},$$rules[$ruleno]);
            push(@{$$grammar{NTERM}{$$rules[$ruleno][0]}},$#{$$grammar{RULES}});
            next;
        };
        push(@{$$grammar{UURULES}},[ @{$$rules[$ruleno]}[0,1] ]);
    }

    _SetNullable(@$grammar{'RULES', 'TERM', 'NULLABLE'});

    $grammar;
}#_ReduceGrammar

1;

=head1 NAME
 
Parse::Eyapp::Grammar - Methods to simplify the grammar and generate the analyzer
 
=head1 SEE ALSO
  
No documentation here. To learn about Parse::Eyapp::Grammar see:

=over

=item * L<Parse::Eyapp>,

=item * The tutorial I<Parsing Strings and Trees with> C<Parse::Eyapp>
(An Introduction to Compiler Construction in seven pages)> in

=item * The pdf files in L<http://nereida.deioc.ull.es/~pl/perlexamples/Eyapp.pdf> and  
L<http://nereida.deioc.ull.es/~pl/perlexamples/eyapptut.pdf>.


=item * L<http://nereida.deioc.ull.es/~pl/perlexamples/section_eyappts.html> (Spanish),

=item * L<eyapp>,

=item * L<treereg>,

=item * L<Parse::yapp>,

=item * yacc(1),

=item * bison(1),

=item * The classic book "Compilers: Principles, Techniques, and Tools" by Alfred V. Aho, Ravi Sethi and

=item * Jeffrey D. Ullman (Addison-Wesley 1986)

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

