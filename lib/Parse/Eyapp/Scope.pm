package Parse::Eyapp::Scope;
use strict;
use warnings;
use Carp;
use List::MoreUtils qw(part);
use Parse::Eyapp::Base qw(valid_keys invalid_keys);

my %_new_scope = (
  SCOPE_NAME      => 'STRING',
  ENTRY_NAME      => 'STRING',
  SCOPE_DEPTH     => 'STRING',
);
my $valid_scope_keys = valid_keys(%_new_scope); 

sub new {
 my $class = shift;
  my %args = @_;

  if (defined($a = invalid_keys(\%_new_scope, \%args))) {
    croak("Parse::Eyapp::Scope::new Error!:\n"
         ."unknown argument $a. Valid arguments for new are:\n  $valid_scope_keys")
  }
  $args{ENTRY_NAME}      = 'entry' unless defined($args{ENTRY_NAME});
  $args{SCOPE_NAME}      = 'scope' unless defined($args{SCOPE_NAME});
  $args{SCOPE_DEPTH}     = ''      unless defined($args{SCOPE_DEPTH});
  $args{PENDING_DECL}    = [];
  $args{SCOPE_MARK}      = 0;
  $args{DEPTH}           = -1; # first depth is 0

  bless \%args, $class;
}

sub begin_scope {
  my $self = shift;

  # Set the mark for next scope to the level of the stack of instances
  $self->{SCOPE_MARK} = @{$self->{PENDING_DECL}};
  # Save current mark in the stack of marks
  push @{$self->{SCOPE_STACK}}, $self->{SCOPE_MARK};
  $self->{DEPTH}++; # new scope, new depth
}

####################################################################
# Usage      : ($nondec, $declared) = $ids->end_scope($program->{symboltable}, $program, 'type');
# Purpose    : ????
# Returns    : ????
# Parameters : ????
# Throws     : no exceptions
# Comments   : none
# See Also   : n/a
# To Do      : nothing
sub end_scope {
  my $self = shift; # The scope object

  my $st;    # reference to the hash holding the symbol table for this scope
  my $block; # The node owning the current scope
  # first arg can be the "block node" in which case the s.t. is omitted
  if (UNIVERSAL::isa($_[0], 'Parse::Eyapp::Node')) {
    $block = shift; 
  }
  elsif (UNIVERSAL::isa($_[0], 'HASH')) {
    # first arg can be the s.t. in which case the block node is expected
    $st = shift;   
    if (UNIVERSAL::isa($_[0], 'Parse::Eyapp::Node')) {
      $block = shift; 
    }
  }
  else {
    croak "end_scope error: Specify a symbol table or a scope node\n"
  }

  # @_ = Remaining args hold key names for the entry that will be added to the instances

  # Get the index pointing to the beginning of the current scope
  my $scope = pop @{$self->{SCOPE_STACK}};
  croak "Error: end_scope called without matching begin_scope\n" unless defined($scope);

  # Get tyhe instances ocurring in this scope
  my @instances = splice @{$self->{PENDING_DECL}}, $scope;

  if (defined($st)) {
    my ($nodeclared, $declared) = part { exists $st->{$_->key} } @instances;

    $declared   = [] unless $declared;
    $nodeclared = [] unless $nodeclared;

    # Return non declared identifiers to the "pending of declarations" queue
    push @{$self->{PENDING_DECL}}, @$nodeclared;
    
    # Set the scope attribute for those instances that were declared
    for my $i (@$declared) {
      next unless UNIVERSAL::isa($i, 'HASH');
      $i->{$self->{SCOPE_NAME}} = $block;
      if (UNIVERSAL::can($i, 'key')) {
        $i->{$self->{ENTRY_NAME}} = $st->{$i->key};
        $i->{$_} = $st->{$i->key}{$_} for @_;
      }
    }
    
    $block->{$self->{SCOPE_DEPTH}} = $self->{DEPTH} if $self->{SCOPE_DEPTH};
    $self->{DEPTH}--;

    return wantarray? ($nodeclared, $declared): $nodeclared;
  }

  # Not symbol table: Simple scope

  # Set the scope attribute for those instances that were declared
  my @r;
  $block->{$self->{SCOPE_NAME}} = \@r;
  for my $i (@instances) {
    $i->{$self->{SCOPE_NAME}} = $block;
    push @r, $i;
  }
    
  $block->{$self->{SCOPE_DEPTH}} = $self->{DEPTH} if $self->{SCOPE_DEPTH};
  $self->{DEPTH}--;

  return \@instances;
}

# To be called for each ocurrence of an identifier
sub scope_instance { 
  my $self = shift;

  my $NODE = shift;
  
  push @{$self->{PENDING_DECL}}, $NODE; 
}

1;

__END__

=head1 NAME 

Parse::Eyapp::Scope - Support for Scope Analysis
 
=head1 DESCRIPTION

A scope manager helps to compute the mapping function
that maps the uses (instances) of 
source objects to their definitions. For instance, 
in I<identifier scope analysis> the problem is to associate
each ocurrence of an identifier with the declaration
that applies to it. Another example is I<loop scope analysis>
where the problem is to associate each occurrence
of a C<CONTINUE> or C<BREAK> node with the 
shallowest C<LOOP> that encloses it. Or I<label scope
analysis>, the problem to associate a C<GOTO>
node with the node to jump to, that is,
with the C<STATEMENT> associated with the label.

To take advantage of C<Parse::Eyapp::Scope>, 
the compiler writer must mark at the appropriate time 
(for example a new block or new subroutine for I<identifier scope analysis>,
a new loop for I<loop scope analysis>, etc.) the I<beginning of a new scope>
calling the method L<begin_scope|/$scope-E<gt>begin_scope>.
From that point on any I<ocurring instance> of an object 
(for example,
variables in expressions for I<identifier scope analysis>, breaks and continues
for I<loop scope analysis>, etc.) must be declared 
calling the method L<scope_instance|/$scope-E<gt>scope_instance>.
The programmer must also mark the I<end of the current scope> 
at the appropriate time. 

=head2 $scope->end_scope

There are two ways of calling C<$scope-E<gt>end_scope>.
The first one is for Scope Analysis Problems where
a symbol table is needed (for example in I<identifier scope analysis>
and I<label scope analysis>.

=head3 $scope->end_scope with first Arg a Symbol Table

For each I<ocurring instance> of an object C<$x>
that occurred since the last call to  L<begin_scope|/$scope-E<gt>begin_scope>
the call to 

  $scope->end_scope(\%symboltable, $definition_node, 'attr1', 'attr2', ... )

decorates the I<ocurring instance> C<$x> with several attributes: 

=over

=item * An entry C<$x-E<gt>{SCOPE_NAME}> is built that will reference C<$definition_node>.

=item * An entry C<$x-E<gt>{ENTRY_NAME}> is built. That
entry references C<$symboltable{$x-E<gt>key}> (to have a
faster access from the instance to the attributes of the object).
The instantiated nodes must have a C<$x-E<gt>key> method which provides
the entry for the node in the symbol table:

  pl@nereida:~/src/perl/YappWithDefaultAction/examples$ sed -ne '651,657p' Types.eyp
  sub VAR::key {
    my $self = shift;

    return $self->child(0)->{attr}[0];
  }

  *VARARRAY::key = *FUNCTIONCALL::key = \&VAR::key;

=item  For each aditional arguments C<attr#k> an
entry C<$x-E<gt>{attr#k>} will be built.
That entry references C<$symboltable{$x-E<gt>key}{attr#k}>. Therefore
the entry for C<$x> in the symbol table must already 
have a field named C<attr#k>.

=back

In a list context C<$scopeE<gt>end_scope> returns
two references. The first one
is a reference to a list of node instantiated
that weren't defined in the current scope.
The second is a reference to a list of nodes
that were defined in this scope. 
In a scalar context returns the first of these two.
An instance C<$x> is I<defined> if, and only if, 
C<exists $symboltable{$_-E<gt>key}>.

=head3 $scope->end_scope for Simple Scope Analysis

Some scope analysis problems do not require the existence
of a symbol table (for instance, the problem of associating
a C<RETURN> node with the C<FUNCTION> that encloses it). 
For such kind of problems C<$scopeE<gt>end_scope> provides
a second form of call.
The second way to call C<$scopeE<gt>end_scope> is

                 $declared = $scopemanager->end_scope($definition_node);

The only argument is the reference to the node that controls/defines
the scope. The method returns a reference to the declared
nodes. Any node instanced with C<scope_instance>
since the last call to C<begin_scope> is considered I<declared>.

=head2 $scope->begin_scope

Marks the beginning of an scope.
Example (file C<examples/Types.eyp>):

   loopPrefix:
       $WHILE '(' expression ')'
         {
           $loops->begin_scope;
           $_[3]->{line} = $WHILE->[1]; # Save the line for error diagostic
           $_[3]
         }

=head2 $scope->scope_instance

Declares the node argument to be an occurring instance of the scope:

   nereida:~/doc/casiano/PLBOOK/PLBOOK/code> \
       sed -ne '375,380p' Simple6.eyp | cat -n
    1      $Variable '=' binary
    2        {
    3          my $parser = shift;
    4          $ids->scope_instance($Variable);
    5          $parser->YYBuildAST(@_); # "Manually" build the node
    6        }


=head2 Parse::Eyapp::Scope->new

C<Parse::Eyapp::Scope-E<gt>new> returns a scope managment object. 
The scope mapping function is implemented 
by C<Parse::Eyapp::Scope> through a set of attributes
that are added to the nodes involved in the scope analysis.
The names of these attributes can be specified 
using the parameters of C<Parse::Eyapp::Scope-E<gt>new>.
The arguments of C<new> are:

=over

=item * C<SCOPE_NAME> 
is the name chosen for the attribute of the 
I<node instance>  which will held
the reference to the I<definition node>.
If not specified it will take the value C<"scope">.

=item * C<ENTRY_NAME> is the name of the attribute of the
I<node instance>  which will held
the reference to the symbol table entry.
By default takes the value C<"entry">.

=item * C<SCOPE_DEPTH> is the name for an attribute of the 
I<definition node>. Optional. If not specified it will not be
defined.

=back

=head1 SEE ALSO
  
=over

=item * L<Parse::Eyapp>,

=item * L<eyapptut>

=item * The pdf files in L<http://nereida.deioc.ull.es/~pl/perlexamples/Eyapp.pdf> and  
L<http://nereida.deioc.ull.es/~pl/perlexamples/eyapptut.pdf>.

=item * L<http://nereida.deioc.ull.es/~pl/perlexamples/section_eyappts.html> (Spanish),

=item * L<eyapp>,

=item * L<treereg>,

=item * L<Parse::yapp>,

=item * yacc(1),

=item * bison(1),

=item * the classic book "Compilers: Principles, Techniques, and Tools" by Alfred V. Aho, Ravi Sethi and

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

