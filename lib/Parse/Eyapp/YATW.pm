# (c) Parse::Eyapp Copyright 2006 Casiano Rodriguez-Leon, all rights reserved.
package Parse::Eyapp::YATW;
use strict;
use warnings;
use Carp;
use Data::Dumper;
use List::Util qw(first);
use List::MoreUtils qw(lastval);
use Parse::Eyapp::Base qw( valid_keys invalid_keys );

our $VERSION = $Parse::Eyapp::Driver::VERSION;

# TODO: Check args. Typical args:
# 'CHANGES' => 0,
# 'PATTERN' => sub { "DUMMY" },
# 'NAME' => 'fold',
# 'PATTERN_ARGS' => [],
# 'PENDING_TASKS' => {},
# 'NODE' => []

my %_new_yatw = (
  PATTERN => 'CODE',
  NAME => 'STRING',
);

my $validkeys = valid_keys(%_new_yatw); 

sub new {
  my $class = shift;
  my %args = @_;

  croak "Error. Expected a code reference when building a tree walker. " unless (ref($args{PATTERN}) eq 'CODE');
  if (defined($a = invalid_keys(\%_new_yatw, \%args))) {
    croak("Parse::Eyapp::YATW::new Error!: unknown argument $a. Valid arguments are: $validkeys")
  }


  # obsolete, I have to delete this
  #$args{PATTERN_ARGS} = [] unless (ref($args{PATTERN_ARGS}) eq 'ARRAY'); 

  # Internal fields

  # Tell us if the node has changed after the visit
  $args{CHANGES} = 0;
  
  # PENDING_TASKS is a queue storing the tasks waiting for a "safe time/node" to do them 
  # Usually that time occurs when visiting the father of the node who generated the job 
  # (when asap criteria is applied).
  # Keys are node references. Values are array references. Each entry defines:
  #  [ the task kind, the node where to do the job, and info related to the particular job ]
  # Example: @{$self->{PENDING_TASKS}{$father}}, ['insert_before', $node, ${$self->{NODE}}[0] ];
  $args{PENDING_TASKS} = {};

  # NODE is a stack storing the ancestor of the node being visited
  # Example: my $ancestor = ${$self->{NODE}}[$k]; when k=1 is the father, k=2 the grandfather, etc.
  # Example: CORE::unshift @{$self->{NODE}}, $_[0]; Finished the visit so take it out
  $args{NODE} = [];

  bless \%args, $class;
}

sub buildpatterns {
  my $class = shift;
  
  my @family;
  while (my ($n, $p) = splice(@_, 0,2)) {
    push @family, Parse::Eyapp::YATW->new(NAME => $n, PATTERN => $p);
  }
  return wantarray? @family : $family[0];
}

####################################################################
# Usage      : @r = $b{$_}->m($t)
#              See Simple4.eyp and m_yatw.pl in the examples directory
# Returns    : Returns an array of nodes matching the treeregexp
#              The set of nodes is a Parse::Eyapp::Node::Match tree 
#              showing the relation between the matches
# Parameters : The tree (and the object of course)
# depth is no longer used: eliminate
sub m {
  my $p = shift(); # pattern YATW object
  my $t = shift;   # tree
  my $pattern = $p->{PATTERN}; # CODE ref

  # References to the found nodes are stored in @stack
  my @stack = ( Parse::Eyapp::Node::Match->new(node=>$t, depth=>0, dewey => "") ); 
  my @results;
  do {
    my $n = CORE::shift(@stack);
    my %n = %$n;

    my $dewey = $n->{dewey};
    my $d = $n->{depth};
    if ($pattern->($n{node})) {
      $n->{family} = [ $p ];
      $n->{patterns} = [ 0 ];

      # Is at this time that I have to compute the father
      my $f = lastval { $dewey =~ m{^$_->{dewey}}} @results;
      $n->{father} = $f;
      # ... and children
      unshift @{$f->{children}}, $n if defined($f);
      push @results, $n;
    }
    my $k = 0;
    CORE::unshift @stack, 
       map { 
              local $a;
              $a = Parse::Eyapp::Node::Match->new(node=>$_, depth=>$d+1, dewey=>"$dewey.$k" );
              $k++;
              $a;
           } $n{node}->children();
  } while (@stack);

  return wantarray? @results : $results[0];
}

######################### getter-setter for YATW objects ###########################

sub pattern {
  my $self = shift;
  $self->{PATTERN} = shift if (@_);
  return $self->{PATTERN};
}

sub name {
  my $self = shift;
  $self->{NAME} = shift if (@_);
  return $self->{NAME};
}

#sub pattern_args {
#  my $self = shift;
#
#  $self->{PATTERN_ARGS} = @_ if @_;
#  return @{$self->{PATTERN_ARGS}};
#}

########################## PENDING TASKS management ################################

# Purpose    : Deletes the node that matched from the list of children of its father. 
sub delete {
  my $self = shift;

  bless $self->{NODE}[0], 'Parse::Eyapp::Node::DELETE';
}
  
sub make_delete_effective {
  my $self = shift;
  my $node = shift;

  my $i = -1+$node->children;
  while ($i >= 0) {
    if (UNIVERSAL::isa($node->child($i), 'Parse::Eyapp::Node::DELETE')) {
      $self->{CHANGES}++ if splice @{$node->{children}}, $i, 1;
    }
    $i--;
  }
}

####################################################################
# Usage      :    my $b = Parse::Eyapp::Node->new( 'NUM(TERMINAL)', sub { $_[1]->{attr} = 4 });
#                 $yatw_pattern->unshift($b); 
# Parameters : YATW object, node to insert, 
#              ancestor offset: 0 = root of the tree that matched, 1 = father, 2 = granfather, etc.

sub unshift {
  my ($self, $node, $k) = @_;
  $k = 1 unless defined($k); # father by default

  my $ancestor = ${$self->{NODE}}[$k];
  croak "unshift: does not exist ancestor $k of node ".Dumper(${$self->{NODE}}[0]) unless defined($ancestor);

  # Stringification of $ancestor. Hope it works
                                            # operation, node to insert, 
  push @{$self->{PENDING_TASKS}{$ancestor}}, ['unshift', $node ];
}

sub insert_before {
  my ($self, $node) = @_;

  my $father = ${$self->{NODE}}[1];
  croak "insert_before: does not exist father of node ".Dumper(${$self->{NODE}}[0]) unless defined($father);

                                           # operation, node to insert, before this node 
  push @{$self->{PENDING_TASKS}{$father}}, ['insert_before', $node, ${$self->{NODE}}[0] ];
}

sub _delayed_insert_before {
  my ($father, $node, $before) = @_;

  my $i = 0;
  for ($father->children()) {
    last if ($_ == $before);
    $i++;
  }
  splice @{$father->{children}}, $i, 0, $node;
}

sub do_pending_tasks {
  my $self = shift;
  my $node = shift;

  my $mytasks = $self->{PENDING_TASKS}{$node};
  while ($mytasks and (my $job = shift @{$mytasks})) {
    my @args = @$job;
    my $task = shift @args;

    # change this for a jump table
    if ($task eq 'unshift') {
      CORE::unshift(@{$node->{children}}, @args);
      $self->{CHANGES}++;
    }
    elsif ($task eq 'insert_before') {
      _delayed_insert_before($node, @args);
      $self->{CHANGES}++;
    }
  }
}

####################################################################
# Parameters : pattern, node, father of the node, index of the child in the children array
# YATW object. Probably too many 
sub s {
  my $self = shift;
  my $node = $_[0] or croak("Error. Method __PACKAGE__::s requires a node");
  CORE::unshift @{$self->{NODE}}, $_[0];
  # father is $_[1]
  my $index = $_[2];

  # If is not a reference or can't children then simply check the matching and leave
  if (!ref($node) or !UNIVERSAL::can($node, "children"))  {
                                         
    $self->{CHANGES}++ if $self->pattern->(
      $_[0],  # Node being visited  
      $_[1],  # Father of this node
      $index, # Index of this node in @Father->children
      $self,  # The YATW pattern object   
    );
    return;
  };
  
  # Else, is not a leaf and is a regular Parse::Eyapp::Node
  # Recursively transform subtrees
  my $i = 0;
  for (@{$node->{children}}) {
    $self->s($_, $_[0], $i);
    $i++;
  }
  
  my $number_of_changes = $self->{CHANGES};
  # Now is safe to delete children nodes that are no longer needed
  $self->make_delete_effective($node);

  # Safely do pending jobs for this node
  $self->do_pending_tasks($node);

  #node , father, childindex, and ... 
  #Change YATW object to be the  first argument?
  if ($self->pattern->($_[0], $_[1], $index, $self)) {
    $self->{CHANGES}++;
  }
  shift @{$self->{NODE}};
}

1;

__END__

=head1 Parse::Eyapp::YATW
 
Parse::Eyapp::YATW - Implements AST transformations
 
=head1 SEE ALSO
  
No documentation here. To learn about Parse::Eyapp::YATW see the following
documents:
  
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


