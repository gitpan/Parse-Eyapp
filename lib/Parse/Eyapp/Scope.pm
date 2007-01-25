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

