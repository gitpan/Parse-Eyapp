package Parse::Eyapp::TokenGen;
use strict;
use warnings;

use Scalar::Util qw{reftype looks_like_number};

sub set_tokengens {
  my $parser = shift;

  my %g = @_;
  my $terms = $parser->{TERMS};
  for (keys %g) {
    # Check if $_is a token?
    $terms->{$_}{GENERATOR} = $g{$_};
  }
}

sub set_tokenweights {
  my $parser = shift;

  my %weight = @_;
  my $terms = $parser->{TERMS};
  for (keys %weight) {
    # Check if $_is a token?
    $terms->{$_}{WEIGHT} = $weight{$_};
  }
}

sub set_tokenweightsandgenerators {
  my $parser = shift;
  my %par = @_;

  my $terms = $parser->{TERMS};
  for (keys %par) {
    my $t = $terms->{$_};

    if (reftype($par{$_}) && (reftype($par{$_}) eq 'ARRAY')) {
      ($t->{WEIGHT}, $t->{GENERATOR}) = @{$par{$_}};
      next;
    }

    if (looks_like_number($par{$_})) {
      if ($par{$_} < 0) {
        warn "Warning: set_weights_and_generators: negative weight ($par{$_}) for token <$_>\n"; 
      }
      $terms->{$_}{WEIGHT} = $par{$_};
      next;
    }

    warn "Warning: set_weights_and_generators: unexpected param <$par{$_}> for token <$_>\n";
  }
}

sub token_weight {
  my $parser = shift;
  my $token = shift;
  my $weight = shift;

  $parser->{TERMS}{$token}{WEIGHT} = $weight if $weight && looks_like_number($weight);
  $parser->{TERMS}{$token}{WEIGHT};
}

sub token_generator {
  my $parser = shift;
  my $token = shift;
  my $generator = shift;

  $parser->{TERMS}{$token}{GENERATOR} = $generator if $generator;
  $parser->{TERMS}{$token}{GENERATOR};
}

sub deltaweight {
  my $parser = shift;

  my %delta = @_;

  for my $token (keys(%delta)) {
    my $t = $parser->{TERMS}{$token};
    $t->{WEIGHT} += $delta{$token} if looks_like_number($delta{$token});
    $t->{WEIGHT} = 0 if $t->{WEIGHT} < 0;
  }
}

sub pushdeltaweight {
  my $parser = shift;

  my %d = @_;
  my $weightstack = $parser->{WEIGHTSTACK};
  my $term = $parser->{TERMS};
  %d = map { $_ => $term->{$_}{WEIGHT} } keys %d;
  push @$weightstack, \%d;
  $parser->deltaweight(@_);
}

sub popweight {
  my $parser = shift;

  my $w = pop @{$parser->{WEIGHTSTACK}};

  my $term = $parser->{TERMS};
  for my $token (keys %$w) {
    $term->{$token}{WEIGHT} = $w->{$token};
  }
}

1;
