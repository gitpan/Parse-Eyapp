# (c) Parse::Eyapp Copyright 2006 Casiano Rodriguez-Leon, all rights reserved.
package Parse::Eyapp::_TreeregexpSupport;
use strict;
use Carp;
use Parse::Eyapp::Node;
use base qw (Exporter);
our @EXPORT_OK = qw(until_first_match checknumchildren);

################### Support routines #########################

# used whith array patterns
# index of the children to start
# $b recognizer treereg
# $r reference to an array where children that don't match are pushed
sub until_first_match {
  my ($father, $order, $b, $r) = @_;

  return undef unless UNIVERSAL::can($father, 'children');
  for ($order..0+$father->children()) {
    my $t = $father->child($_);
    return $t if ($b->($t));
    push @$r, $t;
  }
  return undef;
}

sub checknumchildren {
  my ($self, $numexpected, $line, $filename, $there_are_lists, $severity) = @_;
 
	my $numchildren = $self->children;
  return 1 if ($numchildren == $numexpected) or ($there_are_lists and $numchildren >= $numexpected);

  return 0 unless ($severity > 1);
  my $class = ref($self);
  my $clause = $there_are_lists? ' at least' : '';
	my $warnmessage =<<"END_OF_WARN_MESSAGE";
found node $class with $numchildren children.
Expected$clause $numexpected children (see line $line of $filename)"
END_OF_WARN_MESSAGE
  croak "Error! $warnmessage" if $severity > 2;
	warn "Warning! $warnmessage";

  return 0;
}

1;


__END__

=head1 NAME 

Parse::Eyapp::_TreeregexpSupport - Gives support to Classes Dynamically Generated by Treeregexp
 
=head1 SEE ALSO
  
No documentation here. To learn about Parse::Eyapp::_TreeregexpSupport see:

=over

=item * L<Parse::Eyapp>,

=item * The pdf files in L<http://nereida.deioc.ull.es/~pl/perlexamples/Eyapp.pdf> and  
L<http://nereida.deioc.ull.es/~pl/perlexamples/eyapptut.pdf>.

=item * The tutorial I<Parsing Strings and Trees with> C<Parse::Eyapp>
(An Introduction to Compiler Construction in seven pages)> in

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

