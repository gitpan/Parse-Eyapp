package Parse::Eyapp::Base;
use strict;
use warnings;
use Carp;
use List::Util qw(first);

use base qw(Exporter);
our @EXPORT_OK = qw(compute_lines slurp_file valid_keys invalid_keys write_file);
our %EXPORT_TAGS = ( 'all' => [ @EXPORT_OK ] );

####################################################################
# Usage      : $input = slurp_file($filename, 'trg');
# Purpose    : opens  "$filename.trg" and sets the scalar
# Parameters : file name and extension (not icluding the dot)
# Comments   : Is this O.S dependent?

sub slurp_file {
  my ($filename, $ext) = @_;

    croak "Error in slurp_file opening file. Provide a filename!\n" 
  unless defined($filename) and length($filename) > 0;
  $ext = "" unless defined($ext);
  $filename .= ".$ext" unless (-r $filename) or ($filename =~ m{[.]$ext$});
  local $/ = undef;
  open my $FILE, $filename or croak "Can't open file $filename"; 
  my $input = <$FILE>;
  close($FILE);
  return $input;
}

sub valid_keys {
  my %valid_args = @_;

  my @valid_args = keys(%valid_args); 
  local $" = ", "; 
  return "@valid_args" 
}

sub invalid_keys {
  my $valid_args = shift;
  my $args = shift;

  return (first { !exists($valid_args->{$_}) } keys(%$args));
}

sub write_file {
  my ($outputfile, $text) = @_;
  defined($outputfile) or croak "Error at write_file. Undefined file name";

  my $OUTPUTFILE;
  
  open($OUTPUTFILE, "> $outputfile") or croak "Can't open file $OUTPUTFILE.";
  print $OUTPUTFILE ($$text);
  close($OUTPUTFILE) or croak "Can't close file $OUTPUTFILE.";
}

sub compute_lines {
  my ($textr, $filename, $pattern) = @_;
  
  local $_ = 1;
  $$textr =~ s{\n$pattern\n|(\n)}
              {
                $_++; 
                if (defined($1)) {
                  "\n";
                }
                else {
                 my $directive = "\n#line $_ $filename\n";
                 $_++;
                 $directive;
                }
              }eg;
}

1;

=head1 NAME 

Parse::Eyapp::Base - Miscellaneous subroutines
 
=head1 SEE ALSO
  
No documentation here. To learn about Parse::Eyapp::Base see:

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

