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
