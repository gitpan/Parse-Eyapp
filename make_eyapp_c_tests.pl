#!/usr/bin/perl
# This program builds the files in t/cleanok/ and t/cleanvok/
# used to test the behavior of eyapp when executed with option '-c'
use warnings;
use strict;
use File::Basename;

sub get_grammars {
  open my $f, 'MANIFEST' || exit(0);
  my @grammar = grep { m{examples/.*yp$} } <$f>;
  close($f);
  chomp(@grammar);
  exit(0) unless @grammar;
  return @grammar;
}


print "Generating test files for 'eyapp -c'\n";

my @grammar = get_grammars();
for (@grammar) {
  my ($name,$path,$suffix) = fileparse($_);
  $path =~ s{/}{_}g;
  my $cleanok = " t/cleanok/${path}_$name";
  print "vim -O $cleanok $_\n";
  system("./eyapp -c $_ > t/cleanok/${path}_$name");
  my $cleanvok = " t/cleanvok/${path}_$name";
  print "vim -O $cleanvok $_\n";
  system("./eyapp -vc $_ > t/cleanvok/${path}_$name");
}
