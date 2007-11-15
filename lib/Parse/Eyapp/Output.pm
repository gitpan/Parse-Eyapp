#
# Module Parse::Eyapp::Output
#
# This module is based on Francois Desarmenien Parse::Yapp distribution
# (c) Parse::Yapp Copyright 1998-2001 Francois Desarmenien, all rights reserved.
# (c) Parse::Eyapp Copyright 2006 Casiano Rodriguez-Leon, all rights reserved.
#

package Parse::Eyapp::Output;
@ISA=qw ( Parse::Eyapp::Lalr );

require 5.004;

use Parse::Eyapp::Base qw(compute_lines);
use Parse::Eyapp::Lalr;
use Parse::Eyapp::Driver;
use File::Basename;
#use Data::Dumper;
use List::Util qw(first);

use strict;

use Carp;

####################################################################
# Returns    : The string '{\n file contents }\n'  with pre and post comments
# Parameters : a file name
sub _CopyModule {
  my $file = shift;

	my $text ="\n{ ###########Included $file file\n";

  open(DRV,$file) or	die "BUG: could not open $file";
	$text.= join('',<DRV>);
	close(DRV);
	$text.="\n} ###########End of include $file file\n";
}

# Compute line numbers for the outputfile. Need for debugging
our $pattern = '################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################';

sub Output {
    my($self)=shift;

    $self->Options(@_);

    my ($GRAMMAR, $TERMS, $FILENAME, $PACKAGES); # Cas
    my($package)=$self->Option('classname');
    my($head,$states,$rules,$tail,$driver, $bypass, $accessors, $buildingtree);
    my($version)=$Parse::Eyapp::Driver::VERSION;
    my($datapos);
    my $makenodeclasses = '';
    my($text)=$self->Option('template') ||<<'EOT';
###################################################################################
#
#    This file was generated using Parse::Eyapp version <<$version>>.
#
# (c) Parse::Yapp Copyright 1998-2001 Francois Desarmenien.
# (c) Parse::Eyapp Copyright 2006-2007 Casiano Rodriguez-Leon. Universidad de La Laguna.
#        Don't edit this file, use source file <<$FILENAME>> instead.
#
#             ANY CHANGE MADE HERE WILL BE LOST !
#
###################################################################################
package <<$package>>;
use strict;

push @<<$package>>::ISA, 'Parse::Eyapp::Driver';

<<$driver>>

<<$head>>
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################

my $warnmessage =<< "EOFWARN";
Warning!: Did you changed the \@<<$package>>::ISA variable inside the header section of the eyapp program?
EOFWARN

sub new {
        my($class)=shift;
        ref($class)
    and $class=ref($class);

    warn $warnmessage unless __PACKAGE__->isa('Parse::Eyapp::Driver'); 
    my($self)=$class->SUPER::new( yyversion => '<<$version>>',
                                  yyGRAMMAR  =>
<<$GRAMMAR>>,
                                  yyTERMS  =>
<<$TERMS>>,
                                  yyFILENAME  => <<$FILENAME>>,
                                  yystates =>
<<$states>>,
                                  yyrules  =>
<<$rules>>,
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
                                  yybypass => <<$bypass>>,
                                  yybuildingtree => <<$buildingtree>>,
                                  @_,);
    bless($self,$class);

    <<$makenodeclasses>>
    $self;
}

<<$tail>>
################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################
<<$accessors>>
1;
EOT

	$driver='';

      defined($package)
  or $package='Parse::Eyapp::Default'; # may be the caller package?

	$head= $self->Head();
	$rules=$self->RulesTable();
	$states=$self->DfaTable();
	$tail= $self->Tail();
	#local $Data::Dumper::Purity = 1;

  ($GRAMMAR, $PACKAGES) = $self->Rules();
  $bypass = $self->Bypass;
  $buildingtree = $self->Buildingtree;
  $accessors = $self->Accessors;
  $TERMS = $self->Terms();
  $FILENAME = '"'.$self->Option('inputfile').'"';

	if ($self->Option('standalone')) {
		$driver =_CopyModule($Parse::Eyapp::Driver::FILENAME);
    $driver .= _CopyModule($Parse::Eyapp::Node::FILENAME);
    $driver =~ s/\n\s*use Parse::Eyapp::YATW;\n//g;
    $driver .= _CopyModule($Parse::Eyapp::YATW::FILENAME);
    $makenodeclasses = '$self->make_node_classes('.$PACKAGES.');';
  }
  else {
    $driver = q{
BEGIN {
  # This strange way to load the modules is to guarantee compatibility when
  # using several standalone and non-standalone Eyapp parsers

  require Parse::Eyapp::Driver unless Parse::Eyapp::Driver->can('YYParse');
  require Parse::Eyapp::Node unless Parse::Eyapp::Node->can('hnew'); 
}
    }; # end string $driver
    $makenodeclasses = '$self->make_node_classes('.$PACKAGES.');';
  }

	$text=~s/<<(\$.+)>>/$1/gee;

	$text;
}


####################################################################
# Usage      :   
#   my $warnings = Parse::Eyapp->new_grammar(
#                                 input=>$translationscheme,
#                                 classname=>'main',
#                                 firstline => 6,
#                                 outputfile => 'main.pm'
#                  );
#  die "$warnings\nSolve Ambiguities. See file main.output\n"  if $warnings;
#
# Returns    : string reporting about the ambiguities and conflicts or ''
# Throws     : croaks if invalid arguments, if the grammar has errors, if can not open
#              files or if the semantic actions have errors
#             
# Parameters : 
my %_new_grammar = (
  input => undef, 		
  classname => undef,
  firstline => undef,
  linenumbers => undef,
  outputfile => undef,
);
my $validkeys = do { local $" = ", "; my @validkeys = keys(%_new_grammar); "@validkeys" };

sub new_grammar {
  my $class = shift;

  croak "Error in new_package: Use named arguments" if (@_ %2);
  my %arg = @_;
  if (defined($a = first { !exists($_new_grammar{$_}) } keys(%arg))) {
    croak("Parse::Eyapp::Treeregexp::new Error!: unknown argument $a. Valid arguments are: $validkeys")
  }
  
  my $grammar = $arg{input} or croak "Error in new_package: Specify a input grammar";

  my $name = $arg{classname} or croak 'Error in  new_package: Please provide a name for the grammar';

  my ($package, $filename, $line) = caller;

  $line = $arg{firstline} if defined($arg{firstline}) and ($arg{firstline} =~ /\d+/);

  my $linenumbers = $arg{linenumbers};
  $linenumbers = 1 unless defined($linenumbers);

  croak "Bad grammar." 
    unless my $p = Parse::Eyapp->new(
					input => $grammar, 
					inputfile => $filename, 
					firstline => $line,
					linenumbers => $linenumbers,
		); 

  my $text = $p->Output(classname => $name) or croak "Can't generate parser.";

  my $outputfile = $arg{outputfile};
  croak "Error in new_package: Invalid option for parameter linenumber" unless $linenumbers =~ m{[01]};

  if (defined($outputfile)) {
    my($base,$path,$sfx)=fileparse($outputfile,'\..*$');
    $p->outputtables($path, $base);
    my($outfile)="$path$base.pm";
      open(my $OUT,">$outfile")
    or die "Cannot open $outfile for writing.\n";

    compute_lines(\$text, $outfile, $pattern);
    print $OUT $text; #$p->Output(classname  => $name, linenumbers => $linenumbers);
  }

  my $x = eval $text;
  $@ and die "Error while compiling your parser: $@\n";
  return $p;
}


1;

__END__

=head1 NAME
 
Parse::Eyapp::Output - Implements new_grammar and the dumping of Perl code for the Analysis of Eyapp grammars
 
=head1 SEE ALSO
  
No documentation here. To learn about Parse::Eyapp::Output see the following
documents:
  
=over

=item * L<Parse::Eyapp>,

=item * The tutorial I<Parsing Strings and Trees with> C<Parse::Eyapp>
(An Introduction to Compiler Construction in seven pages)> in

=item * The pdf files in L<http://nereida.deioc.ull.es/~pl/perlexamples/Eyapp.pdf> 


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


