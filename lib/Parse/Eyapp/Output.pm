#
# Module Parse::Eyapp::Output
#
# This module is based on Francois Desarmenien Parse::Yapp distribution
# (c) Parse::Yapp Copyright 1998-2001 Francois Desarmenien, all rights reserved.
# (c) Parse::Eyapp Copyright 2006-2008 Casiano Rodriguez-Leon, all rights reserved.
#

package Parse::Eyapp::Output;
use strict;

our @ISA=qw ( Parse::Eyapp::Lalr );

require 5.004;

use Parse::Eyapp::Base qw(compute_lines);
use Parse::Eyapp::Lalr;
use Parse::Eyapp::Driver;
use Parse::Eyapp::Node; # required to have access to $Parse::Eyapp::Node::FILENAME
use File::Basename;
#use Data::Dumper;
use List::Util qw(first);


use Carp;

####################################################################
# Returns    : The string '{\n file contents }\n'  with pre and post comments
# Parameters : a file name
sub _CopyModule {
  my ($module, $function, $file) = @_;

  open(DRV,$file) or	die "BUG: could not open $file";
	my $text = join('',<DRV>);
	close(DRV);

  my $label = $module;
  $label =~ s/::/_/g;
	return << "EOCODE";
  # Loading $module
  BEGIN {
    unless ($module->can('$function')) {
      eval << 'MODULE_$label'
$text
MODULE_$label
    }; # Unless $module was loaded
  } ########### End of BEGIN { load $file }

EOCODE
}

## This sub gives support to the "%tree alias" directive
## Builds the text for the named accessors to the children
sub make_accessors {
  my $accessors = shift; # hash reference: like left => 0

  my $text = '{';
  for (keys(%$accessors)) {
    $text .= "\n      '$_' => $accessors->{$_},";
  }
  return "$text\n   }";
}

# Compute line numbers for the outputfile. Need for debugging
our $pattern = '################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################';

sub Output {
  my($self)=shift;

  $self->Options(@_);

  my ($GRAMMAR, $TERMS, $FILENAME, $PACKAGES); # Cas
  my($package)=$self->Option('classname');

  my($head,$states,$rules,$tail,$driver, $bypass, $accessors, $buildingtree, $prefix);
  my($version)=$Parse::Eyapp::Driver::VERSION;
  my($datapos);
  my $makenodeclasses = '';
	$driver='';

      defined($package)
  or $package='Parse::Eyapp::Default'; # may be the caller package?

	$head= $self->Head();
	$rules=$self->RulesTable();
	$states=$self->DfaTable();
	$tail= $self->Tail();

  # In case the file ends with documentation and without a 
  # =cut
  #
  $tail = $tail."\n\n=for None\n\n=cut\n\n" unless $tail =~ /\n\n=cut\n/;
	#local $Data::Dumper::Purity = 1;

  ($GRAMMAR, $PACKAGES) = $self->Rules();
  $bypass = $self->Bypass;
  $prefix = $self->Prefix;

  $buildingtree = $self->Buildingtree;
  $accessors = $self->Accessors;
  my $accessors_hash = make_accessors($accessors);
  $TERMS = $self->Terms();
  $FILENAME = '"'.$self->Option('inputfile').'"';

	if ($self->Option('standalone')) {
    # Copy Base, Driver, Node and YATW
		$driver  =_CopyModule('Parse::Eyapp::Base', 'firstval', $Parse::Eyapp::Base::FILENAME);
		$driver .=_CopyModule('Parse::Eyapp::Driver','YYParse', $Parse::Eyapp::Driver::FILENAME);
    $driver .= _CopyModule('Parse::Eyapp::Node', 'm', $Parse::Eyapp::Node::FILENAME);

    # Remove the line use Parse::Eyapp::YATW
    $driver =~ s/\n\s*use Parse::Eyapp::YATW;\n//g;
    $driver .= _CopyModule('Parse::Eyapp::YATW', 'm', $Parse::Eyapp::YATW::FILENAME);

    $makenodeclasses = '$self->make_node_classes('.$PACKAGES.');';
  }
  else {
    $driver = make_header_for_driver_pm();
    $makenodeclasses = '$self->make_node_classes('.$PACKAGES.');';
  }

  my($text)=$self->Option('template') || Driver_pm();

	$text=~s/<<(\$.+)>>/$1/gee;

	$text;
}

sub make_header_for_driver_pm {
  return q{
BEGIN {
  # This strange way to load the modules is to guarantee compatibility when
  # using several standalone and non-standalone Eyapp parsers

  require Parse::Eyapp::Driver unless Parse::Eyapp::Driver->can('YYParse');
  require Parse::Eyapp::Node unless Parse::Eyapp::Node->can('hnew'); 
}
  }; 
}

sub Driver_pm {
  return <<'EOT';
########################################################################################
#
#    This file was generated using Parse::Eyapp version <<$version>>.
#
# (c) Parse::Yapp Copyright 1998-2001 Francois Desarmenien.
# (c) Parse::Eyapp Copyright 2006-2008 Casiano Rodriguez-Leon. Universidad de La Laguna.
#        Don't edit this file, use source file <<$FILENAME>> instead.
#
#             ANY CHANGE MADE HERE WILL BE LOST !
#
########################################################################################
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
  ref($class) and $class=ref($class);

  warn $warnmessage unless __PACKAGE__->isa('Parse::Eyapp::Driver'); 
  my($self)=$class->SUPER::new( 
    yyversion => '<<$version>>',
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
    yybypass       => <<$bypass>>,
    yybuildingtree => <<$buildingtree>>,
    yyprefix       => '<<$prefix>>',
    yyaccessors    => <<$accessors_hash>>,
    @_,
  );
  bless($self,$class);

  <<$makenodeclasses>>
  $self;
}

<<$tail>>

################ @@@@@@@@@ End of User Code @@@@@@@@@ ###################

1;
EOT
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

  croak "Error in new_grammar: Use named arguments" if (@_ %2);
  my %arg = @_;
  if (defined($a = first { !exists($_new_grammar{$_}) } keys(%arg))) {
    croak("Parse::Eyapp::Output::new_grammar Error!: unknown argument $a. Valid arguments are: $validkeys")
  }
  
  my $grammar = $arg{input} or croak "Error in new_grammar: Specify a input grammar";

  my $name = $arg{classname} or croak 'Error in  new_grammar: Please provide a name for the grammar';

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
  croak "Error in new_grammar: Invalid option for parameter linenumber" unless $linenumbers =~ m{[01]};

  if (defined($outputfile)) {
    my($base,$path,$sfx)=fileparse($outputfile,'\..*$');
    $p->outputtables($path, $base);
    my($outfile)="$path$base.pm";
      open(my $OUT,">$outfile")
    or die "Cannot open $outfile for writing.\n";

    compute_lines(\$text, $outfile, $pattern);
    print $OUT $text; #$p->Output(classname  => $name, linenumbers => $linenumbers);
    close $OUT;
  }

  my $x = eval $text;
  $@ and die "Error while compiling your parser: $@\n";
  return $p;
}


1;

__END__

