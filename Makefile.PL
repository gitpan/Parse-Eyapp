use warnings;
use strict;
use ExtUtils::MakeMaker;

my @exe_files = qw{eyapp treereg vgg};

WriteMakefile(
    'NAME'         => 'Parse::Eyapp',
    'VERSION_FROM' => 'lib/Parse/Eyapp.pm', # finds $VERSION
    'PREREQ_PM'    => { 
    'List::Util'   => "1.0", 
    'Data::Dumper' => "1.0",
    'Pod::Usage'   => "1.0", 
    },
    'EXE_FILES'    => \@exe_files,
    ABSTRACT_FROM  => 'lib/Parse/Eyapp.pod', 
    AUTHOR         => 'Casiano Rodriguez-Leon <casiano@ull.es>',
);

sub MY::postamble {

  return '' unless defined($ENV{DEVELOPER}) && ($ENV{DEVELOPER} eq 'casiano');

  require File::Basename;
  
  my $listwithsuffix = sub {
    my $suffix = shift;
    my $lws =  "@_";

    $lws =~ s/(\S+)/$1$suffix/xg;
    return  $lws;
  };

  ### Configuration variables: Change them if you change machine ####
  ### Depend on environment variables set via "source etc/setperl5lib" ##

  my ($PUBLIC_PLACE, $MACHINES, $REMOTETEST);

  ################# END configuration variables #####################

  my ($manifest, @PODS, @BASES, @NOEXT);

  open($manifest ,'<', 'MANIFEST') or die "Can't find MANIFEST";

    @PODS = grep { m{lib/Parse/.*\.pod$}x } <$manifest>;

  close($manifest) or return '';

  chomp(@PODS); # i.e. ( lib/Parse/Eyapp/Base.pod, lib/Parse/Eyapp/debuggingtut.pod, lib/Parse/Eyapp/Driver.pod ... )

  return '' unless @PODS;

  my ($PODS, $TEXS, $DVIS, $PDFS, $HTMLS);

  # Directory where to publish documentation.
  $PUBLIC_PLACE = $ENV{PUBLIC_PLACE} || "/home/$ENV{USER}/public_html/perlexamples/";

  # Run 'make test' in those machines via SSH
  $MACHINES = $ENV{MACHINES} || '127.0.0.1 127.0.0.2';

  # Where the remote.pl program is:
  # Be sure is in the path
  $REMOTETEST = $ENV{REMOTETEST} || 'remotetest.pl';

  @BASES = map { File::Basename::basename($_, '.pod') } @PODS; # i. e. ( Base.pod, debuggingtut.pod, ... )
  @NOEXT = map { m{(.*)\.pod$}; $1 } @PODS;    # i. e. ( lib/Parse/Eyapp/Base, lib/Parse/Eyapp/debuggingtut, lib/Parse/Eyapp/Driver ... )

  $PODS = "@PODS";

  $TEXS =  $listwithsuffix->(".tex", @BASES);

  $DVIS =  $listwithsuffix->(".dvi", @BASES);

  $PDFS =  $listwithsuffix->(".pdf", @BASES);

  $HTMLS = $listwithsuffix->(".html", @NOEXT);

  $_ = targets(
    PODS         => $PODS, 
    TEXS         => $TEXS, 
    DVIS         => $DVIS, 
    PDFS         => $PDFS, 
    HTMLS        => $HTMLS, 
    PUBLIC_PLACE => $PUBLIC_PLACE, 
    MACHINES     => $MACHINES,
    REMOTETEST   => $REMOTETEST
  );


  return $_;
}

sub targets {
  my %var = @_;

  $_ = <<'EOSQT';
PLTAGS=/usr/share/vim/vim71/tools/pltags.pl

PODS=<<PODS>>

TEXS=<<TEXS>>

DVIS=<<DVIS>>

PDFS=<<PDFS>>

HTMLS=<<HTMLS>>

publicdist: docclean release forcepods publichtml publicpdf dist 
	chmod a+r Parse-Eyapp-*.tar.gz
	cp Parse-Eyapp-*.tar.gz <<PUBLIC_PLACE>>Parse-Eyapp.tar.gz
	cp Parse-Eyapp-*.tar.gz /tmp/
	chmod a+r <<PUBLIC_PLACE>>Parse-Eyapp.tar.gz

release: lib/Parse/Eyapp/Parse.yp
	   yapp -n -m 'Parse::Eyapp::Parse' -o 'lib/Parse/Eyapp/Parse.pm' lib/Parse/Eyapp/Parse.yp
	   PERL5LIB=lib; eyapp -n -m 'Parse::Eyapp::Parse' -o 'lib/Parse/Eyapp/Parse.pm' lib/Parse/Eyapp/Parse.yp
	   PERL5LIB=lib; $(PERL) -I./lib eyapp -v -n -m Parse::Eyapp::Treeregparser -o lib/Parse/Eyapp/Treeregexp.pm lib/Parse/Eyapp/Treeregexp.yp
	   rm -fR Parse-Eyapp-*.tar.gz

coverage:
	cover -delete
	make HARNESS_PERL_SWITCHES=-MDevel::Cover test
	cover

remotetest: ${DISTVNAME}.tar.gz
	<<REMOTETEST>> ${DISTVNAME}.tar.gz <<MACHINES>>

docclean:
	rm -f *.tex *.dvi *.pdf *.toc *.ind *.ilg *.aux *.html *.log *.idx

lib/Parse/Eyapp.pod: tt2/Eyapp.tt2
	cd tt2; tpage  Eyapp.tt2 > ../lib/Parse/Eyapp.pod

lib/Parse/Eyapp/%.pod:tt2/%.tt2
	cd tt2; tpage  $*.tt2 > ../$@

pods: ${PODS}

forcepods: 
	touch tt2/*.tt2
	make pods

%.dvi:%.tex
	latex $*.tex; makeindex $*; latex $*.tex

dvis: ${DVIS}

Eyapp.tex:lib/Parse/Eyapp.pod
	pod2latex -full lib/Parse/Eyapp.pod

%.tex:lib/Parse/Eyapp/%.pod
	pod2latex -full lib/Parse/Eyapp/$*.pod

texs: ${TEXS}

treereg.html:treereg
	mpod2html -nowarnings -noverbose -nonavigation -nobanner -noidx -notoc treereg  -o $@

eyapp.html:eyapp
	mpod2html -nowarnings -noverbose -nonavigation -nobanner -noidx -notoc eyapp  -o $@

vgg.html:vgg
	mpod2html -nowarnings -noverbose -nonavigation -nobanner -noidx -notoc vgg  -o $@

%.html:%.pod
	mpod2html -nowarnings -noverbose -nonavigation -nobanner -noidx -notoc $*.pod  -o $@
	#perl -e 's/^Cannot find .*//gi; s/\A\s+//' -p -i.bak $@

htmls: ${HTMLS} eyapp.html treereg.html vgg.html

<<PUBLIC_PLACE>>%.pdf:%.pdf
	cp $*.pdf <<PUBLIC_PLACE>>
	chmod a+r <<PUBLIC_PLACE>>*.pdf

%.pdf:%.dvi
	dvipdfm $*.dvi 
	#rm -f $*.aux  $*.dvi  $*.idx  $*.ilg  $*.ind  $*.log  $*.tex  $*.toc

pdfs: ${PDFS}

publicpdf: pdfs
	cp ${PDFS} <<PUBLIC_PLACE>>
	chmod a+r <<PUBLIC_PLACE>>*.pdf

publichtml: htmls
	cp ${HTMLS} eyapp.html treereg.html vgg.html <<PUBLIC_PLACE>>
	chmod a+r <<PUBLIC_PLACE>>/*.html
	
.PHONY: tags
tags: 
	${PLTAGS} eyapp treereg vgg lib/Parse/Eyapp/*.pm lib/Parse/Eyapp/*.yp lib/Parse/Eyapp.pm 
	ctags -a --exclude=examples --exclude=t --exclude=doc --exclude=tt2
EOSQT

  # Replace variables
  # TODO: Check that all <<variables>> are present in @_
  for my $k (keys(%var)) {
	  my $r = $var{$k};
    s/<<$k>>/$r/ge;
  }

  return $_;
}

__END__

=head1 NAME 

Makefile.PL - Makefile generator for Parse::Eyapp. Developer notes

=head1 SYNOPSIS

=over 2

=item * To build a 'public release'

  make release

=item * Before realising a distribution in CPAN via PAUSE do:

  make publicdist

Remember to change the version number in L<Parse::Eyapp::Driver>
and L<Parse::Eyapp>

=item * To build the *.html files:

   make html 

=item * To build pdf files:

   make pdfs

=item * To build dvi files:

   make dvis

=item * To make public the *.html files:

   make publichtml 

=item * To make public pdf files:

   make publicpdf

=item * To delete generated docs (*.html, *.pdf, etc.)

   make docclean

=item * To run the tests on remote machines

   make remotetest

(Set the environment variable C<MACHINES>)

=item * To use L<Devel::Cover> to study test coverage:

   make coverage

=back

=head1 DEPENDENCIES

To use this C<Makefile.PL> as developer is convenient to have installed:

=over 2

=item * Template toolkit

=item * Pod2LaTeX

=item * LaTeX

=item * mpod2html

=item * Devel::Cover

=item * Parse::Yapp

=back

=head1 AUTHOR
 
Casiano Rodriguez-Leon (casiano@ull.es)

=head1 LICENCE AND COPYRIGHT
 
Copyright (c) 2006-2008 Casiano Rodriguez-Leon (casiano@ull.es). All rights reserved.

These modules are free software; you can redistribute it and/or
modify it under the same terms as Perl itself. See L<perlartistic>.
 
This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 



