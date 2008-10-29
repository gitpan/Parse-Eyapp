use warnings;
use strict;
use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.

### Configuration variables: Change them if you change machine ###

# Values in nereida

# Directory where to publish file.

#my $PUBLIC_PLACE = '/home/pl/public_html/perlexamples/';
my $PUBLIC_PLACE = "/home/$ENV{USER}/public_html/perlexamples/";

# Run 'make test' in those machines via SSH
my $MACHINES = $ENV{MACHINES} || 'orion beowulf';

# Where the remote.pl program is:
#my $REMOTETEST = '/home/pp2/LGRID_Machine/scripts/remotetest.pl';
# Be sure is in the path
my $REMOTETEST = 'remotetest.pl';

################# END configuration variables #####################

WriteMakefile(
    'NAME'	=> 'Parse::Eyapp',
    'VERSION' => '1.123', # finds $VERSION
#    'MAN3PODS' => { 
#                    #'lib/Parse/Eyapp/Tutorial.pod' => '$(INST_MAN3DIR)/Parse::Eyapp.3' 
#		    'lib/Parse/Eyapp.pod' => '$(INST_MAN3DIR)/Parse::Eyapp.3',
#		    'lib/Parse/Eyapp/Base.pod' => '$(INST_MAN3DIR)/Parse::Eyapp::Base.3',
#		    'lib/Parse/Eyapp/debuggingtut.pod' => '$(INST_MAN3DIR)/Parse::Eyapp::debuggingtut.3',
#		    'lib/Parse/Eyapp/Driver.pod' => '$(INST_MAN3DIR)/Parse::Eyapp::Driver.3',
#		    'lib/Parse/Eyapp/eyappintro.pod' => '$(INST_MAN3DIR)/Parse::Eyapp::eyappintro.3',
#		    'lib/Parse/Eyapp/eyapplanguageref.pod' => '$(INST_MAN3DIR)/Parse::Eyapp::eyapplanguageref.3',
#		    'lib/Parse/Eyapp/languageintro.pod' => '$(INST_MAN3DIR)/Parse::Eyapp::languageintro.3',
#		    'lib/Parse/Eyapp/MatchingTrees.pod' => '$(INST_MAN3DIR)/Parse::Eyapp::MatchingTrees.3',
#		    'lib/Parse/Eyapp/Node.pod' => '$(INST_MAN3DIR)/Parse::Eyapp::Node.3',
#		    'lib/Parse/Eyapp/Parse.pod' => '$(INST_MAN3DIR)/Parse::Eyapp::Parse.3',
#		    'lib/Parse/Eyapp/Scope.pod' => '$(INST_MAN3DIR)/Parse::Eyapp::Scope.3',
#		    'lib/Parse/Eyapp/translationschemestut.pod' => '$(INST_MAN3DIR)/Parse::Eyapp::translationschemestut.3',
#		    'lib/Parse/Eyapp/Treeregexp.pod' => '$(INST_MAN3DIR)/Parse::Eyapp::Treeregexp.3',
#		    'lib/Parse/Eyapp/YATW.pod' => '$(INST_MAN3DIR)/Parse::Eyapp::YATW.3',
#                  },
    'PREREQ_PM' => { 
                     'List::Util'   => "1.0", 
                     'Data::Dumper' => "1.0",
                     'Pod::Usage'   => "1.0", 
                     # Test::Warn => "0.08", # recommended
                     # Test::Exception => "0.2", # recommended
                     # Test::Pod => "1.0", # recommended
                   },
#    'PM' => {
#              'lib/Parse/Eyapp.pm' => '$(INST_LIBDIR)/Eyapp.pm',
#              'lib/Parse/Eyapp/Base.pm' => '$(INST_LIBDIR)/Eyapp/Base.pm',
#              'lib/Parse/Eyapp/Options.pm' => '$(INST_LIBDIR)/Eyapp/Options.pm',
#              'lib/Parse/Eyapp/Driver.pm' => '$(INST_LIBDIR)/Eyapp/Driver.pm',
#              'lib/Parse/Eyapp/Grammar.pm' => '$(INST_LIBDIR)/Eyapp/Grammar.pm',
#              'lib/Parse/Eyapp/Lalr.pm' => '$(INST_LIBDIR)/Eyapp/Lalr.pm',
#              'lib/Parse/Eyapp/Output.pm' => '$(INST_LIBDIR)/Eyapp/Output.pm',
#              'lib/Parse/Eyapp/Parse.pm' => '$(INST_LIBDIR)/Eyapp/Parse.pm',
#              'lib/Parse/Eyapp/YATW.pm' => '$(INST_LIBDIR)/Eyapp/YATW.pm',
#              'lib/Parse/Eyapp/Treeregexp.pm' => '$(INST_LIBDIR)/Eyapp/Treeregexp.pm',
#              'lib/Parse/Eyapp/Scope.pm' => '$(INST_LIBDIR)/Eyapp/Scope.pm',
#              'lib/Parse/Eyapp/_TreeregexpSupport.pm' => '$(INST_LIBDIR)/Eyapp/_TreeregexpSupport.pm',
#              'lib/Parse/Eyapp/Node.pm' => '$(INST_LIBDIR)/Eyapp/Node.pm',
#            },
	'EXE_FILES' => [ 'eyapp', 'treereg', 'vgg' ],
   ABSTRACT_FROM  => 'lib/Parse/Eyapp.pod', 
   AUTHOR         => 'Casiano Rodriguez-Leon <casiano@ull.es>',
);

sub MY::postamble {

  $_ = targets();
  s/<<PUBLIC_PLACE>>/$PUBLIC_PLACE/ge;
  s/<<MACHINES>>/$MACHINES/ge;
  s/<<REMOTETEST>>/$REMOTETEST/ge;
  return $_;
}


sub targets {
  return <<'EOSQT';
release: lib/Parse/Eyapp/Parse.yp
	PERL5LIB=lib; eyapp -n -m 'Parse::Eyapp::Parse' -o 'lib/Parse/Eyapp/Parse.pm' lib/Parse/Eyapp/Parse.yp
	PERL5LIB=lib; $(PERL) -I./lib eyapp -v -n -m Parse::Eyapp::Treeregparser -o lib/Parse/Eyapp/Treeregexp.pm lib/Parse/Eyapp/Treeregexp.yp
	rm -fR Parse-Eyapp-*.tar.gz

doc:tt2/Eyapp.tt2 tt2/Treeregexp.tt2 tt2/Scope.tt2 tt2/Node.tt2 tt2/YATW.tt2 tt2/Driver.tt2 tt2/Parse.tt2 tt2/Base.tt2 tt2/debuggingtut.tt2 tt2/eyapplanguageref.tt2 tt2/eyappintro.tt2 tt2/translationschemestut.tt2
	cd tt2; tpage  Eyapp.tt2 > ../lib/Parse/Eyapp.pod
	cd tt2; tpage  Treeregexp.tt2 > ../lib/Parse/Eyapp/Treeregexp.pod
	cd tt2; tpage  Scope.tt2 > ../lib/Parse/Eyapp/Scope.pod
	cd tt2; tpage  Node.tt2 > ../lib/Parse/Eyapp/Node.pod
	cd tt2; tpage  YATW.tt2 > ../lib/Parse/Eyapp/YATW.pod
	cd tt2; tpage  Driver.tt2 > ../lib/Parse/Eyapp/Driver.pod
	cd tt2; tpage  Parse.tt2 > ../lib/Parse/Eyapp/Parse.pod
	cd tt2; tpage  Base.tt2 > ../lib/Parse/Eyapp/Base.pod
	cd tt2; tpage  debuggingtut.tt2 > ../lib/Parse/Eyapp/debuggingtut.pod
	cd tt2; tpage  eyapplanguageref.tt2 > ../lib/Parse/Eyapp/eyapplanguageref.pod
	cd tt2; tpage  eyappintro.tt2 > ../lib/Parse/Eyapp/eyappintro.pod
	cd tt2; tpage  translationschemestut.tt2 > ../lib/Parse/Eyapp/translationschemestut.pod
	cd tt2; tpage  MatchingTrees.tt2 > ../lib/Parse/Eyapp/MatchingTrees.pod

coverage:
	cover -delete
	make HARNESS_PERL_SWITCHES=-MDevel::Cover test
	cover

publicdist: release html pdf dist 
	chmod a+r Parse-Eyapp-*.tar.gz
	cp Parse-Eyapp-*.tar.gz <<PUBLIC_PLACE>>Parse-Eyapp.tar.gz
	cp Parse-Eyapp-*.tar.gz /tmp/
	chmod a+r <<PUBLIC_PLACE>>Parse-Eyapp.tar.gz

xdvi: doc
	pod2latex -full -prefile preamble.tex lib/Parse/Eyapp.pod
	latex Eyapp.tex; makeindex Eyapp; latex Eyapp.tex 
	pod2latex -full lib/Parse/Eyapp/eyapplanguageref.pod
	latex eyapplanguageref.tex; makeindex eyapplanguageref; latex eyapplanguageref.tex 
	pod2latex -full lib/Parse/Eyapp/eyappintro.pod
	latex eyappintro.tex; makeindex eyappintro; latex eyappintro.tex 
	pod2latex -full lib/Parse/Eyapp/debuggingtut.pod
	latex debuggingtut.tex; makeindex debuggingtut; latex debuggingtut.tex 
	pod2latex -full lib/Parse/Eyapp/Treeregexp.pod
	latex Treeregexp.tex; makeindex Treeregexp; latex Treeregexp.tex 
	pod2latex -full lib/Parse/Eyapp/Node.pod
	latex Node.tex; makeindex Node; latex Node.tex 
	pod2latex -full lib/Parse/Eyapp/YATW.pod
	latex YATW.tex; makeindex YATW; latex YATW.tex 
	pod2latex -full lib/Parse/Eyapp/translationschemestut.pod
	latex translationschemestut.tex; makeindex translationschemestut; latex translationschemestut.tex 
	pod2latex -full lib/Parse/Eyapp/Base.pod
	latex Base.tex; makeindex Base; latex Base.tex 
	pod2latex -full lib/Parse/Eyapp/Driver.pod
	latex Driver.tex; makeindex Driver; latex Driver.tex 
	pod2latex -full lib/Parse/Eyapp/MatchingTrees.pod
	latex MatchingTrees.tex; makeindex MatchingTrees; latex MatchingTrees.tex 

pdf: xdvi
	dvipdfm Eyapp.dvi
	dvipdfm Treeregexp.dvi
	dvipdfm debuggingtut.dvi
	dvipdfm eyapplanguageref.dvi
	dvipdfm eyappintro.dvi
	dvipdfm Node.dvi
	dvipdfm YATW.dvi
	dvipdfm translationschemestut.dvi
	dvipdfm Base.dvi
	dvipdfm Driver.dvi
	dvipdfm MatchingTrees.dvi
	rm -f *.toc *.log *.idx *.aux
	mv Eyapp.pdf Treeregexp.pdf \
               debuggingtut.pdf \
               eyapplanguageref.pdf \
               eyappintro.pdf \
               Node.pdf \
               YATW.pdf \
               translationschemestut.pdf \
               Base.pdf \
               Driver.pdf \
               MatchingTrees.pdf \
     <<PUBLIC_PLACE>>
	chmod a+r <<PUBLIC_PLACE>>*.pdf

html: doc
	#rm -f Eyapp.html podindex.html podtoc.html
	mpod2html -nowarnings -noverbose -nonavigation -nobanner -noidx -notoc\
                          lib/Parse/Eyapp.pod \
													lib/Parse/Eyapp/Treeregexp.pod  \
                          lib/Parse/Eyapp/Scope.pod \
													lib/Parse/Eyapp/Node.pod \
                          lib/Parse/Eyapp/Driver.pod \
                          lib/Parse/Eyapp/YATW.pod\
                          lib/Parse/Eyapp/Parse.pod\
                          lib/Parse/Eyapp/debuggingtut.pod\
                          lib/Parse/Eyapp/eyapplanguageref.pod\
                          lib/Parse/Eyapp/eyappintro.pod\
                          lib/Parse/Eyapp/Base.pod\
                          lib/Parse/Eyapp/Driver.pod\
                          eyapp\
                          treereg\
                          lib/Parse/Eyapp/MatchingTrees.pod\
                          lib/Parse/Eyapp/translationschemestut.pod
	#perl -e 's/^Cannot find page .*//gi' -i.bak -p Eyapp.html 
	#perl -e 's/^Cannot find page .*//gi' -i.bak -p eyapp.html 
	#perl -e 's/^Cannot find page .*//gi' -i.bak -p treereg.html 
	mv Eyapp.html <<PUBLIC_PLACE>>
	chmod a+r <<PUBLIC_PLACE>>Eyapp.html
	mv eyapp.html <<PUBLIC_PLACE>>
	chmod a+r <<PUBLIC_PLACE>>eyapp.html
	mv treereg.html <<PUBLIC_PLACE>>
	chmod a+r <<PUBLIC_PLACE>>treereg.html
	mv translationschemestut.html <<PUBLIC_PLACE>>
	chmod a+r <<PUBLIC_PLACE>>translationschemestut.html
	mv Base.html <<PUBLIC_PLACE>>
	chmod a+r <<PUBLIC_PLACE>>Base.html
	mv Driver.html <<PUBLIC_PLACE>>
	chmod a+r <<PUBLIC_PLACE>>Driver.html
	mv Node.html <<PUBLIC_PLACE>>
	chmod a+r <<PUBLIC_PLACE>>Node.html
	mv YATW.html <<PUBLIC_PLACE>>
	chmod a+r <<PUBLIC_PLACE>>YATW.html
	mv MatchingTrees.html <<PUBLIC_PLACE>>
	chmod a+r <<PUBLIC_PLACE>>MatchingTrees.html

remotetest:
	<<REMOTETEST>> ${DISTVNAME}.tar.gz <<MACHINES>>
EOSQT
}
