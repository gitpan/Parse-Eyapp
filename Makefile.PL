use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'	=> 'Parse::Eyapp',
    'VERSION_FROM' => 'lib/Parse/Eyapp/Driver.pm', # finds $VERSION
    #'MAN3PODS' => { 'lib/Parse/Eyapp/Tutorial.pod' => '$(INST_MAN3DIR)/Parse::Eyapp.3' },
    'PREREQ_PM' => { 
                     List::Util   => "1.0", 
                     List::MoreUtils => "0",
                     Data::Dumper => "1.0",
                     Pod::Usage   => "1.0", 
                     # Test::Warn => "0.08", # recommended
                     # Test::Exception => "0.2", # recommended
                     # Test::Pod => "1.0", # recommended
                   },
    'PM' => {
              'lib/Parse/Eyapp.pm' => '$(INST_LIBDIR)/Eyapp.pm',
              'lib/Parse/Eyapp/Base.pm' => '$(INST_LIBDIR)/Eyapp/Base.pm',
              'lib/Parse/Eyapp/Options.pm' => '$(INST_LIBDIR)/Eyapp/Options.pm',
              'lib/Parse/Eyapp/Driver.pm' => '$(INST_LIBDIR)/Eyapp/Driver.pm',
              'lib/Parse/Eyapp/Grammar.pm' => '$(INST_LIBDIR)/Eyapp/Grammar.pm',
              'lib/Parse/Eyapp/Lalr.pm' => '$(INST_LIBDIR)/Eyapp/Lalr.pm',
              'lib/Parse/Eyapp/Output.pm' => '$(INST_LIBDIR)/Eyapp/Output.pm',
              'lib/Parse/Eyapp/Parse.pm' => '$(INST_LIBDIR)/Eyapp/Parse.pm',
              'lib/Parse/Eyapp/YATW.pm' => '$(INST_LIBDIR)/Eyapp/YATW.pm',
              'lib/Parse/Eyapp/Treeregexp.pm' => '$(INST_LIBDIR)/Eyapp/Treeregexp.pm',
              'lib/Parse/Eyapp/Scope.pm' => '$(INST_LIBDIR)/Eyapp/Scope.pm',
              'lib/Parse/Eyapp/_TreeregexpSupport.pm' => '$(INST_LIBDIR)/Eyapp/_TreeregexpSupport.pm',
              'lib/Parse/Eyapp/Node.pm' => '$(INST_LIBDIR)/Eyapp/Node.pm',
            },
	'EXE_FILES' => [ 'eyapp', 'treereg' ],
      ($] >= 5.005 ?     
            (
               ABSTRACT_FROM  => 'lib/Parse/Eyapp.pod', 
               AUTHOR         => 'Casiano Rodriguez-Leon <casiano@ull.es>',
             ) 
             : ()
       ),
);

sub MY::postamble {
return <<'EOT';
release: lib/Parse/Eyapp/Parse.yp
	PERL5LIB=lib; eyapp -n -m 'Parse::Eyapp::Parse' -o 'lib/Parse/Eyapp/Parse.pm' lib/Parse/Eyapp/Parse.yp
	PERL5LIB=lib; $(PERL) -I./lib eyapp -v -n -m Parse::Eyapp::Treeregparser -o lib/Parse/Eyapp/Treeregexp.pm lib/Parse/Eyapp/Treeregexp.yp
	rm -fR Parse-Eyapp-*.tar.gz

doc:tt2/Eyapp.tt2 
	cd tt2; tpage  Eyapp.tt2 > ../lib/Parse/Eyapp.pod
	cd tt2; tpage  Treeregexp.tt2 > ../lib/Parse/Eyapp/Treeregexp.pod
	cd tt2; tpage  Scope.tt2 > ../lib/Parse/Eyapp/Scope.pod
	cd tt2; tpage  Node.tt2 > ../lib/Parse/Eyapp/Node.pod
	cd tt2; tpage  YATW.tt2 > ../lib/Parse/Eyapp/YATW.pod
	cd tt2; tpage  Driver.tt2 > ../lib/Parse/Eyapp/Driver.pod
	cd tt2; tpage  Parse.tt2 > ../lib/Parse/Eyapp/Parse.pod

coverage:
	cover -delete
	make HARNESS_PERL_SWITCHES=-MDevel::Cover test
	cover

publicdist: release html pdf dist
	chmod a+r Parse-Eyapp-*.tar.gz
	cp Parse-Eyapp-*.tar.gz /home/pl/public_html/perlexamples/Parse-Eyapp.tar.gz
	chmod a+r /home/pl/public_html/perlexamples/Parse-Eyapp.tar.gz

xdvi: doc
	pod2latex -full -prefile preamble.tex lib/Parse/Eyapp.pod
	latex Eyapp.tex; makeindex Eyapp; latex Eyapp.tex 

pdf: xdvi
	dvipdfm Eyapp.dvi
	rm -f Eyapp.toc Eyapp.log Eyapp.idx Eyapp.aux
	mv Eyapp.pdf /home/pl/public_html/perlexamples/
	chmod a+r /home/pl/public_html/perlexamples/Eyapp.pdf

html: doc
	#rm -f Eyapp.html podindex.html podtoc.html
	mpod2html -nowarnings -noverbose -nonavigation -nobanner -noidx -notoc eyapp treereg \
                          lib/Parse/Eyapp.pod \
													lib/Parse/Eyapp/Treeregexp.pod  \
                          lib/Parse/Eyapp/Scope.pod \
													lib/Parse/Eyapp/Node.pod \
                          lib/Parse/Eyapp/Driver.pod \
                          lib/Parse/Eyapp/YATW.pod\
                          lib/Parse/Eyapp/Parse.pod
	perl -e 's/^Cannot find page .*//gi' -i.bak -p Eyapp.html 
	perl -e 's/^Cannot find page .*//gi' -i.bak -p eyapp.html 
	perl -e 's/^Cannot find page .*//gi' -i.bak -p treereg.html 
	mv Eyapp.html /home/pl/public_html/perlexamples/
	chmod a+r /home/pl/public_html/perlexamples/Eyapp.html
	mv eyapp.html /home/pl/public_html/perlexamples/
	chmod a+r /home/pl/public_html/perlexamples/eyapp.html
	mv treereg.html /home/pl/public_html/perlexamples/
	chmod a+r /home/pl/public_html/perlexamples/treereg.html
EOT
}
