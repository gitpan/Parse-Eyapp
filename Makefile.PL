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
	'EXE_FILES' => [ 'eyapp', 'treereg', 'eyapptut' ],
);

sub MY::postamble {
return <<'EOT';
release: lib/Parse/Eyapp/Parse.yp
	PERL5LIB=lib; eyapp -n -m 'Parse::Eyapp::Parse' -o 'lib/Parse/Eyapp/Parse.pm' lib/Parse/Eyapp/Parse.yp
	PERL5LIB=lib; $(PERL) -I./lib eyapp -v -n -m Parse::Eyapp::Treeregparser -o lib/Parse/Eyapp/Treeregexp.pm lib/Parse/Eyapp/Treeregexp.yp

coverage:
	cover -delete
	make HARNESS_PERL_SWITCHES=-MDevel::Cover test
	cover

publicdist: release html pdf dist
	chmod a+r Parse-Eyapp-*.tar.gz
	cp Parse-Eyapp-*.tar.gz /home/pl/public_html/perlexamples/

xdvi: 
	pod2latex -full -prefile preamble.tex lib/Parse/Eyapp.pm 
	pod2latex -full -prefile tutpreamble.tex eyapptut 
	latex Eyapp.tex; makeindex Eyapp; latex Eyapp.tex 
	latex eyapptut.tex; makeindex eyapptut; latex eyapptut.tex 

pdf: xdvi
	dvipdfm Eyapp.dvi
	rm -f Eyapp.toc Eyapp.log Eyapp.idx Eyapp.aux
	mv Eyapp.pdf /home/pl/public_html/perlexamples/
	chmod a+r /home/pl/public_html/perlexamples/Eyapp.pdf
	dvipdfm eyapptut.dvi
	rm -f eyapptut.toc eyapptut.log eyapptut.idx eyapptut.aux
	mv eyapptut.pdf /home/pl/public_html/perlexamples/
	chmod a+r /home/pl/public_html/perlexamples/eyapptut.pdf

html:
	#rm -f Eyapp.html podindex.html podtoc.html
	mpod2html -nowarnings -noverbose -nonavigation -nobanner -noidx -notoc eyapp treereg lib/Parse/Eyapp.pm 
	mpod2html -nowarnings -noverbose -nonavigation -nobanner -noidx -notoc eyapp treereg eyapptut 
	perl -e 's/^Cannot find page .*//gi' -i.bak -p Eyapp.html 
	perl -e 's/^Cannot find page .*//gi' -i.bak -p eyapptut.html 
	mv Eyapp.html /home/pl/public_html/perlexamples/
	chmod a+r /home/pl/public_html/perlexamples/Eyapp.html
	mv eyapptut.html /home/pl/public_html/perlexamples/
	chmod a+r /home/pl/public_html/perlexamples/eyapptut.html
EOT
}