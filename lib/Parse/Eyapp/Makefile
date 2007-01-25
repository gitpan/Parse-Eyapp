all:Treeregexp.pm Parse.pm

Treeregexp.pm: Treeregexp.yp Parse.pm _TreeregexpSupport.pm Base.pm
	eyapp -v -n -m Parse::Eyapp::Treeregparser -o Treeregexp.pm Treeregexp.yp
	perl -c Treeregexp.pm

Parse.pm: Parse.yp Driver.pm Grammar.pm Lalr.pm Options.pm Output.pm Node.pm YATW.pm Makefile Base.pm
	yapp -v -n -m Parse::Eyapp::Parse Parse.yp
	perl -c Parse.pm

release: Parse.yp Driver.pm Grammar.pm Lalr.pm Options.pm Output.pm Node.pm YATW.pm Makefile
	eyapp -v -m Parse::Eyapp::Parse Parse.yp
	perl -c Parse.pm

clean:
	rm -f Parse.pm *.pdf

pdf:
	rm -f /tmp/eyapp.pdf
	atopdf --portrait --columns=1 Parse.yp Treeregexp.yp \
	                              Driver.pm Grammar.pm TranslationScheme.pm \
				      Node.pm Options.pm Output.pm YATW.pm\
				      -o /tmp/eyapp.pdf
