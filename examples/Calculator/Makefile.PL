use warnings;
use strict;

use ExtUtils::MakeMaker;

my $PUBLIC_PLACE = "/home/$ENV{USER}/public_html/perlexamples/";
my $MACHINES = 'orion beowulf';

WriteMakefile(
    NAME              => 'Math::Calc',
    VERSION           => '1.0',
    PREREQ_PM         => {}, # e.g., Module::Name => 1.1
    EXE_FILES         => [ 'scripts/calc.pl' ],
    ABSTRACT_FROM     => 'lib/Math/Calc.pod', # retrieve abstract from pod
    AUTHOR            => 'Procesadores de Lenguaje <casiano@ull.es>',
);

sub MY::postamble {

  $_ = targets();
  s/<<PUBLIC_PLACE>>/$PUBLIC_PLACE/ge;
  s/<<MACHINES>>/$MACHINES/ge;
  return $_;
}

sub targets {

  return <<'EOM';
calc: lib/Math/Calc.pm

standalone: lib/Math/Calc.eyp
	eyapp -s -m Math::Calc -o lib/Math/Calc.pm lib/Math/Calc.eyp 

lib/Math/Calc.pm: lib/Math/Calc.eyp
	eyapp -m Math::Calc -o lib/Math/Calc.pm lib/Math/Calc.eyp 

myclean: veryclean
	rm -f Math-Calc-*.tar.gz
	cover -delete

rmcalc: 
	rm -f lib/Math/Calc.pm 

mytest: calc test
	
remotetest: calc dist
	remotetest.pl ${DISTVNAME}.tar.gz <<MACHINES>>

coverage: calc
	cover -delete
	make HARNESS_PERL_SWITCHES=-MDevel::Cover test
	cover

public: calc dist
	cp ${DISTVNAME}.tar.gz <<PUBLIC_PLACE>>
EOM
}

