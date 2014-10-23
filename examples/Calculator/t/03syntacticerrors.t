use warnings;
use strict;
use Test::More tests => 2;
BEGIN { use_ok('Math::Calc') };

#########################

my $warnings = '';

local $SIG{__WARN__} = sub { $warnings .= shift };
my $input = q{
a = 2*3
c = ) # error: unexpected )
};
my $parser = Math::Calc->new();
$parser->Run( \$input );

like($warnings, qr{Syntax error near input: '\)' \(lin num 3\).\s+Expected one of these terminals: }, 'syntax error');


