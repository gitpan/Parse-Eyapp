use Test::More;
eval "use Test::Pod::Coverage";
plan tests => 1;
pod_coverage_ok( "Parse::Eyapp");
