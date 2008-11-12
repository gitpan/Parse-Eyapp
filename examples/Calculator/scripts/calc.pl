#!/usr/bin/perl -I../lib -I../../../lib -w
use strict;
use Math::Calc;
use Math::Tail qw{uploadfile};
use Getopt::Long;

my $debug = 0;
my $file = '';
my $result = GetOptions (
    "debug!" => \$debug,  
    "file=s" => \$file,
    );

$debug = 0x1F if $debug;
$file = shift if !$file && @ARGV; 

my $prompt = "Expressions. Press CTRL-D (Unix) or CTRL-Z (Windows) to finish:\n";
my $input;
$input = uploadfile($file, $prompt) if $file;
$input = <STDIN> unless $input;

my $parser = Math::Calc->new();
$parser->Run( \$input, $debug );

