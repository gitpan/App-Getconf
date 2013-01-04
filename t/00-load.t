#!perl -T

use Test::More tests => 1;

BEGIN {
    use_ok( 'App::Getconf' ) || print "Bail out!
";
}

diag( "Testing App::Getconf $App::Getconf::VERSION, Perl $], $^X" );
