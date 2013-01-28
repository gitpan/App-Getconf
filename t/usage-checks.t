#!perl -T
#
# check option checks
#

use strict;
use warnings;
use Test::More tests => 4 + 4 + 4;
use App::Getconf qw{:schema};

#-----------------------------------------------------------------------------

sub create_app_getconf {
  my $conf = new App::Getconf();

  $conf->option_schema(
    enum => opt {
      type  => "string",
      check => [qw{first second third}],
    },
    func => opt {
      type  => "string",
      check => sub { /^(first|second|third)$/ },
    },
    pcre => opt {
      type  => "string",
      check => qr/^(first|second|third)$/,
    },
  );

  return $conf;
}

#-----------------------------------------------------------------------------

my $conf;
my $ack;

#-----------------------------------------------------------------------------
# enums

$conf = create_app_getconf();
$ack = eval { $conf->options({ enum => "first" }); "PASSED" };
is($ack, "PASSED", "set correct enum option with config");

$conf = create_app_getconf();
$ack = eval { $conf->cmdline([qw{ --enum second }]); "PASSED" };
is($ack, "PASSED", "set correct enum option with cmdline");

$conf = create_app_getconf();
$ack = eval { $conf->options({ enum => "out-of-scope-1" }); "PASSED" };
is($ack, undef, "set invalid enum option with config");

$conf = create_app_getconf();
$ack = eval { $conf->cmdline([qw{ --enum out-of-scope-2 }]); "PASSED" };
is($ack, undef, "set invalid enum option with cmdline");

#-----------------------------------------------------------------------------
# subs

$conf = create_app_getconf();
$ack = eval { $conf->options({ func => "first" }); "PASSED" };
is($ack, "PASSED", "set correct func-checked option with config");

$conf = create_app_getconf();
$ack = eval { $conf->cmdline([qw{ --func second }]); "PASSED" };
is($ack, "PASSED", "set correct func-checked option with cmdline");

$conf = create_app_getconf();
$ack = eval { $conf->options({ func => "out-of-scope-1" }); "PASSED" };
is($ack, undef, "set invalid func-checked option with config");

$conf = create_app_getconf();
$ack = eval { $conf->cmdline([qw{ --func out-of-scope-2 }]); "PASSED" };
is($ack, undef, "set invalid func-checked option with cmdline");

#-----------------------------------------------------------------------------
# regexps

$conf = create_app_getconf();
$ack = eval { $conf->options({ pcre => "first" }); "PASSED" };
is($ack, "PASSED", "set correct regexp option with config");

$conf = create_app_getconf();
$ack = eval { $conf->cmdline([qw{ --pcre second }]); "PASSED" };
is($ack, "PASSED", "set correct regexp option with cmdline");

$conf = create_app_getconf();
$ack = eval { $conf->options({ pcre => "out-of-scope-1" }); "PASSED" };
is($ack, undef, "set invalid regexp option with config");

$conf = create_app_getconf();
$ack = eval { $conf->cmdline([qw{ --pcre out-of-scope-2 }]); "PASSED" };
is($ack, undef, "set invalid regexp option with cmdline");

#-----------------------------------------------------------------------------
# vim:ft=perl
