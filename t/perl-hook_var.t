#!perl -T

use strict;
use warnings;

use Test::More tests => 1;
use Language::Expr::Compiler::Perl;

my $le = new Language::Expr::Compiler::Perl;
$le->hook_var(sub { "get_var('$_[0]')" });
is( $le->perl('$a+1'), q[get_var('a') + 1], "hook_var" );
