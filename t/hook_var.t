#!perl -T

use strict;
use warnings;

use Test::More tests => 3;
use Language::Expr::Compiler::Perl;
use Language::Expr::Compiler::PHP;
use Language::Expr::Compiler::JS;

my $plc = new Language::Expr::Compiler::Perl;
$plc->hook_var(sub { "get_var('$_[0]')" });
is( $plc->perl('$a+1'), q[get_var('a') + 1], "hook_var in perl" );

my $phpc = new Language::Expr::Compiler::PHP;
$phpc->hook_var(sub { "get_var('$_[0]')" });
is( $phpc->php('$a+1'), q[get_var('a') + 1], "hook_var in php" );

my $jsc = new Language::Expr::Compiler::JS;
$jsc->hook_var(sub { "get_var('$_[0]')" });
is( $jsc->js('$a+1'), q[get_var('a') + 1], "hook_var in js" );
