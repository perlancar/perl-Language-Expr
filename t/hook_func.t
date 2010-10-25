#!perl -T

use strict;
use warnings;

use Test::More tests => 3;
use Language::Expr::Compiler::Perl;
use Language::Expr::Compiler::PHP;
use Language::Expr::Compiler::JS;

my $plc = new Language::Expr::Compiler::Perl;
$plc->hook_func(sub { "($_[1]*$_[1] + $_[2]*$_[2])" });
is( $plc->perl('pyth($a, 2)'), q[($a*$a + 2*2)], "hook_func in perl" );

my $phpc = new Language::Expr::Compiler::PHP;
$phpc->hook_func(sub { "($_[1]*$_[1] + $_[2]*$_[2] /* $_[0] */)" });
is( $phpc->php('pyth($a, 2)'), q[($a*$a + 2*2 /* pyth */)], "hook_func in php" );

my $jsc = new Language::Expr::Compiler::JS;
$jsc->hook_func(sub { "($_[1]*$_[1] + $_[2]*$_[2])" });
is( $jsc->js('pyth($a, 2)'), q[(a*a + 2*2)], "hook_func in js" );
