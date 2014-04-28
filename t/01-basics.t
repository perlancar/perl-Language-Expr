#!perl

# test various methods in Language::Expr not tested by other tests.

use strict;
use warnings;

use Test::More 0.98;
use Language::Expr;

plan skip_all => 'Regexp::Grammars is currently broken';

my $le = Language::Expr->new;

is($le->eval('"a"x10'), "aaaaaaaaaa", 'eval() in compiled mode works');
is($le->compiler->perl('"a"x10'), '"a" x 10', 'compiler->perl() works');
is($le->perl('"a"x10'), '"a" x 10', 'perl() works');

$le->interpreted(1);
is($le->eval('"a"x10'), "aaaaaaaaaa", 'eval() in interpreted mode works');

my $sub = $le->compile('($_[0]**2 + $_[1]**2)**0.5');
is(ref($sub), 'CODE', 'compile() returns CODEREF');
is($sub->(3, 4), 5, '$_[0], $_[1], etc in compile() works');

is($le->js('2**3'), 'Math.pow(2, 3)', 'js() works');

is($le->php('2**3'), 'pow(2, 3)', 'php() works');

DONE_TESTING:
done_testing;
