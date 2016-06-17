#!perl

use strict;
use warnings 'FATAL';

use Test::More;
use Test::Exception;
use Language::Expr;
use Language::Expr::JS qw(eval_expr_js);
use Nodejs::Util qw(get_nodejs_path);
use POSIX;
use lib "./t";
require "stdtests.pl";

plan skip_all => "Node.js not available" unless get_nodejs_path();

my $jsc = Language::Expr->new->get_compiler('js');
# add this to code "let a=1; let b=2; let ary1=['one','two','three']; let hash1={one:1, two:2, three:3};";
$jsc->func_mapping->{floor} = 'Math.floor';
$jsc->func_mapping->{ceil}  = 'Math.ceil';
$jsc->func_mapping->{uc}  = '.toUpperCase';
$jsc->func_mapping->{length}  = ':length';

package main;

# currently we need to skip lots of test because boolean quirkiness stuffs
# (should be easy to fix/workaround) and let (will need to upgrade node to
# support let first, and/or use --harmony).

for my $t (stdtests()) {
    next if $t->{category} eq 'var'; # involves let
    next if $t->{category} eq 'dquotestr interpolate var'; #
    next if $t->{category} eq 'comparison equal str'; # involves let
    next if $t->{category} eq 'comparison equal num'; # true & false
    next if $t->{category} eq 'comparison equal chained'; # true & false
    next if $t->{category} eq 'comparison less_greater'; # true & false
    next if $t->{category} eq 'comparison less_greater chained'; # true & false
    next if $t->{category} eq 'or_xor'; # true & false
    next if $t->{category} eq 'true'; # true & false
    next if $t->{category} eq 'unary'; # true & false
    next if $t->{category} eq 'subscripting'; # var, involves let
    next if $t->{category} eq 'grep'; # cannot yet catch js compilation error
    next if $t->{category} eq 'usort'; # cannot yet catch js compilation error
    next if $t->{category} eq 'map'; # cannot yet catch js compilation error

    my $tname = "category=$t->{category} $t->{text}";
    if ($t->{parse_error}) {
        $tname .= ", parse error: $t->{parse_error})";
        throws_ok { eval_expr_js($t->{text}, {js_compiler=>$jsc}) } $t->{parse_error}, $tname;
    } elsif ($t->{run_error}) {
        $tname .= ", run error: $t->{run_error})";
        throws_ok { eval_expr_js($t->{text}, {js_compiler=>$jsc}) } $t->{run_error}, $tname;
    } elsif ($t->{compiler_run_error}) {
        $tname .= ", run error: $t->{compiler_run_error})";
        throws_ok { eval_expr_js($t->{text}, {js_compiler=>$jsc}) } $t->{compiler_run_error}, $tname;
    } else {
        $tname .= ")";
        is_deeply( eval_expr_js($t->{text}, {js_compiler=>$jsc}), $t->{js_result} // $t->{result}, $tname );
    }
}

DONE_TESTING:
done_testing;
