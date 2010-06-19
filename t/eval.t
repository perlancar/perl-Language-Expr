#!perl -T

use strict;
use warnings;

use Test::More tests => 154*2 - (7+7+7);
use Test::Exception;
use Language::Expr;
use POSIX;
use lib "./t";
require "stdtests.pl";

my $le = new Language::Expr;
$Language::Expr::Compiler::Perl::a = 1;
$Language::Expr::Compiler::Perl::b = 2;
$le->var(a => 1, b => 2, 'a b' => 3);

$le->func(
    'length' => sub { length(shift) },
    'floor'  => sub { POSIX::floor(shift) },
    'ceil'   => sub { POSIX::ceil(shift) },
    'rand'   => sub { rand(shift) },
);
package Language::Expr::Compiler::Perl;
sub floor { POSIX::floor(shift) }
sub ceil { POSIX::ceil(shift) }
package main;

for my $t (stdtests()) {
    my @use_itp;

    # currently interpreter doesn't support subexpr yet
    if ($t->{has_subexpr}) {
        @use_itp = (0);
    } else {
        @use_itp = (1, 0);

    }

    for my $use_itp (@use_itp) {
        $le->interpreted($use_itp);
        my $tname = "category=$t->{category} $t->{text} (".
            ($use_itp ? "interpreted" : "compiled");
        if ($t->{parse_error}) {
            $tname .= ", parse error: $t->{parse_error})";
            throws_ok { $le->eval($t->{text}) } $t->{parse_error}, $tname;
        } else {
            $tname .= ", perl=q{".$le->compiler->perl($t->{text})."}"
                unless $use_itp;
            if ($t->{run_error}) {
                $tname .= ", run error: $t->{run_error})";
                throws_ok { $le->eval($t->{text}) } $t->{run_error}, $tname;
            } elsif (!$use_itp && $t->{compiler_run_error}) {
                $tname .= ", run error: $t->{compiler_run_error})";
                throws_ok { $le->eval($t->{text}) } $t->{compiler_run_error}, $tname;
            } elsif ($use_itp && $t->{itp_run_error}) {
                $tname .= ", run error: $t->{itp_run_error})";
                throws_ok { $le->eval($t->{text}) } $t->{itp_run_error}, $tname;
            } else {
                $tname .= ")";
                is_deeply( $le->eval($t->{text}), $t->{result}, $tname );
            }
        }
    }
}

