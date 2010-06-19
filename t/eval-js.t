#!perl -T

use 5.010;

our $JavaScript_available;
our $JE_available;

BEGIN {
    {
        eval { require JavaScript };
        unless ($@) {
            $JavaScript_available++;
            say "# Using JavaScript Perl module for testing JS emitter";
            last;
        }
        eval { require JE };
        unless ($@) {
            $JE_available++;
            say "# Using JE Perl module for testing JS emitter";
            last;
        }
        require Test::More;
        Test::More::plan(skip_all => "no JavaScript engine available");
    }
}

sub eval_js {
    my ($str) = @_;

    if ($JavaScript_available) {
        state $rt = JavaScript::Runtime->new;
        state $cx = $rt->create_context;
        return $cx->eval($str);
    } elsif ($JE_available) {
        state $je = JE->new;
        return $je->eval($str);
    } else {
        die "BUG: no Javascript engine available";
    }
}

use strict;
use warnings;
use Test::More tests => 1; #154*2 - (7+7+7);
use Test::Exception;
use Language::Expr::Compiler::JS;
use POSIX;
use lib "./t";
require "stdtests.pl";

my $js = new Language::Expr::Compiler::JS;
#$le->var(a => 1, b => 2, 'a b' => 3);
#$le->func(
#    'length' => sub { length(shift) },
#    'floor'  => sub { POSIX::floor(shift) },
#    'ceil'   => sub { POSIX::ceil(shift) },
#);
#package Language::Expr::Compiler::Perl;
#sub floor { POSIX::floor(shift) }
#sub ceil { POSIX::ceil(shift) }
#package main;

for my $t (@{[]}) {
    my @use_itp;

    my $tname = "category=$t->{category} $t->{text}";
    if ($t->{parse_error}) {
        $tname .= ", parse error: $t->{parse_error})";
        throws_ok { eval_js($t->{text}) } $t->{parse_error}, $tname;
    } else {
        $tname .= ", js=q{".$js->js($t->{text})."}";
        if ($t->{run_error}) {
            $tname .= ", run error: $t->{run_error})";
            throws_ok { eval_js($t->{text}) } $t->{run_error}, $tname;
        } elsif ($t->{compiler_run_error}) {
            $tname .= ", run error: $t->{compiler_run_error})";
            throws_ok { eval_js($t->{text}) } $t->{compiler_run_error}, $tname;
        } else {
            $tname .= ")";
            is_deeply( eval_js($t->{text}), $t->{result}, $tname );
        }
    }
}

ok(1);
