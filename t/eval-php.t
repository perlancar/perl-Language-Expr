#!perl -T

use 5.010;
use strict;
use warnings;

our $PHP_pm_available;
our $PHP_Interpreter_pm_available;
our $PHP_bin;

BEGIN {
    {
        eval { require PHP };
        $PHP_pm_available++ unless $@;

        eval { require PHP::Interpreter };
        $PHP_Interpreter_pm_available++ unless $@;

        eval {
            require File::Which;
            my @paths = File::Which::which("php");
            ($ENV{PATH}) = $ENV{PATH} =~ /(.*)/;
            for (@paths) {
                #print "# DEBUG Testing $_ ...\n";
                ($_) = /(.*)/;
                my $output = qx($_ -r 'echo json_encode(call_user_func(function() { return array(1+1); }));') or die;
                #print "# DEBUG Output: $output\n";
                if ($output =~ /\A\[2\]$/m) {
                    $PHP_bin = $_;
                    last;
                }
            }
        };
        $@ and die $@;

        # I haven't tested with PHP or PHP::Interpreter, but last time
        # I checked one of them is still at PHP 5.1. Also the lack of
        # JSON encoder/decoder.
        #last if $PHP_pm_available || $PHP_Interpreter_pm_available || $PHP_bin;
        last if $PHP_bin;

        require Test::More;
        Test::More::plan(skip_all => "no usable PHP interpreters available");
    }
}

use Test::More tests => 155;
use Test::Exception;
use Data::Walk;
use JSON;
use Language::Expr::Compiler::PHP;
use String::ShellQuote;
use lib "./t";
require "stdtests.pl";

sub eval_in_php($$) {
    my ($phpcomp, $str) = @_;

    $str = $phpcomp->php($str);

    state $php_itp;

    if (!$php_itp) {
        if ($ENV{TEST_PHP_INTERPRETER}) {
            $php_itp = $ENV{TEST_PHP_INTERPRETER};
        } else {
            if ($PHP_bin || $ENV{TEST_PHP_INTERPRETER_BIN}) {
                $php_itp = "bin";
                $PHP_bin = $ENV{TEST_PHP_INTERPRETER_BIN} if $ENV{TEST_PHP_INTERPRETER_BIN};
            } elsif ($PHP_pm_available) {
                $php_itp = "PHP.pm";
            } elsif ($PHP_Interpreter_pm_available) {
                $php_itp = "PHP::Interpreter";
            }
        }
        print "# Choosing $php_itp".($php_itp eq 'bin' ? " ($PHP_bin)": '')." as PHP interpreter\n";
    }

    if (0) {
    #if ($php_itp eq 'PHP.pm') {
    #    ...
    #} elsif ($php_itp eq 'PHP::Interpreter') {
    #    ...
    } elsif ($php_itp eq 'bin') {
        my $cmd = qq($PHP_bin -r ).shell_quote(qq($phpcomp->{_phpcode_prefix}echo json_encode($str);)).qq( 2>&1);
        print "# DEBUG: $cmd\n";
        my $output;
        $output = qx($cmd);
        $output =~ /(parse|syntax) error/i and die "syntax error/invalid syntax: cmd=$cmd, output=$output";
        #$output =~ /\b([A-Z]\w+Error)\b/i and die "javascript error: $1: cmd=$cmd, output=$output";
        $? and die "Can't execute $cmd successfully: $! ($?)";
        return convert_json_booleans(JSON->new->allow_nonref->decode($output));
    } else {
        die "BUG: Can't test yet with PHP interpreter `$php_itp`!";
    }
}

sub convert_json_booleans {
    my $arg = shift;
    walk sub { bless $_, 'boolean' if ref($_) eq 'JSON::PP::Boolean' }, $arg;
    $arg;
}

my $phpcomp = new Language::Expr::Compiler::PHP;
$phpcomp->{_phpcode_prefix} = '$a=1; $b=2; ';
$phpcomp->func_mapping->{uc}  = 'strtoupper';

my @stdtests = stdtests();
#my @stdtests = (
#    {category=>'test', text=>'1+1', result=>2},
#);

for my $t (@stdtests) {
    my $tname = "category=$t->{category} $t->{text}";
    if ($t->{parse_error}) {
        $tname .= ", parse error: $t->{parse_error})";
        throws_ok { eval_in_php($phpcomp, $t->{text}) } $t->{parse_error}, $tname;
    } else {
        $tname .= ", php=q{".$phpcomp->{_phpcode_prefix}.$phpcomp->php($t->{text})."}";
        if ($t->{run_error}) {
            $tname .= ", run error: $t->{run_error})";
            throws_ok { eval_in_php($phpcomp, $t->{text}) } $t->{run_error}, $tname;
        } elsif ($t->{php_compiler_run_error}) {
            $tname .= ", run error: $t->{php_compiler_run_error})";
            throws_ok { eval_in_php($phpcomp, $t->{text}) } $t->{php_compiler_run_error}, $tname;
        } elsif ($t->{compiler_run_error}) {
            $tname .= ", run error: $t->{compiler_run_error})";
            throws_ok { eval_in_php($phpcomp, $t->{text}) } $t->{compiler_run_error}, $tname;
        } else {
            $tname .= ")";
            my $res = exists($t->{php_result}) ? $t->{php_result} : $t->{result};
            is_deeply( eval_in_php($phpcomp, $t->{text}), $res, $tname );
        }
    }
}

ok(1);
