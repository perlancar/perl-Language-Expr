#!perl -T

use 5.010;
use strict;
use warnings;

our $JavaScript_pm_available;
our $JE_pm_available;
our $JS_bin;

BEGIN {
    {
        eval { require JavaScript };
        $JavaScript_pm_available++ unless $@;

        eval { require JE };
        $JE_pm_available++ unless $@;

        eval {
            require File::Which;
            my @paths = File::Which::which("js");
            ($ENV{PATH}) = $ENV{PATH} =~ /(.*)/;
            for (@paths) {
                #print "# DEBUG Testing $_ ...\n";
                ($_) = /(.*)/;
                my $output = qx($_ -e 'print(JSON.stringify([1+1]))') or die;
                #print "# DEBUG Output: $output\n";
                if ($output =~ /\A\[2\]$/m) {
                    $JS_bin = $_;
                    last;
                }
            }
        };
        #$@ and die $@;

        # JavaScript.pm && JE.pm are not good/compatible enough: no
        # JSON object, etc.
        #last if $JavaScript_pm_available || $JE_pm_available || $JS_bin;
        last if $JS_bin;

        require Test::More;
        Test::More::plan(skip_all => "no usable JavaScript engines available");
    }
}

use Test::More tests => 162;
use Test::Exception;
use Data::Walk;
use JSON;
use Language::Expr::Compiler::JS;
use String::ShellQuote;
use lib "./t";
require "testlib.pl";
require "stdtests.pl";

sub eval_in_js($$) {
    my ($js, $str) = @_;

    $str = $js->js($str);

    state $js_engine;

    if (!$js_engine) {
        if ($ENV{TEST_JS_ENGINE}) {
            $js_engine = $ENV{TEST_JS_ENGINE};
        } else {
            if ($JS_bin || $ENV{TEST_JS_ENGINE_BIN}) {
                $js_engine = "bin";
                $JS_bin = $ENV{TEST_JS_ENGINE_BIN} if $ENV{TEST_JS_ENGINE_BIN};
            } elsif ($JavaScript_pm_available) {
                $js_engine = "JavaScript.pm";
            } elsif ($JE_pm_available) {
                $js_engine = "JS.pm";
            }
        }
        print "# Choosing $js_engine".($js_engine eq 'bin' ? " ($JS_bin)": '')." as JavaScript engine\n";
    }

    if (0) {
    #if ($js_engine eq 'JavaScript.pm') {
    #    state $rt = JavaScript::Runtime->new;
    #    state $cx = $rt->create_context;
    #    return $cx->eval($str);
    #} elsif ($js_engine eq 'JE.pm') {
    #    state $je = JE->new;
    #    return $je->eval($str);
    } elsif ($js_engine eq 'bin') {
        my $cmd = qq($JS_bin -e ).shell_quote(qq($js->{_jscode_prefix}print(JSON.stringify($str)))).qq( 2>&1);
        #print "# DEBUG: $cmd\n";
        my $output;
        $output = qx($cmd);
        $output = "null" if $output =~ /\Aundefined\s*\z/s;
        $output =~ /syntax ?error/i and die "syntax error/invalid syntax: cmd=$cmd, output=$output";
        $output =~ /\b([A-Z]\w+Error)\b/i and die "javascript error: $1: cmd=$cmd, output=$output";
        $? and die "Can't execute $cmd successfully: $! ($?)";
        return convert_json_booleans(JSON->new->allow_nonref->decode($output));
    } else {
        die "BUG: Can't test yet with JS engine `$js_engine`!";
    }
}

my $js = new Language::Expr::Compiler::JS;
$js->{_jscode_prefix} = "let a=1; let b=2; let ary1=['one','two','three']; let hash1={one:1, two:2, three:3};";
$js->func_mapping->{floor} = 'Math.floor';
$js->func_mapping->{ceil}  = 'Math.ceil';
$js->func_mapping->{uc}  = '.toUpperCase';
$js->func_mapping->{length}  = ':length';

my @stdtests = stdtests();
#my @stdtests = (
#    {category=>'test', text=>'1+1', result=>2},
#);

for my $t (@stdtests) {
    my $tname = "category=$t->{category} $t->{text}";
    if ($t->{parse_error}) {
        $tname .= ", parse error: $t->{parse_error})";
        throws_ok { eval_in_js($js, $t->{text}) } $t->{parse_error}, $tname;
    } else {
        $tname .= ", js=q{".$js->{_jscode_prefix}.$js->js($t->{text})."}";
        if ($t->{run_error}) {
            $tname .= ", run error: $t->{run_error})";
            throws_ok { eval_in_js($js, $t->{text}) } $t->{run_error}, $tname;
        } elsif ($t->{js_compiler_run_error}) {
            $tname .= ", run error: $t->{js_compiler_run_error})";
            throws_ok { eval_in_js($js, $t->{text}) } $t->{js_compiler_run_error}, $tname;
        } elsif ($t->{compiler_run_error}) {
            $tname .= ", run error: $t->{compiler_run_error})";
            throws_ok { eval_in_js($js, $t->{text}) } $t->{compiler_run_error}, $tname;
        } else {
            $tname .= ")";
            my $res = exists($t->{js_result}) ? $t->{js_result} : $t->{result};
            is_deeply( eval_in_js($js, $t->{text}), $res, $tname );
        }
    }
}

ok(1);
