use 5.010;
use strict;
use warnings;
use experimental 'smartmatch';
use FindBin qw($Bin);

use File::chdir;
use File::Slurp::Tiny qw(read_file);
use File::Temp qw(tempfile);
use JSON;
use Language::Expr;
use List::Util qw(first);
#use SHARYANTO::String::Util qw(indent);
use Test::Exception;
use Test::More 0.98;
use Version::Util qw(version_eq);

my $json = JSON->new->allow_nonref;

my $le = Language::Expr->new;

# return true if all elements in $list1 are in $list2
sub all_match {
    my ($list1, $list2) = @_;

    for (@$list1) {
        return 0 unless $_ ~~ @$list2;
    }
    1;
}

# return true if any element in $list1 is in $list2
sub any_match {
    my ($list1, $list2) = @_;

    for (@$list1) {
        return 1 if $_ ~~ @$list2;
    }
    0;
}

# return true if none of the elements in $list1 is in $list2
sub none_match {
    my ($list1, $list2) = @_;

    for (@$list1) {
        return 0 if $_ ~~ @$list2;
    }
    1;
}

sub run_spectest {
    require Sah;

    my ($cname, $opts) = @_; # compiler/interpreter name
    $opts //= {};

    my $dir = "$Bin/../share/spectest";
    my @specfiles;
    {
        local $CWD = $dir;
        @specfiles = <*.json>;
    }

    # to test certain files only
    my @files;
    if ($ENV{TEST_LE_SPECTEST_FILES}) {
        @files = split /\s*,\s*|\s+/, $ENV{TEST_LE_SPECTEST_FILES};
    } else {
        @files = @specfiles;
    }

    # to test only tests that have all matching tags
    my @include_tags;
    if ($ENV{TEST_LE_SPECTEST_INCLUDE_TAGS}) {
        @include_tags = split /\s*,\s*|\s+/,
            $ENV{TEST_LE_SPECTEST_INCLUDE_TAGS};
    }

    # to skip tests that have all matching tags
    my @exclude_tags;
    if ($ENV{TEST_LE_SPECTEST_EXCLUDE_TAGS}) {
        @exclude_tags = split /\s*,\s*|\s+/,
            $ENV{TEST_LE_SPECTEST_EXCLUDE_TAGS};
    }

    my $should_skip_test = sub {
        my $t = shift;
        if ($opts->{skip_if}) {
            my $reason = $opts->{skip_if}->($t);
            return $reason if $reason;
        }
        if ($t->{tags} && @exclude_tags) {
            return "contains all excluded tags (".
                join(", ", @exclude_tags).")"
                    if all_match(\@exclude_tags, $t->{tags});
        }
        if (@include_tags) {
            return "does not contain all include tags (".
                join(", ", @include_tags).")"
                    if !all_match(\@include_tags, $t->{tags} // []);
        }
        "";
    };

    if ($cname eq 'perl') {
        for my $file (@files) {
            subtest $file => sub {
                my $spec = $json->decode(~~read_file("$dir/$file"));
              TEST:
                for my $test (@{ $spec->{tests} }) {
                    my $testname = $test->{name} //= $test->{input};
                    $testname .= " (".join(", ", @{ $test->{tags} }).")"
                        if @{ $test->{tags} };
                    subtest $testname => sub {
                        if (my $reason = $should_skip_test->($test)) {
                            plan skip_all => "Skipping test $test->{name}: $reason";
                            return;
                        }
                        eval {
                            my $codestr = $le->perl($test->{input});
                            diag "generated perl code: $codestr";
                            my $res = eval $codestr; die if $@;
                            if (exists $test->{result}) {
                                is_deeply($res, $test->{result}, "result");
                            }
                        };
                        my $eval_err = $@;
                        if ($test->{dies}) {
                            ok($eval_err, "dies");
                            return;
                        } else {
                            ok(!$eval_err, "doesn't die")
                                or diag $eval_err;
                        }
                    };
                }
                ok 1; # an extra dummy ok to pass even if all spectest is skipped
            };
        }

    } else {
        die "Unknown/unsupported compiler '$cname'";
    }
}

1;
