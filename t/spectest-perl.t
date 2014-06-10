#!perl

use 5.010;
use strict;
use warnings;
use FindBin '$Bin';
use lib "$Bin";

require "testlib.pl";
use Test::More 0.98;
use Test::Exception;
use Language::Expr;
use POSIX;
use lib "./t";
require "stdtests.pl";

run_spectest('perl', {
});

DONE_TESTING:
done_testing;
