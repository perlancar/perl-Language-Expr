#!perl

use strict;
use warnings;
use Test::More;
use Test::Exception;
use Language::Expr;
use POSIX;

my @data = (
    {category=>'none', text=>'[]', result=>[]},
    {category=>'none', text=>'1+2+3', result=>[]},

    {category=>'basic', text=>'$b', result=>['b']},
    {category=>'basic', text=>q[${a b}], result=>['a b']},
    {category=>'basic', text=>'$a+2*$b', result=>['a', 'b']},

    {category=>'repeat', text=>'$b+$b*$b', result=>['b']},

    {category=>'quotestr', text=>q("${a b} $c" . '$d'), result=>['a b', 'c']},

    {category=>'func', text=>'length($a)', result=>['a']},

    {category=>'subscript', text=>'$a::b[$b]+([1, 2, $b::c])[$a]', result=>['a::b', 'b', 'b::c', 'a']},

);

my $le = new Language::Expr;

for (@data) {
    is_deeply($le->enum_vars($_->{text}), $_->{result}, "$_->{category} ($_->{text})");
}

DONE_TESTING:
done_testing;
