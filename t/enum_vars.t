#!perl -T

use strict;
use warnings;
use Test::More tests => 8;
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

    #{category=>'dquotestr', text=>q("$a"), result=>['a']},   # currently causes segfault, RG bug?
    #{category=>'dquotestr', text=>q("$(a)"), result=>['a']},   # currently causes segfault, RG bug?

    {category=>'func', text=>'length($a)', result=>['a']},

    {category=>'subscript', text=>'$.[$b]+([1, 2, $..])[$a]', result=>['.', 'b', '..', 'a']},

);

my $le = new Language::Expr;

for (@data) {
    is_deeply($le->enum_vars($_->{text}), $_->{result}, "$_->{category} ($_->{text})");
}

