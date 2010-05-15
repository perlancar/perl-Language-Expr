#!perl -T

use strict;
use warnings;
use Test::More tests => 132;
use Test::Exception;
use Language::Expr;
use POSIX;

my @data = (
    # array
    {category=>'array', text=>'[]', result=>[]},
    {category=>'array', text=>'[1,2]', result=>[1, 2]},
    {category=>'array', text=>'[1, 2, 3+4]', result=>[1, 2, 7]},
    {category=>'array', error=>qr/invalid syntax/i, text=>'['},
    {category=>'array', error=>qr/invalid syntax/i, text=>']'},
    {category=>'array', error=>qr/invalid syntax/i, text=>'[,]'},
    {category=>'array', error=>qr/invalid syntax/i, text=>'[1,]'},
    {category=>'array', error=>qr/invalid syntax/i, text=>'[1 2]'},
    {category=>'array', error=>qr/invalid syntax/i, text=>'[a]'},

    # hash
    {category=>'hash', text=>'{}', result=>{}},
    {category=>'hash', text=>'{a=>1}', result=>{a=>1}},
    {category=>'hash', text=>q[{'a'=>1}], result=>{a=>1}},
    {category=>'hash', text=>'{a=>1, "b c"=>1+1}', result=>{a=>1, "b c"=>2}},
    {category=>'hash', error=>qr/invalid syntax/i, text=>'{'},
    {category=>'hash', error=>qr/invalid syntax/i, text=>'}'},
    {category=>'hash', error=>qr/invalid syntax/i, text=>'{=>}'},
    {category=>'hash', error=>qr/invalid syntax/i, text=>'{a=>}'},
    {category=>'hash', error=>qr/invalid syntax/i, text=>'{=>1}'},
    {category=>'hash', error=>qr/invalid syntax/i, text=>'{a, 1}'},
    {category=>'hash', error=>qr/invalid syntax/i, text=>'{a=>1, }'},
    {category=>'hash', error=>qr/invalid syntax/i, text=>'{1=>a}'},
    {category=>'hash', error=>qr/invalid syntax/i, text=>'a=>1'},

    # equal
    {category=>'equal num', text=>'1 == 2', result=>''},
    {category=>'equal num', text=>'1 == 1', result=>'1'},
    {category=>'equal num', text=>'1 != 2', result=>'1'},
    {category=>'equal num', text=>'1 != 1', result=>''},
    {category=>'equal num', text=>'0 == 0', result=>'1'},
    {category=>'equal num', text=>'3 <=> 4', result=>'-1'},
    {category=>'equal num', text=>'4 <=> 3', result=>'1'},
    {category=>'equal num', text=>'3 <=> 3', result=>'0'},
    {category=>'equal num', text=>'3 <=> 3', result=>'0'},

    {category=>'equal chained', text=>"0 == 1 == 0", result=>''},
    {category=>'equal chained', text=>"2 == 2 == 2", result=>'1'},
    {category=>'equal chained', text=>"0 eq 1 eq ''", result=>''},
    {category=>'equal chained', text=>"2 != 3 != 1", result=>'1'},

    {category=>'equal str', text=>'"" eq ""', result=>'1'},
    {category=>'equal str', text=>'"aa" cmp "ab"', result=>'-1'},

    # lessgreater
    {category=>'less_greater', text=>'1<2', result=>'1'},
    {category=>'less_greater', text=>'2<2', result=>''},
    {category=>'less_greater', text=>'3<2', result=>''},
    {category=>'less_greater', text=>'1 <= 2', result=>'1'},
    {category=>'less_greater', text=>'1 <= 1', result=>'1'},
    {category=>'less_greater', text=>'3 <= 2', result=>''},
    {category=>'less_greater', text=>'1>2', result=>''},
    {category=>'less_greater', text=>'2>2', result=>''},
    {category=>'less_greater', text=>'3>2', result=>'1'},
    {category=>'less_greater', text=>'1 >= 2', result=>''},
    {category=>'less_greater', text=>'1 >= 1', result=>'1'},
    {category=>'less_greater', text=>'3 >= 2', result=>'1'},

    {category=>'less_greater chained', text=>'3 > 2 > 1', result=>'1'},
    {category=>'less_greater chained', text=>'2 > 3 > 1', result=>''},
    {category=>'less_greater chained', text=>'2 > 3 < 1', result=>''},

    {category=>'less_greater', error=>qr/invalid syntax/i, text=>'>'},
    {category=>'less_greater', error=>qr/invalid syntax/i, text=>'1 >'},
    {category=>'less_greater', error=>qr/invalid syntax/i, text=>'> 1'},
    #{category=>'less_greater', error=>qr/invalid syntax/i, text=>'1 > 0 >'}, # RG bug? causes subsequent parsing to fail
    {category=>'less_greater', error=>qr/invalid syntax/i, text=>'< 1 < 2'},

    # and
    {category=>'and', text=>'1 && 2', result=>'2'},
    {category=>'and', text=>'1 && 0', result=>'0'},
    {category=>'and', text=>'1 > 1 && 1 >= 1', result=>''},
    {category=>'and chained', text=>'1 > 1 && 1 > 1', result=>''},

    # or & xor
    {category=>'or_xor', text=>'1 || 2', result=>'1'},
    {category=>'or_xor', text=>'1 || 0', result=>'1'},
    {category=>'or_xor', text=>'1 > 1 || 1 >= 1', result=>'1'},
    {category=>'or_xor', text=>'1 > 1 || 1 > 1', result=>''},
    {category=>'or_xor', text=>'1 // 2', result=>'1'},
    {category=>'or_xor', text=>'1 // undef', result=>'1'},
    {category=>'or_xor', text=>'undef // 2', result=>'2'},
    {category=>'or_xor', text=>'undef // undef', result=>undef},

    # add
    {category=>'add', text=>'1+1', result=>'2'},
    {category=>'add', text=>'1+1+4+7', result=>'13'},
    {category=>'add', text=>'1-1', result=>'0'},
    {category=>'add', text=>'10-2-5-1', result=>'2'},
    {category=>'add', text=>'10+2-5+1', result=>'8'},
    {category=>'add', text=>'1 . 1', result=>'11'},
    {category=>'add', text=>'"satu "."dua"', result=>'satu dua'},

    # mult
    {category=>'mult', text=>'2*4', result=>'8'},
    {category=>'mult', text=>'2*-1*-4*7', result=>'56'},
    {category=>'mult', text=>'6/2', result=>'3'},
    {category=>'mult', text=>'80/2/5/4', result=>'2'},
    {category=>'mult', text=>'80/2/5*4', result=>'32'},
    {category=>'mult', text=>'80 % 3', result=>'2'},
    {category=>'mult', text=>'800 % 30 % 3', result=>'2'},
    {category=>'mult', text=>'"ab" x 2', result=>'abab'},
    {category=>'mult', text=>'"ab" x 2 x 2', result=>'abababab'},

    # pow
    {category=>'power', text=>'2**4', result=>'16'},
    {category=>'power', text=>'2**4**2', result=>'65536'},

    # unary
    {category=>'unary', text=>'!2', result=>''},
    {category=>'unary', text=>'!!2', result=>'1'},
    {category=>'unary', text=>'!!2', result=>'1'},
    {category=>'unary', text=>'--2', result=>2},
    {category=>'unary', text=>'---2', result=>-2},
    {category=>'unary', text=>'~2', result=>~2},

    # bitwise
    {category=>'bit', text=>'3|5', result=>'7'},
    {category=>'bit', text=>'3 & 5', result=>'1'},
    {category=>'bit', text=>'3 ^ 5', result=>'6'},
    # ~, see unary
    {category=>'bit', text=>'3 << 2', result=>'12'},
    {category=>'bit', text=>'3 << 2+1', result=>'24'},
    {category=>'bit', text=>'12 >> 2', result=>'3'},
    {category=>'bit', text=>'24 >> 2+1', result=>'3'},

    # term:literal
    {category=>'undef', text=>'undef', result=>undef},
    {category=>'true', text=>'true', result=>1},
    {category=>'false', text=>'false', result=>''},
    {category=>'num', text=>'1', result=>'1'},
    {category=>'num', text=>'1.1', result=>'1.1'},
    {category=>'dquotestr', text=>q("satu ' dua"), result=>"satu ' dua"},
    {category=>'squotestr', text=>q('satu " dua'), result=>'satu " dua'},
    {category=>'squotestr escape sequence', text=>q('\\''), result=>'\''},
    {category=>'squotestr escape sequence', text=>q('\\\\'), result=>'\\'},
    {category=>'squotestr escape sequence', text=>q('\\n'), result=>'\n'},
    {category=>'dquotestr escape sequence', text=>q("\\n"), result=>"\n"},
    # XXX more escape sequences
    {category=>'squotestr interpolate var', text=>q('$a'), result=>'$a'},
    #{category=>'dquotestr interpolate var', text=>q("$a"), result=>1},   # currently causes segfault, RG bug?
    #{category=>'dquotestr interpolate var', text=>q("$(a)"), result=>1}, # currently causes segfault, RG bug?

    # term:paren
    {category=>'paren', text=>'4*(2 + 3)', result=>'20'},
    {category=>'paren', text=>'-(1+1)', result=>'-2'},
    {category=>'paren', text=>'(((2)))', result=>'2'},
    {category=>'paren', text=>'2**(1+1+1+1+1 + 1+1+1+1+1)', result=>'1024'},
    {category=>'paren', text=>'(2)+((3))+(((4)))+((((5))))+(((((6)))))', result=>'20'},

    # term:var
    {category=>'var', text=>'$b', result=>'2'},
    {category=>'var', text=>q[${a b}], result=>'3'},
    {category=>'var', text=>'$a+2*$b', result=>'5'},

    # term:subscript
    {category=>'subscripting', text => '([10, 20, 30])[0]', result=>'10'},
    {category=>'subscripting', text => '([10, 20, 30])[2]', result=>'30'},
    {category=>'subscripting', text => '([1, 2, 3])[3]', result=>undef},
    {category=>'subscripting', text => '({a=>10, b=>20, "c 2" => 30})["b"]', result=>'20'},
    {category=>'subscripting', text => '({a=>10, b=>20, "c 2" => 30})["c 2"]', result=>'30'},
    {category=>'subscripting', text => '({a=>10, b=>20, "c 2" => 30})["x"]', result=>undef},
    {category=>'subscripting', text => '{a=>[10, 20]}["a"][1]', result=>20},
    #{category=>'subscripting', error=>qr/subscript/i, text => '1[1]'}, # currently doesn't work, RG bug?

    {category=>'func', text=>'length("str")', result=>'3'},
    {category=>'func', error=>qr/invalid syntax/i, text => 'length'},
    {category=>'func', error=>qr/invalid syntax/i, text => 'length "str"'},
    {category=>'func', text=>'length("s" . "tr")', result=>'3'},
    {category=>'func', text=>'ceil(rand())+floor(rand()*rand())', result=>'1'},
    {category=>'func', error => qr/unknown func/i, text=>'foo(1)', result=>'1'},

);

my $le = new Language::Expr;
$le->var(a => 1, b => 2, 'a b' => 3);
$le->func(
    'length' => sub { length(shift) },
    'rand'   => sub { rand() },
    'floor'  => sub { POSIX::floor(shift) },
    'ceil'   => sub { POSIX::ceil(shift) },
);

for (@data) {
    if ($_->{error}) {
        throws_ok { $le->eval($_->{text}) } $_->{error},
            "$_->{category} ($_->{text}) (error: $_->{error})";
    } else {
        is_deeply( $le->eval($_->{text}), $_->{result},
                   "$_->{category} ($_->{text})" );
    }
}

