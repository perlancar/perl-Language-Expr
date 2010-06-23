use boolean;

sub stdtests {
    return (
    # array
    {category=>'array', text=>'[]', result=>[]},
    {category=>'array', text=>'[1,2]', result=>[1, 2]},
    {category=>'array', text=>'[1, 2, 3+4]', result=>[1, 2, 7]},
    {category=>'array', parse_error=>qr/invalid syntax/i, text=>'['},
    {category=>'array', parse_error=>qr/invalid syntax/i, text=>']'},
    {category=>'array', parse_error=>qr/invalid syntax/i, text=>'[,]'},
    {category=>'array', parse_error=>qr/invalid syntax/i, text=>'[1,]'},
    {category=>'array', parse_error=>qr/invalid syntax/i, text=>'[1 2]'},
    {category=>'array', parse_error=>qr/invalid syntax/i, text=>'[a]'},

    # hash
    {category=>'hash', text=>'{}', result=>{}},
    {category=>'hash', text=>'{a=>1}', result=>{a=>1}},
    {category=>'hash', text=>q[{'a'=>1}], result=>{a=>1}},
    {category=>'hash', text=>'{a=>1, "b c"=>1+1}', result=>{a=>1, "b c"=>2}},
    {category=>'hash', parse_error=>qr/invalid syntax/i, text=>'{'},
    {category=>'hash', parse_error=>qr/invalid syntax/i, text=>'}'},
    {category=>'hash', parse_error=>qr/invalid syntax/i, text=>'{=>}'},
    {category=>'hash', parse_error=>qr/invalid syntax/i, text=>'{a=>}'},
    {category=>'hash', parse_error=>qr/invalid syntax/i, text=>'{=>1}'},
    {category=>'hash', parse_error=>qr/invalid syntax/i, text=>'{a, 1}'},
    {category=>'hash', parse_error=>qr/invalid syntax/i, text=>'{a=>1, }'},
    {category=>'hash', parse_error=>qr/invalid syntax/i, text=>'{1=>a}'},
    {category=>'hash', parse_error=>qr/invalid syntax/i, text=>'a=>1'},

    # comparison equal
    {category=>'comparison equal num', text=>'1 == 2', result=>false},
    {category=>'comparison equal num', text=>'1 == 1', result=>true},
    {category=>'comparison equal num', text=>'1 != 2', result=>true},
    {category=>'comparison equal num', text=>'1 != 1', result=>false},
    {category=>'comparison equal num', text=>'0 == 0', result=>true},
    {category=>'comparison equal num', text=>'3 <=> 4', result=>-1},
    {category=>'comparison equal num', text=>'4 <=> 3', result=>1},
    {category=>'comparison equal num', text=>'3 <=> 3', result=>0},
    {category=>'comparison equal num', text=>'3 <=> 3', result=>0},

    {category=>'comparison equal chained', text=>"0 == 1 == 0", result=>false},
    {category=>'comparison equal chained', text=>"2 == 2 == 2", result=>true},
    {category=>'comparison equal chained', text=>"0 eq 1 eq ''", result=>false},
    {category=>'comparison equal chained', text=>"2 != 3 != 1", result=>true},

    {category=>'comparison equal str', text=>'"" eq ""', result=>true},
    {category=>'comparison equal str', text=>'"aa" cmp "ab"', result=>-1},

    # comparison less_greater
    {category=>'comparison less_greater', text=>'1<2', result=>true},
    {category=>'comparison less_greater', text=>'2<2', result=>false},
    {category=>'comparison less_greater', text=>'3<2', result=>false},
    {category=>'comparison less_greater', text=>'1 <= 2', result=>true},
    {category=>'comparison less_greater', text=>'1 <= 1', result=>true},
    {category=>'comparison less_greater', text=>'3 <= 2', result=>false},
    {category=>'comparison less_greater', text=>'1>2', result=>false},
    {category=>'comparison less_greater', text=>'2>2', result=>false},
    {category=>'comparison less_greater', text=>'3>2', result=>true},
    {category=>'comparison less_greater', text=>'1 >= 2', result=>false},
    {category=>'comparison less_greater', text=>'1 >= 1', result=>true},
    {category=>'comparison less_greater', text=>'3 >= 2', result=>true},

    {category=>'comparison less_greater chained', text=>'3 > 2 > 1', result=>true},
    {category=>'comparison less_greater chained', text=>'2 > 3 > 1', result=>false},
    {category=>'comparison less_greater chained', text=>'2 > 3 < 1', result=>false},

    {category=>'comparison less_greater', parse_error=>qr/invalid syntax/i, text=>'>'},
    {category=>'comparison less_greater', parse_error=>qr/invalid syntax/i, text=>'1 >'},
    {category=>'comparison less_greater', parse_error=>qr/invalid syntax/i, text=>'> 1'},
    #{category=>'comparison less_greater', parse_error=>qr/invalid syntax/i, text=>'1 > 0 >'}, # RG bug? causes subsequent parsing to fail
    {category=>'comparison less_greater', parse_error=>qr/invalid syntax/i, text=>'< 1 < 2'},

    # and
    {category=>'and', text=>'1 && 2', result=>'2'},
    {category=>'and', text=>'1 && 0', result=>'0'},
    {category=>'and', text=>'1 > 1 && 1 >= 1', result=>false},
    {category=>'and chained', text=>'1 > 1 && 1 > 1', result=>false},

    # or & xor
    {category=>'or_xor', text=>'1 || 2', result=>true},
    {category=>'or_xor', text=>'1 || 0', result=>true},
    {category=>'or_xor', text=>'1 > 1 || 1 >= 1', result=>true},
    {category=>'or_xor', text=>'1 > 1 || 1 > 1', result=>false},
    {category=>'or_xor', text=>'1 // 2', result=>1},
    {category=>'or_xor', text=>'1 // undef', result=>1},
    {category=>'or_xor', text=>'undef // 2', result=>2},
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
    {category=>'unary', text=>'!2', result=>false},
    {category=>'unary', text=>'!!2', result=>true},
    {category=>'unary', text=>'!!2', result=>true},
    {category=>'unary', text=>'--2', result=>2},
    {category=>'unary', text=>'---2', result=>-2},
    {category=>'unary', text=>'~2', result=>~2, js_result=>(-(2)-1)}, # js's bitwise NOT is a bit peculiar

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
    {category=>'true', text=>'true', result=>true},
    {category=>'false', text=>'false', result=>false},
    {category=>'num', text=>'1', result=>'1'},
    {category=>'num', text=>'1.1', result=>'1.1'},
    {category=>'dquotestr', text=>q("satu ' dua"), result=>"satu ' dua"},
    {category=>'squotestr', text=>q('satu " dua'), result=>'satu " dua'},
    {category=>'squotestr escape sequence', text=>q('\\''), result=>'\''},
    {category=>'squotestr escape sequence', text=>q('\\\\'), result=>'\\'},
    {category=>'squotestr escape sequence', text=>q('\\n'), result=>'\n'},
    {category=>'dquotestr escape sequence', text=>q("\\n"), result=>"\n"},
    {category=>'squotestr', text=>q('@b'), result=>'@b'}, # to test compiler properly escaping "@foo"
    {category=>'dquotestr', text=>'"@b"', result=>'@b'}, # to test compiler properly escaping "@foo"
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
    {category=>'var', text=>q[${a b}], result=>'3', compiler_run_error=>qr/syntax error/i},
    {category=>'var', text=>'$a+2*$b', result=>'5'},

    # term:subscript
    {category=>'subscripting', text => '([10, 20, 30])[0]', result=>'10'},
    {category=>'subscripting', text => '([10, 20, 30])[2]', result=>'30'},
    {category=>'subscripting', text => '([1, 2, 3])[3]', result=>undef},
    {category=>'subscripting', text => '({a=>10, b=>20, "c 2" => 30})["b"]', result=>'20'},
    {category=>'subscripting', text => '({a=>10, b=>20, "c 2" => 30})["c 2"]', result=>'30'},
    {category=>'subscripting', text => '({a=>10, b=>20, "c 2" => 30})["x"]', result=>undef},
    {category=>'subscripting', text => '{a=>[10, 20]}["a"][1]', result=>20},
    #{category=>'subscripting', parse_error=>qr/subscript/i, text => '1[1]'}, # currently doesn't work, RG bug?

    {category=>'func', text=>'floor(3.6)', result=>'3'},
    {category=>'func', parse_error=>qr/invalid syntax/i, text => 'floor'},
    {category=>'func', parse_error=>qr/invalid syntax/i, text => 'floor 3.6'},
    {category=>'func', text=>'floor(3.6 + 0.4)', result=>4},
    {category=>'func', text=>'ceil(0.7)+floor(0.3+0.6)', result=>1},
    #{category=>'func', run_error => qr/unknown func|undefined sub/i, text=>'foo(1)', result=>'1'}, # BUG in RG? causes error: Can't use an undefined value as an ARRAY reference at (re_eval 252) line 2.

    # map=7
    {category=>'map', has_subexpr=>1, text=>'map {}, []', parse_error=>qr/invalid syntax/i}, # lack parenthesis
    {category=>'map', has_subexpr=>1, text=>'map({1<}, [])', parse_error=>qr/invalid syntax/i}, # invalid subexpression

    {category=>'map', has_subexpr=>1, text=>'map()', compiler_run_error=>qr/not enough arg/i, js_compiler_run_error=>qr/javascript error/i},
    #{category=>'map', has_subexpr=>1, text=>'map({}, [])'}, # empty subexpression. won't be parsed as map(), but ok.
    #{category=>'map', has_subexpr=>1, text=>'map(1, [])'}, # not subexpression. won't be parsed as map(), but ok. but in perl result will be 1.

    {category=>'map', has_subexpr=>1, text=>'map({$_*2}, {})', compiler_run_error=>qr/syntax error|unmatched right/i, js_compiler_run_error=>qr/javascript error/i}, # although doesn't make sense, parses
    {category=>'map', has_subexpr=>1, text=>'map({$_*2}, [])', result=>[]},
    {category=>'map', has_subexpr=>1, text=>'map({$_*2}, [1,2,3])', result=>[2, 4, 6]},
    {category=>'map', has_subexpr=>1, text=>'map({ map({$_+1}, [$_])[0] }, [1,2,3])', result=>[2, 3, 4]}, # nested map

    # grep=7
    {category=>'grep', has_subexpr=>1, text=>'grep {}, []', parse_error=>qr/invalid syntax/i}, # lack parenthesis
    {category=>'grep', has_subexpr=>1, text=>'grep({1<}, [])', parse_error=>qr/invalid syntax/i}, # invalid subexpression

    {category=>'grep', has_subexpr=>1,  text=>'grep()', compiler_run_error=>qr/not enough arg/i, js_compiler_run_error=>qr/javascript error/i}, # lack arguments. won't be parsed as grep(), but ok
    #{category=>'grep', has_subexpr=>1, text=>'grep({}, [])'}, # empty subexpression. won't be parsed as grep(), but ok
    #{category=>'grep', has_subexpr=>1, text=>'grep(1, [])'}, # not subexpression. won't be parsed as grep(), but ok

    {category=>'grep', has_subexpr=>1, text=>'grep({$_>1}, {})', compiler_run_error=>qr/syntax error|unmatched right/i, js_compiler_run_error=>qr/javascript error/i}, # although doesn't make sense, parses
    {category=>'grep', has_subexpr=>1, text=>'grep({$_>1}, [])', result=>[]},
    {category=>'grep', has_subexpr=>1, text=>'grep({$_>1}, [1,2,3])', result=>[2, 3]},
    {category=>'grep', has_subexpr=>1, text=>'grep({ grep({$_ > 1}, [$_])[0] }, [1,2,3])', result=>[2, 3]}, # nested grep

    # usort=7
    {category=>'usort', has_subexpr=>1, text=>'usort {}, []', parse_error=>qr/invalid syntax/i}, # lack parenthesis
    {category=>'usort', has_subexpr=>1, text=>'usort({1<}, [])', parse_error=>qr/invalid syntax/i}, # invalid subexpression

    {category=>'usort', has_subexpr=>1, text=>'usort()', compiler_run_error=>qr/undefined sub.+usort/i, js_compiler_run_error=>qr/javascript error/i}, # lack arguments. won't be parsed as usort(), but ok
    #{category=>'usort', has_subexpr=>1, text=>'usort({}, [])'}, # empty subexpression. won't be parsed as usort(), but ok
    #{category=>'usort', has_subexpr=>1, text=>'usort(1, [])'}, # not subexpression. won't be parsed as usort(), but ok

    {category=>'usort', has_subexpr=>1, text=>'usort({uc($a) cmp uc($b)}, {})', compiler_run_error=>qr/syntax error|unmatched right/i, js_compiler_run_error=>qr/javascript error/i}, # although doesn't make sense, parses
    {category=>'usort', has_subexpr=>1, text=>'usort({uc($a) cmp uc($b)}, [])', result=>[]},
    {category=>'usort', has_subexpr=>1, text=>'usort({uc($a) cmp uc($b)}, ["B", "a", "C"])', result=>["a", "B", "C"]},
    {category=>'usort', has_subexpr=>1, text=>'usort({ usort({$b <=> $a}, [$a])[0] <=> usort({$b<=>$a}, [$b])[0] }, [3, 2, 1])', result=>[1, 2, 3]}, # nested usort
    );
}

1;
