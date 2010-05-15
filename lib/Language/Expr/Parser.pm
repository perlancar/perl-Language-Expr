package Language::Expr::Parser;
# ABSTRACT: Parse Language::Expr expression

use feature 'state';
use strict;
use warnings;

=head1 FUNCTIONS

=head2 parse_expr($str, $obj)

Parse expression in $str. Will call various rule_*() methods in $obj.

=cut

sub parse_expr {
    my ($str, $obj_arg) = @_;

    use Regexp::Grammars;
    state $obj; # WARN: this is not thread-safe!?
    state $grammar = qr{
        ^<answer>$

        <rule: answer>
            <MATCH=or_xor>

# precedence level  2: left     =>
        <rule: pair>
            <key> =\> <value=answer>
            (?{ $MATCH = $obj->rule_pair(match=>%MATCH) })
            # yeah, weird. it should've been match=>\%MATCH

# precedence level  3: left     || // ^^
        <rule: or_xor>
            <[operand=and]> ** <[op=(\|\||//|\^\^)]>
            (?{ $MATCH = $obj->rule_or_xor(match=>%MATCH) })

# precedence level  4: left     &&
        <rule: and>
            <[operand=bit_or_xor]> ** <[op=(&&)]>
            (?{ $MATCH = $obj->rule_and(match=>%MATCH) })

# precedence level  5: left     | ^
        <rule: bit_or_xor>
            <[operand=bit_and]> ** <[op=(\||\^)]>
            (?{ $MATCH = $obj->rule_bit_or_xor(match=>%MATCH) })

# precedence level  6: left     &
        <rule: bit_and>
            <[operand=equal]> ** <[op=(&)]>
            (?{ $MATCH = $obj->rule_bit_and(match=>%MATCH) })

# precedence level  7: nonassoc == != <=> eq ne cmp
        <rule: equal>
            # \x3c = "<", \x3e = ">"
            <[operand=less_greater]> ** <[op=(==|!=|\x3c=\x3e|eq|ne|cmp)]>
            (?{ $MATCH = $obj->rule_equal(match=>%MATCH) })

# precedence level  8: nonassoc < > <= >= lt gt le ge
        <rule: less_greater>
            # \x3c = "<", \x3e = ">"
            <[operand=bit_shift]> ** <[op=(\x3c=?|\x3e=?|lt|gt|le|ge)]>
            (?{ $MATCH = $obj->rule_less_greater(match=>%MATCH) })

# precedence level  9: left     << >>
        <rule: bit_shift>
            # \x3c = "<", \x3e = ">"
            <[operand=add]> ** <[op=(\x3c\x3c|\x3e\x3e)]>
            (?{ $MATCH = $obj->rule_bit_shift(match=>%MATCH) })

# precedence level 10: left     + - .
        <rule: add>
            <[operand=mult]> ** <[op=(\+|-|\.)]>
            (?{ $MATCH = $obj->rule_add(match=>%MATCH) })

# precedence level 11: left     * / % x
        <rule: mult>
            <[operand=unary]> ** <[op=(\*|/|%|x)]>
            (?{ $MATCH = $obj->rule_mult(match=>%MATCH) })

# precedence level 12: right    ! ~ unary+ unary-
        <rule: unary>
            <[op=(!|~|\+|-)]>* <operand=power>
            (?{ $MATCH = $obj->rule_unary(match=>%MATCH) })

# precedence level 13: right    **
        <rule: power>
            <[operand=subscripting]> ** <op=(\*\*)>
            (?{ $MATCH = $obj->rule_power(match=>%MATCH) })

# precedence level 14: left    hash[s], array[i]
        <rule: subscripting>
            <[operand=term]> <[subscript]>*
            (?{ $MATCH = $obj->rule_subscripting(match=>%MATCH) })

        <rule: subscript>
              \[ <MATCH=term> \]

# precedence level 15: left     term (variable, str/num literals, func(), (paren))
        <rule: term>
              <MATCH=func>
            | <MATCH=var0>
            | <MATCH=str0>
            | <MATCH=undef>
            | <MATCH=num0>
            | <MATCH=bool0>
            | <MATCH=array>
            | <MATCH=hash>
            | \( <MATCH=answer> \)

        <rule: array>
            \[ \]
            (?{ $MATCH = $obj->rule_array(match=>{element=>[]}) })
          | \[ <[element=answer]> ** (,) \]
            (?{ $MATCH = $obj->rule_array(match=>%MATCH) })

        <rule: hash>
            \{ \}
            (?{ $MATCH = $obj->rule_hash(match=>{pair=>[]}) })
          | \{ <[pair]> ** (,) \}
            (?{ $MATCH = $obj->rule_hash(match=>%MATCH) })

        <rule: key>
            <MATCH=(\w+)>
          | <MATCH=answer>

        <token: undef>
            undef
            (?{ $MATCH = $obj->rule_undef() })

        <token: bool0>
            <bool=(true|false)>
            (?{ $MATCH = $obj->rule_bool(match=>%MATCH) })

        <token: num0>
            <num=( [+-]? \d++ (?: \. \d++ )?+ | inf | nan )>
            (?{ $MATCH = $obj->rule_num(match=>{num=>$MATCH{num}}) })

        <rule: str0>
            <MATCH=squotestr>
          | <MATCH=dquotestr>

        <token: squotestr>
            '<[part=(\\\\|\\'|\\|[^\\']+)]>*'
            (?{ $MATCH = $obj->rule_squotestr(match=>%MATCH) })

        <token: dquotestr>
            "<[part=([^"\044\\]|\$\.\.?|\$\w+|\$\{[^\}]+\}|\\\\|\\'|\\"|\\[tnrfbae\$]|\\[0-7]{1,3}|\\x[0-9A-Fa-f]{1,2}|\\x\{[0-9A-Fa-f]{1,4}\}|\\)]>*"
#                              ^ can't add + here, segfaults (perl/RG bug?)
            (?{ $MATCH = $obj->rule_dquotestr(match=>%MATCH) })

        <rule: var0>
            \$ <var=(\.\.?|\w+)>
            (?{ $MATCH = $obj->rule_var(match=>%MATCH) })
          | \$ \{ <var=([^\}]+)> \}
            (?{ $MATCH = $obj->rule_var(match=>%MATCH) })

        <rule: func>
            <func_name=([A-Za-z_]\w*)> \( \)
            (?{ $MATCH = $obj->rule_func(match=>{func_name=>$MATCH{func_name}, args=>[]}) })
          | <func_name=([A-Za-z_]\w*)> \( <[args=answer]> ** (,) \)
            (?{ $MATCH = $obj->rule_func(match=>%MATCH) })

    }xms;

    $obj = $obj_arg;
    $obj_arg->rule_preprocess(string_ref => \$str);
    die "Invalid syntax in expression `$str`" unless $str =~ $grammar;
    $obj_arg->rule_postprocess(result => $/{answer});
}

1;
