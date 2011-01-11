package Language::Expr::Parser;
# ABSTRACT: Parse Language::Expr expression

use 5.010;
# now can't compile with this on?
#use strict;
#use warnings;

require Exporter;
our @ISA = qw(Exporter);
our @EXPORT_OK = qw(parse_expr);

my $MAX_LEVELS = 3;

=head1 METHODS

=head2 parse_expr($str, $obj)

Parse expression in $str. Will call various rule_*() methods in $obj.

=cut

sub parse_expr {
    my ($str, $obj_arg, $level) = @_;

    $level //= 0;
    die "Recursion level ($level) too deep (max $MAX_LEVELS)" if $level >= $MAX_LEVELS;

    use Regexp::Grammars;

    # WARN: this is not thread-safe!?
    state $obj;
    local $subexpr_stack = [];

    # create not just 1 but 0..$MAX_LEVELS-1 of grammar objects, each
    # for each recursion level (e.g. for map/grep/usort), fearing that
    # the grammar is not reentrant. but currently no luck yet, still
    # results in segfault/bus error.

    state $grammars = [ map { qr{
        ^\s*<answer>\s*$

        <rule: answer>
            <MATCH=or_xor>

# precedence level  2: left     =>
        <rule: pair>
            <key=(\w+)> =\> <value=answer>
            (?{ $MATCH = $obj->rule_pair_simple(match=>\%MATCH) })
          | <key=squotestr> =\> <value=answer>
            (?{ $MATCH = $obj->rule_pair_string(match=>\%MATCH) })
          | <key=dquotestr> =\> <value=answer>
            (?{ $MATCH = $obj->rule_pair_string(match=>\%MATCH) })

# precedence level  3: left     || // ^^
        <rule: or_xor>
            <[operand=and]> ** <[op=(\|\||//|\^\^)]>
            (?{ $MATCH = $obj->rule_or_xor(match=>\%MATCH) })

# precedence level  4: left     &&
        <rule: and>
            <[operand=bit_or_xor]> ** <[op=(&&)]>
            (?{ $MATCH = $obj->rule_and(match=>\%MATCH) })

# precedence level  5: left     | ^
        <rule: bit_or_xor>
            <[operand=bit_and]> ** <[op=(\||\^)]>
            (?{ $MATCH = $obj->rule_bit_or_xor(match=>\%MATCH) })

# precedence level  6: left     &
        <rule: bit_and>
            <[operand=comparison3]> ** <[op=(&)]>
            (?{ $MATCH = $obj->rule_bit_and(match=>\%MATCH) })

            # NOTE: \x3c = "<", \x3e = ">"

# precedence level  7: nonassoc (currently the grammar says assoc) <=> cmp
        <rule: comparison3>
            <[operand=comparison]> ** <[op=(\x3c=\x3e|cmp)]>
            (?{ $MATCH = $obj->rule_comparison3(match=>\%MATCH) })

# precedence level  8: left == != eq ne < > <= >= ge gt le lt
        <rule: comparison>
            <[operand=bit_shift]> ** <[op=(==|!=|eq|ne|\x3c=?|\x3e=?|lt|gt|le|ge)]>
            (?{ $MATCH = $obj->rule_comparison(match=>\%MATCH) })

# precedence level  9: left     << >>
        <rule: bit_shift>
            <[operand=add]> ** <[op=(\x3c\x3c|\x3e\x3e)]>
            (?{ $MATCH = $obj->rule_bit_shift(match=>\%MATCH) })

# precedence level 10: left     + - .
        <rule: add>
            <[operand=mult]> ** <[op=(\+|-|\.)]>
            (?{ $MATCH = $obj->rule_add(match=>\%MATCH) })

# precedence level 11: left     * / % x
        <rule: mult>
            <[operand=unary]> ** <[op=(\*|/|%|x)]>
            (?{ $MATCH = $obj->rule_mult(match=>\%MATCH) })

# precedence level 12: right    ! ~ unary+ unary-
        <rule: unary>
            <[op=(!|~|\+|-)]>* <operand=power>
            (?{ $MATCH = $obj->rule_unary(match=>\%MATCH) })

# precedence level 13: right    **
        <rule: power>
            <[operand=subscripting]> ** <[op=(\*\*)]>
            (?{ $MATCH = $obj->rule_power(match=>\%MATCH) })

# precedence level 14: left    hash[s], array[i]
        <rule: subscripting>
            <operand=var0> <[subscript]>*
            (?{ $MATCH = $obj->rule_subscripting_var(match=>\%MATCH) })
          | <operand=term> <[subscript]>*
            (?{ $MATCH = $obj->rule_subscripting_expr(match=>\%MATCH) })

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
          | \( <answer> \)
            (?{ $MATCH = $obj->rule_parenthesis(match=>\%MATCH) // $MATCH{answer} })

        <rule: array>
            \[ \]
            (?{ $MATCH = $obj->rule_array(match=>{element=>[]}) })
          | \[ <[element=answer]> ** (,) \]
            (?{ $MATCH = $obj->rule_array(match=>\%MATCH) })

        <rule: hash>
            \{ \}
            (?{ $MATCH = $obj->rule_hash(match=>{pair=>[]}) })
          | \{ <[pair]> ** (,) \}
            (?{ $MATCH = $obj->rule_hash(match=>\%MATCH) })

        <token: undef>
            undef
            (?{ $MATCH = $obj->rule_undef() })

        <token: bool0>
            <bool=(true|false)>
            (?{ $MATCH = $obj->rule_bool(match=>\%MATCH) })

        <token: num0>
            <sign0a=([+-]?)> 0x <num0a=([0-9A-Fa-f]++)>
            (?{ $MATCH = $obj->rule_num(match=>{num=>
                ($MATCH{sign0a} eq '-' ? -1:1) * hex($MATCH{num0a})}) })
          | <sign0b=([+-]?)> 0o <num0b=([0-7]++)>
            (?{ $MATCH = $obj->rule_num(match=>{num=>
                ($MATCH{sign0b} eq '-' ? -1:1) * oct($MATCH{num0b})}) })
          | <sign0c=([+-]?)> 0b <num0c=([0-1]++)>
            (?{ $MATCH = $obj->rule_num(match=>{num=>
                ($MATCH{sign0c} eq '-' ? -1:1) * oct("0b".$MATCH{num0c})}) })
          | <num0c=( [+-]?\d++(?:\.\d++)?+ | inf | nan)>
            (?{ $MATCH = $obj->rule_num(match=>{num=>$MATCH{num0c}}) })

        <rule: str0>
            <MATCH=squotestr>
          | <MATCH=dquotestr>

        <token: squotestr>
            '<[part=(\\\\|\\'|\\|[^\\']+)]>*'
            (?{ $MATCH = $obj->rule_squotestr(match=>\%MATCH) })

        <token: dquotestr>
            "<[part=([^"\044\\]+|\$\.\.?|\$\w+|\$\{[^\}]+\}|\\\\|\\'|\\"|\\[tnrfbae\$]|\\[0-7]{1,3}|\\x[0-9A-Fa-f]{1,2}|\\x\{[0-9A-Fa-f]{1,4}\}|\\)]>*"
            (?{ $MATCH = $obj->rule_dquotestr(match=>\%MATCH) })

        <rule: var0>
            \$ <var=(\w+(?:::\w+)*)>
            (?{ $MATCH = $obj->rule_var(match=>\%MATCH) })
          | \$ \{ <var=([^\}]+)> \}
            (?{ $MATCH = $obj->rule_var(match=>\%MATCH) })

        <rule: func>
            <func_name=([A-Za-z_]\w*)> \( \)
            (?{ $MATCH = $obj->rule_func(match=>{func_name=>$MATCH{func_name}, args=>[]}) })
          | <func_name=(map|grep|usort)> \( \{ <expr=answer> \} (?{ push @$subexpr_stack, $CONTEXT }), <input_array=answer> \)
            (?{ my $meth = "rule_func_$MATCH{func_name}";
                $MATCH = $obj->$meth(match=>{expr=>pop(@$subexpr_stack), array=>$MATCH{input_array}}) })
          | <func_name=([A-Za-z_]\w*)> \( <[args=answer]> ** (,) \)
            (?{ $MATCH = $obj->rule_func(match=>\%MATCH) })

    }xms } 0..($MAX_LEVELS-1)];

    $obj = $obj_arg;
    $obj_arg->expr_preprocess(string_ref => \$str, level => $level);
    #print "DEBUG: Parsing expression `$str` with grammars->[$level] ...\n";
    die "Invalid syntax in expression `$str`" unless $str =~ $grammars->[$level];
    $obj_arg->expr_postprocess(result => $/{answer});
}

1;
