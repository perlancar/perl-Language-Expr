package Language::Expr::Parser;

use 5.010;
use strict;
use warnings;

require Exporter;
our @ISA = qw(Exporter);
our @EXPORT_OK = qw(parse_expr);

# VERSION
# DATE

my $bnf = <<'_'
:start             ::= answer

answer             ::= or_dor_xor

# precedence level : left     =>
pair               ::= word ('=>') value         action=>rule_pair_simple
                     | squotestr ('=>') value    action=>rule_pair_string
                     | dquotestr ('=>') value    action=>rule_pair_string
word               ~   [\w]+
value              ::= answer

# precedence level : left     || // ^^
or_dor_xor         ::= or                        action=>::first
                     | dor                       action=>::first
                     | xor                       action=>::first
or                 ::= ternary+                  separator=>op_or action=>rule_or
op_or              ~   '||'
dor                ::= ternary+                  separator=>op_dor action=>rule_dor
op_dor             ~   '//'
xor                ::= ternary+                  separator=>op_xor action=>rule_xor
op_xor             ~   '^^'

# precedence level : right    ?:
ternary            ::= and                       action=>::first
                     | and ('?') and (':') and   action=>rule_ternary
# precedence level : left     &&
and                ::= bit_or_xor+               separator=op_and action=rule_and
op_and             ~   '&&'

# precedence level : left     | ^
bit_or_xor         ::= bit_or                    action=>::first
                     | bit_xor                   action=>::first
bit_or             ::= bit_and+                  separator=op_bit_or action=rule_bit_or
op_bit_or          ~   '|'
bit_xor            ::= bit_and+                  separator=op_bit_xor action=rule_bit_xor
op_bit_xor         ~   '^'

# precedence level : left     &
bit_and            ::= comparison3+              separator=op_bit_and action=rule_bit_and
op_bit_and         ~   '&'

            # NOTE: \x3c = "<", \x3e = ">"

# precedence level: nonassoc (currently the grammar says assoc) <=> cmp
        <rule: comparison3>
            <[operand=comparison]> ** <[op=(\x3c=\x3e|cmp)]>
            (?{
                if ($MATCH{op} && @{ $MATCH{op} }) {
                    $MATCH = $obj->rule_comparison3(match=>\%MATCH);
                } else {
                    $MATCH = $MATCH{operand}[0];
                }
            })

# precedence level: left == != eq ne < > <= >= ge gt le lt
        <rule: comparison>
            <[operand=bit_shift]> ** <[op=(==|!=|eq|ne|\x3c=?|\x3e=?|lt|gt|le|ge)]>
            (?{
                if ($MATCH{op} && @{ $MATCH{op} }) {
                    $MATCH = $obj->rule_comparison(match=>\%MATCH);
                } else {
                    $MATCH = $MATCH{operand}[0];
                }
            })

# precedence level: left     << >>
        <rule: bit_shift>
            <[operand=add]> ** <[op=(\x3c\x3c|\x3e\x3e)]>
            (?{
                if ($MATCH{op} && @{ $MATCH{op} }) {
                    $MATCH = $obj->rule_bit_shift(match=>\%MATCH);
                } else {
                    $MATCH = $MATCH{operand}[0];
                }
            })

# precedence level: left     + - .
        <rule: add>
            <[operand=mult]> ** <[op=(\+|-|\.)]>
            (?{
                if ($MATCH{op} && @{ $MATCH{op} }) {
                    $MATCH = $obj->rule_add(match=>\%MATCH);
                } else {
                    $MATCH = $MATCH{operand}[0];
                }
            })

# precedence level: left     * / % x
        <rule: mult>
            <[operand=unary]> ** <[op=(\*|/|%|x)]>
            (?{
                if ($MATCH{op} && @{ $MATCH{op} }) {
                    $MATCH = $obj->rule_mult(match=>\%MATCH);
                } else {
                    $MATCH = $MATCH{operand}[0];
                }
            })

# precedence level: right    ! ~ unary+ unary-
        <rule: unary>
            <[op=(!|~|\+|-)]>* <operand=power>
            (?{
                if ($MATCH{op} && @{ $MATCH{op} }) {
                    $MATCH = $obj->rule_unary(match=>\%MATCH);
                } else {
                    $MATCH = $MATCH{operand};
                }
            })

# precedence level: right    **
        <rule: power>
            <[operand=subscripting]> ** <[op=(\*\*)]>
            (?{
                if ($MATCH{op} && @{ $MATCH{op} }) {
                    $MATCH = $obj->rule_power(match=>\%MATCH);
                } else {
                    $MATCH = $MATCH{operand}[0];
                }
            })

# precedence level: left    hash[s], array[i]
        <rule: subscripting>
            <operand=var0> <[subscript]>*
            (?{
                if ($MATCH{subscript} && @{ $MATCH{subscript} }) {
                    $MATCH = $obj->rule_subscripting_var(match=>\%MATCH);
                } else {
                    $MATCH = $MATCH{operand};
                }
            })
          | <operand=term> <[subscript]>*
            (?{
                if ($MATCH{subscript} && @{ $MATCH{subscript} }) {
                    $MATCH = $obj->rule_subscripting_expr(match=>\%MATCH);
                } else {
                    $MATCH = $MATCH{operand};
                }
            })

        <rule: subscript>
              \[ <MATCH=term> \]

# precedence level: left     term (variable, str/num literals, func(), (paren))
term           ::= func
                 | var0
                 | str0
                 | undef
                 | num0
                 | bool0
                 | array
                 | hash
                 | ('(') answer (')')        action => rule_parenthesis

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
            <sign0a=([+-]?+)> 0x <num0a=([0-9A-Fa-f]++)>
            (?{ $MATCH = $obj->rule_num(match=>{num=>
                ($MATCH{sign0a} eq '-' ? -1:1) * hex($MATCH{num0a})}) })
          | <sign0b=([+-]?+)> 0o <num0b=([0-7]++)>
            (?{ $MATCH = $obj->rule_num(match=>{num=>
                ($MATCH{sign0b} eq '-' ? -1:1) * oct($MATCH{num0b})}) })
          | <sign0c=([+-]?+)> 0b <num0c=([0-1]++)>
            (?{ $MATCH = $obj->rule_num(match=>{num=>
                ($MATCH{sign0c} eq '-' ? -1:1) * oct("0b".$MATCH{num0c})}) })
          | <num0c=( [+-]?\d++(?:\.\d++)?+ | inf | nan)>
            (?{ $MATCH = $obj->rule_num(match=>{num=>$MATCH{num0c}}) })

        <rule: str0>
            <MATCH=squotestr>
          | <MATCH=dquotestr>

squotepart     ~ ('\\')
               | ("\'")
               | [^\\']+
squotestr      ::= ("'") squotepart* ("'")

#        <token: dquotestr>
#            "<[part=([^"\044\\]++|\$\.\.?|\$\w+|\$\{[^\}]+\}|\\\\|\\'|\\"|\\[tnrfbae\$]|\\[0-7]{1,3}|\\x[0-9A-Fa-f]{1,2}|\\x\{[0-9A-Fa-f]{1,4}\}|\\)]>*"
#            (?{ $MATCH = $obj->rule_dquotestr(match=>\%MATCH) })

        <rule: var0>
            \$ <var=(\w++(?:::\w+)*+)>
            (?{ $MATCH = $obj->rule_var(match=>\%MATCH) })
          | \$ \{ <var=([^\}]++)> \}
            (?{ $MATCH = $obj->rule_var(match=>\%MATCH) })

        <rule: func>
            <func_name=([A-Za-z_]\w*+)> \( \)
            (?{ $MATCH = $obj->rule_func(match=>{func_name=>$MATCH{func_name}, args=>[]}) })
          | <func_name=(map|grep|usort)> \( \{ <expr=answer> \} (?{ push @$subexpr_stack, $CONTEXT }), <input_array=answer> \)
            (?{ my $meth = "rule_func_$MATCH{func_name}";
                $MATCH = $obj->$meth(match=>{expr=>pop(@$subexpr_stack), array=>$MATCH{input_array}}) })
          | <func_name=([A-Za-z_]\w*+)> \( <[args=answer]> ** (,) \)
            (?{ $MATCH = $obj->rule_func(match=>\%MATCH) })

_

use Marpa::R2;
my $grammar = Marpa::R2::Scanless::G->new({source => \$bnf});
my $recce = Marpa::R2::Scanless::R->new({grammar=>$grammar, semantics_package=>"MyActions"});
my $input = q|str => |;
$recce->read(\$input);

sub MyActions::rule_pair_simple {
}
sub MyActions::rule_pair_string {
}

1;
# ABSTRACT: Parse Language::Expr expression

=head1 METHODS

=head2 parse_expr($str, $obj)

Parse expression in $str. Will call various rule_*() methods in $obj.


=head1 KNOWN BUGS

=over 4

=item * Ternary operator is not chainable yet.

=back

=cut
