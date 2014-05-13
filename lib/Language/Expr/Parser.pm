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
:default           ::= action=>::first
:start             ::= answer

answer             ::= or_xor

# precedence level : left     =>
pair               ::= word '=>' value                         action=>rule_pair_simple
                     | squotestr '=>' value                    action=>rule_pair_string
                     | dquotestr '=>' value                    action=>rule_pair_string
word                 ~ [\w]+
value              ::= answer

# precedence level : left     || // ^^
or_xor             ::= ternary
                     | or_xor op_or_xor or_xor                 action=>rule_or_xor
op_or                ~ '||'
                     | '//'
                     | '^^'

# precedence level : right    ?:
ternary            ::= and
                    || ternary '?' ternary ':' ternary         action=>rule_ternary assoc=>right

# precedence level : left     &&
and                ::= bit_or_xor
                     | and '&&' and                            action=>rule_and

# precedence level : left     | ^
bit_or_xor         ::= bit_and
                     | bit_or_xor op_bit_or_xor bit_or_xor     action=>rule_bit_or_xor
op_bit_or_xor        ~ '|'
                     | '^'

# precedence level : left     &
bit_and            ::= comparison3
                     | bit_and '&' bit_and                     action=>rule_bit_and

# precedence level: group     <=> cmp
comparison3        ::= comparison
                     | comparison3 op_comparison3 comparison3  action=>rule_comparison3 assoc=>group

# precedence level: left == != eq ne < > <= >= ge gt le lt
comparison         ::= bit_shift
                     | comparison op_comparison comparison     action=>rule_comparison
op_comparison        ~ '=='
                     | '!='
                     | 'eq'
                     | 'ne'
                     | '>='
                     | '>'
                     | '<='
                     | '<'
                     | 'lt'
                     | 'gt'
                     | 'le'
                     | 'ge'

# precedence level: left     << >>
bit_shift          ::= add
                     | bit_shift op_bit_shift bit_shift        action=>rule_bit_shift
op_bit_shift         ~ '<<'
                     | '>>'

# precedence level: left     + - .
add                ::= mult
                     | add op_add add                          action=>rule_add
op_add               ~ '+'
                     | '-'
                     | '.'

# precedence level: left     * / % x
mult               ::= unary
                     | mult op_mult mult                       action=>rule_mult
op_mult              ~ '*'
                     | '/'
                     | '%'
                     | 'x'

# precedence level: right    ! ~ unary+ unary-
unary              ::= power
                     | op_unary unary                          action=>rule_unary
op_unary             ~ '!'
                     | '~'
                     | '+'
                     | '-'

# precedence level: right    **
power              ::= subscripting
                    || power '**' power                        action=>rule_power assoc=>right

# precedence level: left    hash[s], array[i]
subscripting       ::= var0 '[' term ']'                       action=>rule_subscripting_var
                     | term '[' term ']'                       action=>rule_subscripting_expr

# precedence level: left     term (variable, str/num literals, func(), (paren))
term               ::= func
                     | var
                     | str
                     | undef
                     | num
                     | bool
                     | array
                     | hash
                     | '(' answer ')'                          action=>rule_parenthesis

array              ::= '[' members ']'                         action=>rule_array
members            ::= answer*                                 separator=>comma action=[values]
comma                ~ ','

hash               ::= '{' pairs '}'                           action=>rule_hash
pairs              ::= pair*                                   separator=>comma action=[values]

undef                ~ 'undef'

bool                 ~ 'true'
                     | 'false'

hexdigits            ~ [0-9A-Fa-f]+
decdigits            ~ [0-9]+
decdigits            ~ [0-7]+
bindigits            ~ [01]+

sign                 ~ [+-]
num                  ~      '0x' hexdigits
                     | sign '0x' hexdigits
                     |      '0o' octdigits
                     | sign '0o' octdigits
                     |      '0b' bindigits
                     | sign '0b' bindigits
                     |      decdigits
                     | sign decdigits
                     |      decdigits '.' decdigits
                     | sign decdigits '.' decdigits
                     |      decdigits [Ee] decdigits
                     | sign decdigits [Ee] decdigits
                     |      decdigits [Ee] sign decdigits
                     | sign decdigits [Ee] sign decdigits
                     |      decdigits '.' decdigits [Ee] decdigits
                     | sign decdigits '.' decdigits [Ee] decdigits
                     |      decdigits '.' decdigits [Ee] sign decdigits
                     | sign decdigits '.' decdigits [Ee] sign decdigits

str                  ~ squotestr
                     | dquotestr

squotepart           ~ '\\'
                     | '\' [\x27]
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
