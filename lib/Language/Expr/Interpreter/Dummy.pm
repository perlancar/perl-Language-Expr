package Language::Expr::Interpreter::Dummy;
# Dummy interpreter for Language::Expr (used for testing)

use Any::Moose;
with 'Language::Expr::EvaluatorRole';

require Language::Expr::Parser;

=head1 DESCRIPTION

This interpreter does nothing. It is used only for testing the parser.

=head1 ATTRIBUTES

=head2 METHODS

=for Pod::Coverage ^(rule|expr)_.+

=cut

sub rule_pair { }
sub rule_or_xor { }
sub rule_and { }
sub rule_bit_or_xor { }
sub rule_bit_and { }
sub rule_comparison3 { }
sub rule_comparison { }
sub rule_bit_shift { }
sub rule_add { }
sub rule_mult { }
sub rule_unary { }
sub rule_power { }
sub rule_subscripting { }
sub rule_array { }
sub rule_hash { }
sub rule_undef { }
sub rule_squotestr { }
sub rule_dquotestr { }
sub rule_bool { }
sub rule_num { }
sub rule_var { }
sub rule_func { }
sub rule_func_map { }
sub rule_func_grep { }
sub rule_func_usort { }
sub rule_parenthesis { }
sub expr_preprocess { }
sub expr_postprocess { }

__PACKAGE__->meta->make_immutable;
no Any::Moose;
1;
