package Language::Expr::DummyItp;
# Dummy interpreter for Language::Expr (used for testing)

use Any::Moose;
with 'Language::Expr::InterpreterRole';

require Language::Expr::Parser;

=head1 DESCRIPTION

This interpreter does nothing. It is used only for testing the parser.

=head1 ATTRIBUTES

=head2 METHODS

=for Pod::Coverage ^rule_.+

=cut

sub rule_pair { }
sub rule_or_xor { }
sub rule_and { }
sub rule_bit_or_xor { }
sub rule_bit_and { }
sub rule_equal { }
sub rule_less_greater { }
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
sub rule_preprocess { }
sub rule_postprocess { }

__PACKAGE__->meta->make_immutable;
no Any::Moose;
1;
