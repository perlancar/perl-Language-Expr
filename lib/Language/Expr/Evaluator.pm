package Language::Expr::Evaluator;
# ABSTRACT: Base class for Language::Expr compilers and interpreters

use Any::Moose;
require Language::Expr::Parser;

=head1 METHODS

=head2 eval($expr) => $result

Evaluate expression and return the result.

=cut

sub eval {
    my ($self, $expr) = @_;
    my $res = Language::Expr::Parser::parse_expr($expr, $self);
    $res;
}

__PACKAGE__->meta->make_immutable;
no Any::Moose;
1;
