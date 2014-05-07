package Language::Expr::Evaluator;

use 5.010;
use strict;
use warnings;

use Moo;
require Language::Expr::Parser;

# VERSION
# DATE

sub eval {
    my ($self, $expr) = @_;
    my $res = Language::Expr::Parser::parse_expr($expr, $self);
    $res;
}

1;
# ABSTRACT: Base class for Language::Expr compilers and interpreters

=head1 METHODS

=head2 eval($expr) => $result

Evaluate expression and return the result.
