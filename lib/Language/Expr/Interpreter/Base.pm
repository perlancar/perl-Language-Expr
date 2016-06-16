package Language::Expr::Interpreter::Base;

# DATE
# VERSION

use 5.010;
use strict;
use warnings;

require Language::Expr::Parser;

sub new {
    my $class = shift;
    bless {}, $class;
}

sub eval {
    my ($self, $expr) = @_;
    my $res = Language::Expr::Parser::parse_expr($expr, $self);
    $res;
}

1;
# ABSTRACT: Base class for Language::Expr interpreters

=head1 METHODS

=head2 new()

=head2 eval($expr) => $result

Evaluate expression and return the result.

=cut
