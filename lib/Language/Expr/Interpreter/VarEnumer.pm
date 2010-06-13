package Language::Expr::Interpreter::VarEnumer;
# Enumerate variables mentioned in Language::Expr expression

use Any::Moose;
with 'Language::Expr::EvaluatorRole';

require Language::Expr::Parser;

=head1 ATTRIBUTES

=head2 result => ARRAYREF

Store the list of variables seen during parsing.

=cut

has result => (is => 'rw');

=head2 METHODS

=for Pod::Coverage ^(rule|expr)_.+

=cut

=head2 add_var(VAR)

Add variable to B<result> if it is not already in there.

=cut

sub add_var {
    my ($self, $v) = @_;
    push @{$self->result}, $v unless $v ~~ @{$self->result};
}

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

sub rule_dquotestr {
    my ($self, %args) = @_;
    my $match = $args{match};

    for (@{ $match->{part} }) {
        if    (/^\$(\w+)$/)    { $self->add_var($1) }
        elsif (/^\$\((.+)\)$/) { $self->add_var($1) }
    }
}

sub rule_bool { }

sub rule_num { }

sub rule_var {
    my ($self, %args) = @_;
    my $match = $args{match};
    $self->add_var($match->{var});
}

sub rule_func { }

sub rule_func_map {
    die "Subexpression not yet supported";
}

sub rule_func_grep {
    die "Subexpression not yet supported";
}

sub rule_func_usort {
    die "Subexpression not yet supported";
}

sub rule_parenthesis {}

sub expr_preprocess {
    my ($self, %args) = @_;
    $self->result([]);
}

sub expr_postprocess {}

=head1 BUGS/TODOS

Currently subexpression (map/grep/usort) doesn't work yet.

=cut

__PACKAGE__->meta->make_immutable;
no Any::Moose;
1;
