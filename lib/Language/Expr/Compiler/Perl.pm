package Language::Expr::Compiler::Perl;
# ABSTRACT: Compile Language::Expr expression to Perl

use Any::Moose;
with 'Language::Expr::EvaluatorRole';
extends 'Language::Expr::Evaluator';

use UUID::Tiny ':std';
use boolean;

=head1 SYNOPSIS

 use Language::Expr::Compiler::Perl;
 my $plc = Language::Expr::Compiler::Perl->new;
 print $plc->perl('1 ^^ 2'); # prints '1 xor 2'

=head1 DESCRIPTION

Compiles Language::Expr expression to Perl code. Some notes:

=over 4

=item * Emitted Perl code version

Emitted Perl code requires Perl 5.10 (it uses 5.10's "//" defined-or
operator) and also the L<boolean> module (it uses 'true' and 'false'
objects).

=item * Perliness

The emitted Perl code will follow Perl's notion of true and false,
e.g. the expression '"" || "0" || 2' will result to 2 since Perl
thinks that "" and "0" are false. It is also weakly typed like Perl,
i.e. allows '1 + "2"' to become 3.

=item * Currently strings are rudimentary escaped.

Data dumping modules can't be used currently due to segfaults (at
least in 5.10.1).

=item * Variables by default simply use Perl variables.

E.g. $a becomes $a, and so on. Be careful not to make variables which
are invalid in Perl, e.g. $.. or ${foo/bar} (but ${foo::bar} is okay
because it translates to $foo::bar).

You can subclass and override rule_var() if you want to provide your
own variables.

=item * Functions by default simply use Perl functions.

=back

=head1 ATTRIBUTES

=head2 todo => ARRAYREF

Used to remember which subexpression need to be parsed later.

=cut

has todo => (is => 'rw', default => sub { [] });


=head1 METHODS

=for Pod::Coverage ^(rule|expr)_.+

=cut

sub rule_pair {
    my ($self, %args) = @_;
    my $match = $args{match};
    "$match->{key} => $match->{value}";
}

sub rule_or_xor {
    my ($self, %args) = @_;
    my $match = $args{match};
    my @res;
    push @res, shift @{$match->{operand}};
    for my $term (@{$match->{operand}}) {
        my $op = shift @{$match->{op}//=[]};
        last unless $op;
        if    ($op eq '||') { push @res, " || $term" }
        elsif ($op eq '//') { push @res, " // $term" }
        # add parenthesis because perl's xor precendence is low
        elsif ($op eq '^^') { @res = ("(", @res, " xor $term)") }
    }
    join "", grep {defined} @res;
}

sub rule_and {
    my ($self, %args) = @_;
    my $match = $args{match};
    my @res;
    push @res, shift @{$match->{operand}};
    for my $term (@{$match->{operand}}) {
        my $op = shift @{$match->{op}//=[]};
        last unless $op;
        if    ($op eq '&&') { @res = ("((", @res, " && $term) || false)") }
    }
    join "", grep {defined} @res;
}

sub rule_bit_or_xor {
    my ($self, %args) = @_;
    my $match = $args{match};
    my @res;
    push @res, shift @{$match->{operand}};
    for my $term (@{$match->{operand}}) {
        my $op = shift @{$match->{op}//=[]};
        last unless $op;
        if    ($op eq '|') { push @res, " | $term" }
        elsif ($op eq '^') { push @res, " ^ $term" }
    }
    join "", grep {defined} @res;
}

sub rule_bit_and {
    my ($self, %args) = @_;
    my $match = $args{match};
    my @res;
    push @res, shift @{$match->{operand}};
    for my $term (@{$match->{operand}}) {
        my $op = shift @{$match->{op}//=[]};
        last unless $op;
        if    ($op eq '&') { push @res, " & $term" }
    }
    join "", grep {defined} @res;
}

sub rule_comparison3 {
    my ($self, %args) = @_;
    my $match = $args{match};
    my @res;
    push @res, shift @{$match->{operand}};
    for my $term (@{$match->{operand}}) {
        my $op = shift @{$match->{op}//=[]};
        last unless $op;
        if    ($op eq '<=>') { push @res, " <=> $term" }
        elsif ($op eq 'cmp') { push @res, " cmp $term" }
    }
    join "", grep {defined} @res;
}

sub rule_comparison {
    my ($self, %args) = @_;
    my $match = $args{match};
    my @opds;
    push @opds, shift @{$match->{operand}};
    return '' unless defined $opds[0];
    my @ops;
    for my $term (@{$match->{operand}}) {
        push @opds, $term;
        my $op = shift @{$match->{op}//=[]};
        last unless $op;
        if    ($op eq '==' ) { push @ops, '=='  }
        elsif ($op eq '!=' ) { push @ops, '!='  }
        elsif ($op eq 'eq' ) { push @ops, 'eq'  }
        elsif ($op eq 'ne' ) { push @ops, 'ne'  }
        elsif ($op eq '<'  ) { push @ops, '<'   }
        elsif ($op eq '<=' ) { push @ops, '<='  }
        elsif ($op eq '>'  ) { push @ops, '>'   }
        elsif ($op eq '>=' ) { push @ops, '>='  }
        elsif ($op eq 'lt' ) { push @ops, 'lt'  }
        elsif ($op eq 'le' ) { push @ops, 'le'  }
        elsif ($op eq 'gt' ) { push @ops, 'gt'  }
        elsif ($op eq 'ge' ) { push @ops, 'ge'  }
    }
    return $opds[0] unless @ops;
    my @res;
    my $lastopd;
    my ($opd1, $opd2);
    while (@ops) {
        my $op = pop @ops;
        if (defined($lastopd)) {
            $opd2 = $lastopd;
            $opd1 = pop @opds;
        } else {
            $opd2 = pop @opds;
            $opd1 = pop @opds;
        }
        if (@res) {
            @res = ("(($opd1 $op $opd2) ? ", @res, " : false)");
        } else {
            push @res, "($opd1 $op $opd2 ? true:false)";
        }
        $lastopd = $opd1;
    }
    join "", @res;
}

sub rule_bit_shift {
    my ($self, %args) = @_;
    my $match = $args{match};
    my @res;
    push @res, shift @{$match->{operand}};
    for my $term (@{$match->{operand}}) {
        my $op = shift @{$match->{op}//=[]};
        last unless $op;
        if    ($op eq '>>') { push @res, " >> $term" }
        elsif ($op eq '<<') { push @res, " << $term" }
    }
    join "", grep {defined} @res;
}

sub rule_add {
    my ($self, %args) = @_;
    my $match = $args{match};
    my @res;
    push @res, shift @{$match->{operand}};
    for my $term (@{$match->{operand}}) {
        my $op = shift @{$match->{op}//=[]};
        last unless $op;
        if    ($op eq '.') { push @res, " . $term" }
        if    ($op eq '+') { push @res, " + $term" }
        if    ($op eq '-') { push @res, " - $term" }
    }
    join "", grep {defined} @res;
}

sub rule_mult {
    my ($self, %args) = @_;
    my $match = $args{match};
    my @res;
    push @res, shift @{$match->{operand}};
    for my $term (@{$match->{operand}}) {
        my $op = shift @{$match->{op}//=[]};
        last unless $op;
        if    ($op eq '*') { push @res, " * $term" }
        if    ($op eq '/') { push @res, " / $term" }
        if    ($op eq '%') { push @res, " % $term" }
        if    ($op eq 'x') { push @res, " x $term" }
    }
    join "", grep {defined} @res;
}

sub rule_unary {
    my ($self, %args) = @_;
    my $match = $args{match};
    my @res;
    push @res, $match->{operand};
    for my $op (reverse @{$match->{op}//=[]}) {
        last unless $op;
        # use paren because --x or ++x is interpreted as pre-decrement/increment
        if    ($op eq '!') { @res = ("(", @res, " ? false:true)") }
        if    ($op eq '-') { @res = ("-(", @res, ")") }
        if    ($op eq '~') { @res = ("~(", @res, ")") }
    }
    join "", grep {defined} @res;
}

sub rule_power {
    my ($self, %args) = @_;
    my $match = $args{match};
    my @res;
    push @res, shift @{$match->{operand}};
    for my $term (@{$match->{operand}}) {
        push @res, " ** $term";
    }
    join "", grep {defined} @res;
}

sub rule_subscripting {
    my ($self, %args) = @_;
    my $match = $args{match};
    my $opd = $match->{operand};
    my @ss = @{$match->{subscript}//=[]};
    return $opd unless @ss;
    my $res;
    for my $s (@ss) {
        $opd = $res if defined($res);
        $res = qq!(do { my (\$v) = ($opd); my (\$s) = ($s); !.
            qq!if (ref(\$v) eq 'HASH') { \$v->{\$s} } !.
                qq!elsif (ref(\$v) eq 'ARRAY') { \$v->[\$s] } else { !.
                    qq!die "Invalid subscript \$s for \$v" } })!;
    }
    $res;
}

sub rule_array {
    my ($self, %args) = @_;
    my $match = $args{match};
    "[" . join(", ", @{ $match->{element} }) . "]";
}

sub rule_hash {
    my ($self, %args) = @_;
    my $match = $args{match};
    "{" . join(", ", @{ $match->{pair} }). "}";
}

sub rule_undef {
    "undef";
}

sub rule_squotestr {
    __quote(Language::Expr::Interpreter::Default::rule_squotestr(@_));
}

sub rule_dquotestr {
    __quote(Language::Expr::Interpreter::Default::rule_dquotestr(@_));
}

sub rule_bool {
    my ($self, %args) = @_;
    my $match = $args{match};
    if ($match->{bool} eq 'true') { "true" } else { "false" }
}

sub rule_num {
    my ($self, %args) = @_;
    my $match = $args{match};
    if    ($match->{num} eq 'inf') { '"Inf"' }
    elsif ($match->{num} eq 'nan') { '"NaN"' }
    else                           { $match->{num}+0 }
}

sub rule_var {
    my ($self, %args) = @_;
    my $match = $args{match};
    "\$$match->{var}";
}

sub rule_func {
    my ($self, %args) = @_;
    my $match = $args{match};
    my $f = $match->{func_name};
    my $args = $match->{args};
    "$f(".join(", ", @$args).")";
}

sub _map_grep_usort {
    my ($which, $self, %args) = @_;
    my $match = $args{match};
    my $ary = $match->{array};
    my $expr = $match->{expr};

    my $perlop = $which eq 'map' ? 'map' : $which eq 'grep' ? 'grep' : 'sort';
    my $todoid = __uuidgen(); # yes, this is not proper
    push @{ $self->todo }, [$todoid, $expr];
    "[$perlop({ TODO-$todoid } \@{$ary})]";
}

sub rule_func_map {
    _map_grep_usort('map', @_);
}

sub rule_func_grep {
    _map_grep_usort('grep', @_);
}

sub rule_func_usort {
    _map_grep_usort('usort', @_);
}

sub rule_parenthesis {
    my ($self, %args) = @_;
    my $match = $args{match};
    "(" . $match->{answer} . ")";
}

sub expr_preprocess {}

sub expr_postprocess {
    my ($self, %args) = @_;
    my $result = $args{result};
    $result;
}

# can't use regex here (perl segfaults), at least in 5.10.1, because
# we are in one big re::gr regex.
sub __quote {
    my @c;
    for my $c (split '', $_[0]) {
        my $o = ord($c);
        if    ($c eq '"') { push @c, '\\"' }
        elsif ($c eq "\\") { push @c, "\\\\" }
        elsif ($c eq '$') { push @c, "\\\$" }
        elsif ($c eq '@') { push @c, '\\@' }
        elsif ($o >= 32 && $o <= 127) { push @c, $c }
        elsif ($o > 255) { push @c, sprintf("\\x{%04x}", $o) }
        else  { push @c, sprintf("\\x%02x", $o) }
    }
    '"' . join("", @c) . '"';
}

sub __uuidgen {
    UUID::Tiny::create_uuid_as_string(UUID_V4);
}

=head2 perl($expr) => $perl_code

Convert Language::Expr expression into Perl code. Dies if there is
syntax error in expression.

=cut

sub perl {
    my ($self, $expr) = @_;
    my $res = Language::Expr::Parser::parse_expr($expr, $self);
    for my $todo (@{ $self->todo }) {
        my $todoid = $todo->[0];
        my $subexpr = $todo->[1];
        my $subres = Language::Expr::Parser::parse_expr($subexpr, $self);
        $res =~ s/TODO-$todoid/$subres/g;
    }
    $self->todo([]);
    $res;
}

sub eval {
    my ($self, $expr) = @_;
    my $res = eval $self->perl($expr);
    die $@ if $@;
    $res;
}

__PACKAGE__->meta->make_immutable;
no Any::Moose;
1;
