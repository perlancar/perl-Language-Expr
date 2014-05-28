package Language::Expr::Compiler::Perl;

use 5.010;
use strict;
use warnings;

use Moo;
with 'Language::Expr::EvaluatorRole';
extends 'Language::Expr::Compiler::Base';

use boolean;

# VERSION
# DATE

sub rule_pair {
    my $h = shift;
    "$_[0] => $_[1]";
}

sub rule_or_xor {
    my $h = shift;
    my ($opn1, $op, $opn2) = @_;
    if    ($op eq '||') { "$opn1 || $opn2" }
    elsif ($op eq '//') { "$opn1 // $opn2" }
    elsif ($op eq '^^') { "($opn1 xor $opn2)" }
}

sub rule_and {
    my $h = shift;
    "(($_[0] && $_[2]) || false)";
}

sub rule_ternary {
    my $h = shift;
    "$_[0] ? $_[1] : $_[2]";
}

sub rule_bit_or_xor {
    my $h = shift;
    my ($opn1, $op, $opn2) = @_;
    if    ($op eq '|') { "$opn1 | $opn2" }
    elsif ($op eq '^') { "$opn1 ^ $opn2" }
}

sub rule_bit_and {
    my $h = shift;
    my ($opn1, $op, $opn2) = @_;
    if    ($op eq '&') { "$opn1 & $opn2" }
}

sub rule_comparison3 {
    my $h = shift;
    my ($opn1, $op, $opn2) = @_;
    if    ($op eq '<=>') { "$opn1 <=> $opn2" }
    elsif ($op eq 'cmp') { "$opn1 cmp $opn2" }
}

sub rule_comparison {
    my $h = shift;
    my ($opn1, $op, $opn2) = @_;
    "($opn1 $op $opn2) ? true : false)";
}

sub rule_bit_shift {
    my $h = shift;
    my ($opn1, $op, $opn2) = @_;
    if    ($op eq '>>') { "$opn1 >> $opn2" }
    elsif ($op eq '<<') { "$opn1 << $opn2" }
}

sub rule_add {
    my $h = shift;
    my ($opn1, $op, $opn2) = @_;
    if    ($op eq '.') { "$opn1 . $opn2" }
    elsif ($op eq '+') { "$opn1 + $opn2" }
    elsif ($op eq '-') { "$opn1 - $opn2" }
}

sub rule_mult {
    my $h = shift;
    my ($opn1, $op, $opn2) = @_;
    if    ($op eq '*') { "$opn1 * $opn2" }
    elsif ($op eq '/') { "$opn1 / $opn2" }
    elsif ($op eq '%') { "$opn1 % $opn2" }
    elsif ($op eq 'x') { "$opn1 x $opn2" }
}

sub rule_unary {
    my $h = shift;
    my ($op, $opn) = @_;
    if    ($op eq '!') { "($opn ? false:true)" }
    # we use paren because --x or ++x is interpreted as pre-decrement/increment
    elsif ($op eq '~') { "~($opn)" }
    elsif ($op eq '-') { "-($opn)" }
    elsif ($op eq '+') { "+($opn)" }
}

sub rule_power {
    my $h = shift;
    "$_[0] ** $_[2]";
}

sub rule_subscripting_var {
    my $h = shift;
    rule_subscripting_expr($h, @_);
}

sub rule_subscripting_expr {
    my $h = shift;
    my ($opn, $s) = @_;
    qq!(do { my (\$v) = ($opn); my (\$s) = ($s); !.
        qq!if (ref(\$v) eq 'HASH') { \$v->{\$s} } !.
            qq!elsif (ref(\$v) eq 'ARRAY') { \$v->[\$s] } else { !.
                qq!die "Invalid subscript \$s for \$v" } })!;
}

sub rule_array {
    my $h = shift;
    "[" . join(", ", @{ $_[0] }) . "]";
}

sub rule_hash {
    my $h = shift;
    "{" . join(", ", @{ $_[0] }). "}";
}

sub rule_undef {
    "undef";
}

sub rule_squotestr {
    my $h = shift;
    # TODO
}

sub rule_dquotestr {
    my $h = shift;
    # TODO
}

sub rule_bool {
    my $h = shift;
    # TODO
}

sub rule_num {
    my ($self, %args) = @_;
    # TODO
}

sub rule_var {
    my $h = shift;
    # TODO
    my ($self, %args) = @_;
    my $match = $args{match};
    if ($self->hook_var) {
        my $res = $self->hook_var->($match->{var});
        return $res if defined($res);
    }
    return "\$$match->{var}";
}

sub rule_func {
    my $h = shift;
    # TODO
    my ($self, %args) = @_;
    my $match = $args{match};
    my $f = $match->{func_name};
    my $args = $match->{args};
    if ($self->hook_func) {
        my $res = $self->hook_func->($f, @$args);
        return $res if defined($res);
    }
    my $fmap = $self->func_mapping->{$f};
    $f = $fmap if $fmap;
    "$f(".join(", ", @$args).")";
}

sub _map_grep_usort {
    my ($self, $which, %args) = @_;
    my $match = $args{match};
    my $ary = $match->{array};
    my $expr = $match->{expr};

    my $perlop = $which eq 'map' ? 'map' : $which eq 'grep' ? 'grep' : 'sort';
    my $uuid = $self->new_marker('subexpr', $expr);
    "[$perlop({ TODO-$uuid } \@{$ary})]";
}

sub rule_func_map {
    my ($self, %args) = @_;
    $self->_map_grep_usort('map', %args);
}

sub rule_func_grep {
    my ($self, %args) = @_;
    $self->_map_grep_usort('grep', %args);
}

sub rule_func_usort {
    my ($self, %args) = @_;
    $self->_map_grep_usort('usort', %args);
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
sub _quote {
    my ($self, $str) = @_;
    my @c;
    for my $c (split '', $str) {
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

sub perl {
    my ($self, $expr) = @_;
    my $res = Language::Expr::Parser::parse_expr($expr, ref($self));
    for my $m (@{ $self->markers }) {
        my $type = $m->[0];
        next unless $type eq 'subexpr';
        my $uuid = $m->[1];
        my $subexpr = $m->[2];
        my $subres = Language::Expr::Parser::parse_expr($subexpr, $self);
        $res =~ s/TODO-$uuid/$subres/g;
    }
    $self->markers([]);
    $res;
}

sub eval {
    my ($self, $expr) = @_;
    no strict;
    my $res = eval $self->perl($expr);
    die $@ if $@;
    $res;
}

1;
# ABSTRACT: Compile Language::Expr expression to Perl

=for Pod::Coverage ^(rule|expr)_.+

=head1 SYNOPSIS

 use Language::Expr::Compiler::Perl;
 my $plc = Language::Expr::Compiler::Perl->new;
 print $plc->perl('1 ^^ 2'); # prints '1 xor 2'


=head1 DESCRIPTION

Compiles Language::Expr expression to Perl code. Some notes:

=over 4

=item * Emitted Perl code version

Emitted Perl code requires Perl 5.10 (it uses 5.10's "//" defined-or operator)
and also the L<boolean> module (it uses 'true' and 'false' objects).

=item * Perliness

The emitted Perl code will follow Perl's notion of true and false, e.g. the
expression '"" || "0" || 2' will result to 2 since Perl thinks that "" and "0"
are false. It is also weakly typed like Perl, i.e. allows '1 + "2"' to become 3.

=item * Variables by default simply use Perl variables.

E.g. $a becomes $a, and so on. Be careful not to make variables which are
invalid in Perl, e.g. $.. or ${foo/bar} (but ${foo::bar} is okay because it
translates to $foo::bar).

You can customize this behaviour by subclassing rule_var() or by providing a
hook_var() (see documentation in L<Language::Expr::Compiler::Base>).

=item * Functions by default simply use Perl functions.

Unless those specified in func_mapping. For example, if
$compiler->func_mapping->{foo} = "Foo::do_it", then the expression 'foo(1)' will
be compiled into 'Foo::do_it(1)'.

You can customize this behaviour by subclassing rule_func() or by providing a
hook_func() (see documentation in L<Language::Expr::Compiler::Base>).

=back


=head1 METHODS

=head2 perl($expr) => $perl_code

Convert Language::Expr expression into Perl code. Dies if there is
syntax error in expression.
