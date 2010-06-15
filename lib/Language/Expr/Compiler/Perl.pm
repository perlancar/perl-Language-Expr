package Language::Expr::Compiler::Perl;
# Compile Language::Expr expression to Perl

use Any::Moose;
with 'Language::Expr::EvaluatorRole';

require Language::Expr::Interpreter::Default;

=head1 DESCRIPTION

Compiles Language::Expr expression to Perl code. Some notes:

=over 4

=item * Currently strings are rudimentary escaped.

Data dumping modules can't be used currently due to segfaults (at
least in 5.10.1).

=item * Variables by default simply use Perl variables.

E.g. $a becomes $a, and so on. Be careful not to make variables which
are invalid in Perl, e.g. $.. or ${foo/bar} (but ${foo::bar} is okay
because it translates to $foo::bar).

You can subclass and override rule_var() if you want to provide your
own variables.

=back

=head2 METHODS

=for Pod::Coverage ^((rule|expr)_.+|quote)$

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
        if    ($op eq '&&') { push @res, " && $term" }
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
            @res = ("(($opd1 $op $opd2) ? ", @res, " : '')");
        } else {
            push @res, "($opd1 $op $opd2)";
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
        if    ($op eq '!') { unshift @res, "!" }
        if    ($op eq '-') { unshift @res, "-" }
        if    ($op eq '~') { unshift @res, "~" }
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
    my @res;
    my $opd = $match->{operand};
    my @ss = @{$match->{subscript}//=[]};
    return $opd unless @ss;
    for my $s (@ss) {
        push @res, qq!(do { my (\$v) = ($opd); my (\$s) = ($s); !.
            qq!if (ref(\$v)) eq 'HASH') { \$v->{\$s} } !.
                qq!elsif (ref(\$v) eq 'ARRAY') { \$v->[\$s] } else { !.
                    qq!die "Invalid subscript \$s for \$v" } })!;
    }
    join "", grep {defined} @res;
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
    quote(Language::Expr::Interpreter::Default::rule_squotestr(@_));
}

sub rule_dquotestr {
    quote(Language::Expr::Interpreter::Default::rule_dquotestr(@_));
}

sub rule_bool {
    my ($self, %args) = @_;
    my $match = $args{match};
    if ($match->{bool} eq 'true') { 1 } else { "''" }
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
    my $res;
    if ($self->funcs->{$f}) {
        return $self->funcs->{$f}->(@$args);
    } else {
        die "Unknown function $f";
    }
}

sub _map_grep_usort {
    my ($which, $self, %args) = @_;
    my $match = $args{match};
    my $ary = $match->{array};
    my $expr = $match->{expr};
    die "Second argument to map/grep/usort must be an array"
        unless ref($ary) eq 'ARRAY';
    local $self->{level} = $self->{level}+1;
    print "DEBUG: _map_grep_usort: level=$self->{level}, expr=`$expr`, array=[".join(",", @$ary),"]\n";
    my $res;
    if ($which eq 'map') {
        $res = [];
        local $self->{vars}{_};
        for (@$ary) {
            $self->{vars}{_} = $_;
            push @$res, Language::Expr::Parser::parse_expr($expr, $self,
                                                           $self->level);
            push @$res, $_;
        }
    } elsif ($which eq 'grep') {
        local $self->{vars}{_};
        $res = [ grep {
            $self->{vars}{_} = $_;
            $self->Language::Expr::Parser::parse_expr($expr, $self,
                                                      $self->level)
        } @$ary];
    } elsif ($which eq 'usort') {
        local $self->{vars}{a};
        local $self->{vars}{b};
        $res = [ sort {
            $self->{vars}{a} = $a;
            $self->{vars}{b} = $b;
            Language::Expr::Parser::parse_expr($expr, $self,
                                               $self->level)
        } @$ary];
    }
    $res;
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
sub quote {
    my @c;
    for my $c (split '', $_[0]) {
        my $o = ord($c);
        if    ($c eq "'") { push @c, "\\'" }
        elsif ($c eq "\\") { push @c, "\\\\" }
        elsif ($o >= 32 && $o <= 127) { push @c, $c }
        else { push @c, sprintf("\\x%02x", $o) }
    }
    "'" . join("", @c) . "'";
}

__PACKAGE__->meta->make_immutable;
no Any::Moose;
1;
