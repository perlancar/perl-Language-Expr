package Language::Expr::Compiler::JS;
# Compile Language::Expr expression to JS

use 5.010;
use Any::Moose;
with 'Language::Expr::EvaluatorRole';
extends 'Language::Expr::Evaluator';

use UUID::Tiny ':std';
use Language::Expr::Interpreter::Default;

=head1 SYNOPSIS

 use Language::Expr::Compiler::JS;
 my $jsc = Language::Expr::Compiler::JS;
 print $jsc->js('map({$_**2}, [1, 2, 3])'); # prints '[1, 2, 3].map(function(_){ Math.pow(_, 2) })'

=head1 DESCRIPTION

Compiles Language::Expr expression to JS code. Some notes:

=over 4

=item * JavaScript version

This compiler emits JavaScript 1.8.1 code (it uses several Array
methods like map() and filter() [supported since JavaScript 1.6],
'let' lexical variables [supported since JavaScript 1.7 / Firefox 2]
and native JSON [supported since JavaScript 1.8.1 / Firefox 3.5]).

Currently to test emitted JavaScript code, we use Spidermonkey 1.9.1+
JavaScript interpreter (typically located in /usr/bin/js) as the
L<JavaScript> and L<JE> modules are still not up to par.

=item * JavaScript-ness

The emitted JS code will follow JavaScript's weak typing and coercion
rules, e.g. Expr '1+"2"' will simply be translated to JavaScript
'1+"2"' and will result in "12".

=item * Currently strings are rudimentary escaped.

Data dumping modules can't be used currently due to segfaults (at
least in 5.10.1).

=item * Variables by default simply use JavaScript variables.

E.g. $a becomes a, and so on. Be careful not to make variables which
are invalid in JavaScript, e.g. $.. or ${foo/bar}.

You can subclass and override rule_var() if you want to provide your
own variables.

=item * Functions by default simply use Javascript functions.

foo() becomes foo(). Except those mentioned in B<func_mapping>
(e.g. rand() becomes Math.rand() if func_mapping->{rand} is
'Math.rand').

Or you can subclass and override rule_func() for more translation
freedom.

=back

=head1 ATTRIBUTES

=head2 todo => ARRAYREF

Used to remember which subexpression need to be parsed later.

=cut

has todo => (is => 'rw', default => sub { [] });

=head2 func_mapping => HASHREF

Mapping from Expr function to JavaScript functions/methods. Example:

 { ceil => 'Math.ceil',
   uc   => '.toUpperCase',
 }

=cut

has func_mapping => (is => 'rw', default => sub { {} });

=head1 METHODS

=for Pod::Coverage ^(rule|expr)_.+

=cut

sub rule_pair {
    my ($self, %args) = @_;
    my $match = $args{match};
    "$match->{key}:$match->{value}";
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
        elsif ($op eq '//') { @res = ("(function() { let _x = (",
                                      @res, "); return _x==null ? (",
                                      $term, ") : _x })()") }
        elsif ($op eq '//') { @res = ("(function() { let _a = (",
                                      @res, "); let _b = ($term); ",
                                      "return _a&&!_b || !_a&&_b ? _a : _b })()") }
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
        if    ($op eq '<=>') { @res = ("(function() { let _a = (",
                                      @res, "); let _b = ($term); ",
                                      "return _a > _b ? 1 : (_a < _b ? -1 : 0) })()") }
        elsif ($op eq 'cmp') { @res = ("(function() { let _a = (",
                                      @res, ") + ''; let _b = ($term) + ''; ",
                                      "return _a > _b ? 1 : (_a < _b ? -1 : 0) })()") }
    }
    join "", grep {defined} @res;
}

sub _comparison1 {
    my ($opd1, $op, $opd2) = @_;
    given ($op) {
        when ('eq') { return "(function() { let _a = ($opd1) + ''; let _b = ($opd2) + ''; return $opd1 == $opd2 })()" }
        when ('ne') { return "(function() { let _a = ($opd1) + ''; let _b = ($opd2) + ''; return $opd1 != $opd2 })()" }
        when ('lt') { return "(function() { let _a = ($opd1) + ''; let _b = ($opd2) + ''; return $opd1 <  $opd2 })()" }
        when ('le') { return "(function() { let _a = ($opd1) + ''; let _b = ($opd2) + ''; return $opd1 <= $opd2 })()" }
        when ('gt') { return "(function() { let _a = ($opd1) + ''; let _b = ($opd2) + ''; return $opd1 >  $opd2 })()" }
        when ('ge') { return "(function() { let _a = ($opd1) + ''; let _b = ($opd2) + ''; return $opd1 >= $opd2 })()" }
        default { return "($opd1 $op $opd2)" }
    }
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
            @res = ("("._comparison1($opd1, $op, $opd2)." ? ", @res, " : false)");
        } else {
            push @res, _comparison1($opd1, $op, $opd2);
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
        if    ($op eq '.') { @res = ("'' + ", @res, " + $term") }
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
        if    ($op eq 'x') { @res = ("(new Array(1 + $term).join(", @res, "))") }
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
        if    ($op eq '!') { @res = ("!(", @res, ")") }
        if    ($op eq '-') { @res = ("-(", @res, ")") }
        if    ($op eq '~') { @res = ("~(", @res, ")") }
    }
    join "", grep {defined} @res;
}

sub rule_power {
    my ($self, %args) = @_;
    my $match = $args{match};
    my @res;
    push @res, pop @{$match->{operand}};
    for my $term (reverse @{$match->{operand}}) {
        @res = ("Math.pow($term, ", @res, ")");
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
        $res = $opd . "[$s]";
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
    "null";
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
    if    ($match->{num} eq 'inf') { 'Infinity' }
    elsif ($match->{num} eq 'nan') { 'NaN' }
    else                           { $match->{num}+0 }
}

sub rule_var {
    my ($self, %args) = @_;
    my $match = $args{match};
    "$match->{var}";
}

sub rule_func {
    my ($self, %args) = @_;
    my $match = $args{match};
    my $f = $match->{func_name};
    my $fmap = $self->func_mapping->{$f};
    $f = $fmap if $fmap;
    my $args = $match->{args};
    if (substr($f, 0, 1) eq '.') {
        my $invoc = shift @$args;
        return "($invoc)$f(".join(", ", @$args).")";
    } else {
        return "$f(".join(", ", @$args).")";
    }
}

sub _map_grep_usort {
    my ($which, $self, %args) = @_;
    my $match = $args{match};
    my $ary = $match->{array};
    my $expr = $match->{expr};

    my $todoid = __uuidgen(); # yes, this is not proper
    push @{ $self->todo }, [$todoid, $expr];
    if ($which eq 'map') {
        return "($ary).map(function(_){ return (TODO-$todoid); })";
    } elsif ($which eq 'grep') {
        return "($ary).filter(function(_){ return (TODO-$todoid); })";
    } elsif ($which eq 'usort') {
        return "($ary).map(function(x) x).sort(function(a, b){ return (TODO-$todoid); })";
    }
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
        elsif ($o >= 32 && $o <= 127) { push @c, $c }
        elsif ($o > 255) { push @c, sprintf("\\x{%04x}", $o) }
        else  { push @c, sprintf("\\x%02x", $o) }
    }
    '"' . join("", @c) . '"';
}

sub __uuidgen {
    UUID::Tiny::create_uuid_as_string(UUID_V4);
}

=head2 js($expr) => $js_code

Convert Language::Expr expression into JavaScript code. Dies if there
is syntax error in expression.

=cut

sub js {
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
    my $res = eval $self->js($expr);
    die $@ if $@;
    $res;
}

__PACKAGE__->meta->make_immutable;
no Any::Moose;
1;
