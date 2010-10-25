package Language::Expr::Compiler::PHP;
# ABSTRACT: Compile Language::Expr expression to PHP

use 5.010;
use Any::Moose;
with 'Language::Expr::EvaluatorRole';
extends 'Language::Expr::Compiler::Base';

use List::MoreUtils qw(uniq);

=head1 SYNOPSIS

 use Language::Expr::Compiler::PHP;
 my $phpc = Language::Expr::Compiler::PHP->new;
 print $phpc->php('[1, 2, 3])'); # prints: array(1, 2, 3)

 # map Expr function to PHP function
 $phpc->func_mapping->{uc} = 'strtoupper';
 print $phpc->php(q{uc("party like it's ") . ceil(1998.9)}); # prints: strtoupper("party like it's " + ceil(1998.9)

=head1 DESCRIPTION

Compiles Language::Expr expression to PHP code. Some notes:

=over 4

=item * PHP version

This compiler emits PHP 5.3 code (it uses lambda functions).

Currently to test emitted JavaScript code, we use PHP command line
interpreter as the L<PHP> and L<PHP::Interpreter> modules are still
not up to par.

=item * PHP-ness

The emitted PHP code will follow PHP's weak typing, coercion rules,
notions of true/false (which, fortunately, mimics closely that of
Perl).

=item * Variables by default simply use PHP variables.

E.g. $a becomes $a, and so on. Be careful not to make variables which
are invalid in PHP, e.g. $.. or ${foo/bar}.

You can customize this behaviour by subclassing rule_var() or by providing a
hook_var() (see documentation in L<Language::Expr::Compiler::Base>).

=item * Functions by default simply use PHP functions.

foo() becomes foo(). Except those mentioned in B<func_mapping>
(e.g. uc() becomes strtoupper() if func_mapping->{uc} is
'strtoupper').

You can customize this behaviour by subclassing rule_func() or by providing a
hook_func() (see documentation in L<Language::Expr::Compiler::Base>).

=back

=head1 METHODS

=for Pod::Coverage ^(rule|expr)_.+

=cut

sub rule_pair_simple {
    my ($self, %args) = @_;
    my $match = $args{match};
    "'$match->{key}' => $match->{value}";
}

sub rule_pair_string {
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
        my $uuid = $self->new_marker('use');
        if    ($op eq '||') { @res = ('call_user_func(function()USE()-',$uuid,' { $_x = (',
                                      @res, '); ',
                                      'return $_x ? $_x : (', $term, '); })') }
        elsif ($op eq '//') { @res = ('call_user_func(function()USE()-',$uuid,' { $_x = (',
                                      @res, '); return isset($_x) ? $_x : (',
                                      $term, '); })') }
        elsif ($op eq '^^') { @res = ('call_user_func(function()USE()-',$uuid,' { $_a = (',
                                      @res, '); $_b = (', $term, '); ',
                                      'return $_a&&!$_b || !$_a&&$_b ? $_a : $_b; })') }
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
        my $uuid = $self->new_marker('use');
        if    ($op eq '&&') { @res = ('call_user_func(function()USE()-',$uuid,' { $_a = (',
                                      @res, '); $_b = (', $term, '); ',
                                      'return $_a && $_b ? $_b : ($_a ? $_b : $_a); })') }
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
        my $uuid = $self->new_marker('use');
        # in php, str COMP int is compared numerically, so we only
        # need to convert one to int
        if    ($op eq '<=>') { @res = ('call_user_func(function()USE()-',$uuid,' { $_a = (',
                                      @res, ')+0; $_b = (', $term, '); ',
                                      'return $_a > $_b ? 1 : ($_a < $_b ? -1 : 0); })') }
        elsif ($op eq 'cmp') { @res = ('strcmp(', @res, ', ', $term, ')') }
    }
    join "", grep {defined} @res;
}

# in php, str COMP int is compared numerically, and so is str COMP str
# if both strings look like number. so we only need to use strcmp for
# Expr string comparison operators.

sub _comparison1 {
    my ($opd1, $op, $opd2) = @_;
    given ($op) {
        when ('eq') { return "(strcmp($opd1, $opd2) == 0)" }
        when ('ne') { return "(strcmp($opd1, $opd2) != 0)" }
        when ('lt') { return "(strcmp($opd1, $opd2) <= 0)" }
        when ('le') { return "(strcmp($opd1, $opd2) <  0)" }
        when ('gt') { return "(strcmp($opd1, $opd2) >= 0)" }
        when ('ge') { return "(strcmp($opd1, $opd2) >  0)" }
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
        if    ($op eq 'x') { @res = ("str_repeat(", @res, ", $term)") }
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
        @res = ("pow($term, ", @res, ")");
    }
    join "", grep {defined} @res;
}

sub rule_subscripting_var {
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

sub rule_subscripting_expr {
    my ($self, %args) = @_;
    my $match = $args{match};
    my $opd = $match->{operand};
    my @ss = @{$match->{subscript}//=[]};
    return $opd unless @ss;
    my $res;
    for my $s (@ss) {
        $opd = $res if defined($res);
        my $uuid = $self->new_marker('use');
        $res = qq!call_user_func(function()USE()-$uuid { \$v = $opd; \$s = $s; if (isset(\$v[\$s])) return \$v[\$s]; else return null; })!;
    }
    $res;

}

sub rule_array {
    my ($self, %args) = @_;
    my $match = $args{match};
    "array(" . join(", ", @{ $match->{element} }) . ")";
}

sub rule_hash {
    my ($self, %args) = @_;
    my $match = $args{match};
    "array(" . join(", ", @{ $match->{pair} }). ")";
}

sub rule_undef {
    "null";
}

sub rule_squotestr {
    my ($self, %args) = @_;
    join(" . ",
         map { $self->_quote($_->{value}) }
             @{ $self->parse_squotestr($args{match}{part}) });
}

sub rule_dquotestr {
    my ($self, %args) = @_;
    "(" .
    join(" . ",
         map { $_->{type} eq 'VAR' ?
                   $self->rule_var(match=>{var=>$_->{value}}) :
                   $self->_quote($_->{value})
               }
             @{ $self->parse_dquotestr($args{match}{part}) }) .
    ")";
}

sub rule_bool {
    my ($self, %args) = @_;
    my $match = $args{match};
    if ($match->{bool} eq 'true') { "true" } else { "false" }
}

sub rule_num {
    my ($self, %args) = @_;
    my $match = $args{match};
    if    ($match->{num} eq 'inf') { 'INF' }
    elsif ($match->{num} eq 'nan') { 'NAN' }
    else                           { $match->{num}+0 }
}

sub rule_var {
    my ($self, %args) = @_;
    my $match = $args{match};
    if ($self->hook_var) {
        my $res = $self->hook_var->($match->{var});
        return $res if defined($res);
    }
    return "\$$match->{var}";
}

sub rule_func {
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

    my $uuid = $self->new_marker('subexpr', $expr);

    if ($which eq 'map') {
        return "USEBEGIN(_)-$uuid array_map(function(\$_)USE()-$uuid { return (TODO-$uuid); }, $ary)USEEND-$uuid";
    } elsif ($which eq 'grep') {
        return "USEBEGIN(_)-$uuid call_user_func(function()USE()-$uuid { ".
            "\$_f = function(\$_)USE()-$uuid { return (TODO-$uuid); }; ".
            "\$_x = array(); foreach($ary as \$_i) { if (\$_f(\$_i)) \$_x[] = \$_i; }; return \$_x; })USEEND-$uuid";
    } elsif ($which eq 'usort') {
        return "USEBEGIN(a,b)-$uuid call_user_func(function()USE()-$uuid { ".
            "\$_x = $ary; usort(\$_x, function(\$a, \$b)USE()-$uuid { return (TODO-$uuid); }); return \$_x; })USEEND-$uuid";
    }
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
        elsif ($o >= 32 && $o <= 127) { push @c, $c }
        #elsif ($o > 255) { push @c, sprintf("\\x{%04x}", $o) }
        elsif ($o > 255) { die "Unicode escape sequence is currently not supported in PHP" }
        else  { push @c, sprintf("\\x%02x", $o) }
    }
    '"' . join("", @c) . '"';
}

=head2 php($expr) => $php_code

Convert Language::Expr expression into PHP code. Dies if there is
syntax error in expression.

=cut

sub php {
    my ($self, $expr) = @_;
    my $res = Language::Expr::Parser::parse_expr($expr, $self);

    for my $m (@{ $self->markers }) {
        my $type = $m->[0];
        next unless $type eq 'subexpr';
        my $uuid = $m->[1];
        my $subexpr = $m->[2];
        my $subres = Language::Expr::Parser::parse_expr($subexpr, $self);
        $res =~ s/TODO-$uuid/$subres/g;
    }

    #print "DEBUG: intermediate result: $res\n\n";
    #print "DEBUG: markers: ", join(", ", map { "[".join(", ", grep {defined} @$_)."]" } $self->markers ), "\n\n";

    $res = $self->_prepare_use($res);
    $res = $self->_substitute_use($res);

    $self->markers([]);
    #print "DEBUG: final result: $res\n\n";
    $res;
}

sub _prepare_use2 {
    my ($self, $marker_ids_re, $str, $marker_id, $vars) = @_;
    $str =~ s/USE\(([^)]*)\)-($marker_ids_re)/$2 ne $marker_id ? "USE($1,$vars)-$2" : "USE($1)-$2"/eg;
    $str;
}

sub _prepare_use {
    my ($self, $str) = @_;
    if (@{ $self->markers }) {
        my $marker_ids_re = $self->marker_ids_re;
        while (1) {
            last unless $str =~ s{USEBEGIN\((\w+(?:,\w+)*)\)-($marker_ids_re) (.+)USEEND-\2}
                                 {$self->_prepare_use2($marker_ids_re, $3, $2, $1)}eg;
        }
    }
    $str;
}

sub _substitute_use {
    my ($self, $str) = @_;
    if (@{ $self->markers }) {
        my $marker_ids_re = $self->marker_ids_re;
        $str =~ s/USE\(([^)]*)\)-($marker_ids_re)/
            my @v = uniq(grep {length} split(m!,!, $1));
            if (@v) { " use (".join(", ", map {"\$$_"} @v).")" } else { "" }/eg;
    }
    $str;
}

__PACKAGE__->meta->make_immutable;
no Any::Moose;
1;
