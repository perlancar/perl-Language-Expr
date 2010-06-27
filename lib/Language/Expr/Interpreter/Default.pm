package Language::Expr::Interpreter::Default;
# ABSTRACT: A default interpreter for Language::Expr

use Any::Moose;
with 'Language::Expr::EvaluatorRole';
extends 'Language::Expr::Evaluator';
use List::Util 'reduce';
use boolean;

=head1 DESCRIPTION

Interprets Language::Expr expression. Some notes:

=over 4

=item * Uses L<boolean> module.

Comparison like '1 > 1' and '

=item * Follows Perl's notion of true and false.

That is, this expression ' "" || "0" || 2 ' will result to 2 because
Perl thinks "" and "0" are false.

=back

=head1 ATTRIBUTES

=head2 vars => {NAME => VAL, ...}

Store variables.

=cut

has vars  => (is => 'rw', default => sub { {} });

=head2 funcs => {NAME => CODEREF, ...}

List known functions.

=cut

has funcs => (is => 'rw', default => sub { {} });

=head2 level => INT

Current recursion level.

=cut

has level => (is => 'rw', default => 0);


=head1 METHODS

=for Pod::Coverage ^(rule|expr)_.+

=cut

sub rule_pair_simple {
    my ($self, %args) = @_;
    my $match = $args{match};
    [$match->{key}, $match->{value}];
}

sub rule_pair_string {
    my ($self, %args) = @_;
    my $match = $args{match};
    [$match->{key}, $match->{value}];
}

sub rule_or_xor {
    my ($self, %args) = @_;
    my $match = $args{match};
    my $res = shift @{$match->{operand}};
    for my $term (@{$match->{operand}}) {
        my $op = shift @{$match->{op}//=[]};
        last unless $op;
        if    ($op eq '||') { $res ||= $term          }
        elsif ($op eq '//') { $res //= $term          }
        elsif ($op eq '^^') { $res = ($res xor $term) }
    }
    $res;
}

sub rule_and {
    my ($self, %args) = @_;
    my $match = $args{match};
    my $res = shift @{$match->{operand}};
    for my $term (@{$match->{operand}}) {
        my $op = shift @{$match->{op}//=[]};
        last unless $op;
        if    ($op eq '&&') { $res = $res && $term || false }
    }
    $res;
}

sub rule_bit_or_xor {
    my ($self, %args) = @_;
    my $match = $args{match};
    my $res = shift @{$match->{operand}};
    for my $term (@{$match->{operand}}) {
        my $op = shift @{$match->{op}//=[]};
        last unless $op;
        if    ($op eq '|') { $res = $res+0 | $term }
        elsif ($op eq '^') { $res = $res+0 ^ $term }
    }
    $res;
}

sub rule_bit_and {
    my ($self, %args) = @_;
    my $match = $args{match};
    my $res = shift @{$match->{operand}};
    for my $term (@{$match->{operand}}) {
        my $op = shift @{$match->{op}//=[]};
        last unless $op;
        if    ($op eq '&') { $res = $res+0 & $term }
    }
    $res;
}

sub rule_comparison3 {
    my ($self, %args) = @_;
    my $match = $args{match};
    my $res = shift @{$match->{operand}};
    return $res unless @{$match->{operand}};
    my $last_term = $res;
    for my $term (@{$match->{operand}}) {
        my $op = shift @{$match->{op}//=[]};
        last unless $op;
        if    ($op eq '<=>') { $res = ($last_term <=> $term) }
        elsif ($op eq 'cmp') { $res = ($last_term cmp $term) }
        $last_term = $term;
    }
    $res;
}

sub rule_comparison {
    my ($self, %args) = @_;
    my $match = $args{match};
    my $res = shift @{$match->{operand}};
    return $res unless @{$match->{operand}};
    my $last_term = $res;
    for my $term (@{$match->{operand}}) {
        my $op = shift @{$match->{op}//=[]};
        last unless $op;
        if    ($op eq '==' ) { return false unless $res = ($last_term == $term ? true:false) }
        elsif ($op eq '!=' ) { return false unless $res = ($last_term != $term ? true:false) }
        elsif ($op eq 'eq' ) { return false unless $res = ($last_term eq $term ? true:false) }
        elsif ($op eq 'ne' ) { return false unless $res = ($last_term ne $term ? true:false) }
        elsif ($op eq '<'  ) { return false unless $res = ($last_term <  $term ? true:false) }
        elsif ($op eq '<=' ) { return false unless $res = ($last_term <= $term ? true:false) }
        elsif ($op eq '>'  ) { return false unless $res = ($last_term >  $term ? true:false) }
        elsif ($op eq '>=' ) { return false unless $res = ($last_term >= $term ? true:false) }
        elsif ($op eq 'lt' ) { return false unless $res = ($last_term lt $term ? true:false) }
        elsif ($op eq 'gt' ) { return false unless $res = ($last_term gt $term ? true:false) }
        elsif ($op eq 'le' ) { return false unless $res = ($last_term le $term ? true:false) }
        elsif ($op eq 'ge' ) { return false unless $res = ($last_term ge $term ? true:false) }
        $last_term = $term;
    }
    $res ? true : false;
}

sub rule_bit_shift {
    my ($self, %args) = @_;
    my $match = $args{match};
    my $res = shift @{$match->{operand}};
    for my $term (@{$match->{operand}}) {
        my $op = shift @{$match->{op}//=[]};
        last unless $op;
        if    ($op eq '>>') { $res >>= $term }
        elsif ($op eq '<<') { $res <<= $term }
    }
    $res;
}

sub rule_add {
    my ($self, %args) = @_;
    my $match = $args{match};
    my $res = shift @{$match->{operand}};
    for my $term (@{$match->{operand}}) {
        my $op = shift @{$match->{op}//=[]};
        last unless $op;
        if    ($op eq '+') { $res += $term }
        elsif ($op eq '-') { $res -= $term }
        elsif ($op eq '.') { $res .= $term }
    }
    $res;
}

sub rule_mult {
    my ($self, %args) = @_;
    my $match = $args{match};
    my $res = shift @{$match->{operand}};
    for my $term (@{$match->{operand}}) {
        my $op = shift @{$match->{op}//=[]};
        last unless $op;
        if    ($op eq '*') { $res *= $term }
        elsif ($op eq '/') { $res /= $term }
        elsif ($op eq '%') { $res %= $term }
        elsif ($op eq 'x') { $res x= $term }
    }
    $res;
}

sub rule_unary {
    my ($self, %args) = @_;
    my $match = $args{match};
    my $res = $match->{operand};
    if ($match->{op}) {
        for my $op (reverse @{$match->{op}}) {
            if    ($op eq '!') { $res = $res ? false : true }
            elsif ($op eq '-') { $res = -$res }
            elsif ($op eq '~') { $res = ~($res+0) }
        }
    }
    $res;
}

sub rule_power {
    my ($self, %args) = @_;
    my $match = $args{match};
    reduce { $b ** $a } reverse @{$match->{operand}};
}

sub rule_subscripting {
    my ($self, %args) = @_;
    my $match = $args{match};
    my $res = $match->{operand};
    for my $i (@{$match->{subscript}}) {
        if (ref($res) eq 'ARRAY'  ) { $res = $res->[$i] }
        elsif (ref($res) eq 'HASH') { $res = $res->{$i} }
        else { die "Invalid subscript on nonhash/nonarray" }
    }
    $res;
}

sub rule_array {
    my ($self, %args) = @_;
    my $match = $args{match};
    $match->{element};
}

sub rule_hash {
    my ($self, %args) = @_;
    my $match = $args{match};
    return { map { $_->[0] => $_->[1] } @{ $match->{pair} } }
}

sub rule_undef {
    my ($self, %args) = @_;
    my $match = $args{match};
    undef;
}

sub rule_squotestr {
    my ($self, %args) = @_;
    my $match = $args{match};
    join "", map {
        $_ eq "\\'" ? "'" :
        $_ eq "\\\\" ? "\\" :
        $_
    } @{ $match->{part} };
}

sub rule_dquotestr {
    my ($self, %args) = @_;
    my $match = $args{match};

    #return join(", ", map {"[$_]"} @{$match->{part}}); #DEBUG

    join "", map {
        $_ eq "\\'" ? "'" :
        $_ eq "\\\"" ? '"' :
        $_ eq "\\\\" ? "\\" :
        $_ eq "\\\$" ? '$' :
        $_ eq "\\t" ? "\t" :
        $_ eq "\\n" ? "\n" :
        $_ eq "\\f" ? "\f" :
        $_ eq "\\b" ? "\b" :
        $_ eq "\\a" ? "\a" :
        $_ eq "\\e" ? "\e" :
        $_ eq "\\e" ? "\e" :
        /^\\([0-7]{1,3})$/ ? chr(oct($1)) :
        /^\\x([0-9A-Fa-f]{1,2})$/ ? chr(hex($1)) :
        /^\\x\{([0-9A-Fa-f]{1,4})\}$/ ? chr(hex($1)) :
        /^\$(\w+)$/ ? $self->vars->{$1} :
        /^\$\((.+)\)$/ ? $self->vars->{$1} :
        $_ eq "\\" ? "" :
        $_
    } @{ $match->{part} };
}

sub rule_bool {
    my ($self, %args) = @_;
    my $match = $args{match};
    if ($match->{bool} eq 'true') { true } else { false }
}

sub rule_num {
    my ($self, %args) = @_;
    my $match = $args{match};
    if    ($match->{num} eq 'inf') { "Inf"+0 }
    elsif ($match->{num} eq 'nan') { "NaN"+0 }
    else                           { $match->{num}+0 }
}

sub rule_var {
    my ($self, %args) = @_;
    my $match = $args{match};
    $self->vars->{ $match->{var} };
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
    #print "DEBUG: _map_grep_usort: level=$self->{level}, expr=`$expr`, array=[".join(",", @$ary),"]\n";
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

sub rule_parenthesis {}

sub expr_preprocess {}

sub expr_postprocess {
    my ($self, %args) = @_;
    my $result = $args{result};
    $result;
}

=head1 BUGS/TODOS

Currently subexpression (map/grep/usort) doesn't work yet.

=cut

__PACKAGE__->meta->make_immutable;
no Any::Moose;
1;
