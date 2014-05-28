package Language::Expr::Parser;

use 5.010;
use strict;
use warnings;

require Exporter;
our @ISA = qw(Exporter);
our @EXPORT_OK = qw(parse_expr);

# VERSION
# DATE

sub parse_expr {
    my ($str, $pkg) = @_;
    state $slif = do {
        require Marpa::R2;
        my $slif = ''; # MARPA_SLIF_GRAMMAR(cf=grammar.bnf replace='')
        unless ($slif) {
            my $f;
            if (-f "share/grammar.bnf") {
                $f = "share/grammar.bnf";
            } else {
                require File::ShareDir;
                $f = File::ShareDir::dist_file(
                    'Language-Expr', 'grammar.bnf');
            }
            require File::Slurp::Tiny;
            my $bnf = File::Slurp::Tiny::read_file($f);
            $slif = Marpa::R2::Scanless::G->new({source=>\$bnf});
        }
    };

    my $recce = Marpa::R2::Scanless::R->new({
        grammar           => $slif,
        semantics_package => $pkg,
        trace_terminals   => $ENV{DEBUG} ? 1:0,
        trace_values      => $ENV{DEBUG} ? 1:0,
    });
    $recce->read(\$str);
    my $valref = $recce->value;
    if (!defined($valref)) {
        die "No parse was found after reading the entire input\n";
        # XXX show last expression
    }
    $$valref;
}

1;
# ABSTRACT: Parse Language::Expr expression

=head1 METHODS

=head2 parse_expr($str, $pkg)

Parse expression in C<$str>. Will call various C<rule_*()> subroutines in
package C<$pkg>.


=head1 KNOWN ISSUES

=over 4

=item * Ternary operator is not chainable yet.

=back

=cut
