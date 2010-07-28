package Language::Expr::Compiler::Base;
# ABSTRACT: Base class for Expr compilers

use 5.010;
use Any::Moose;
extends 'Language::Expr::Evaluator';

use UUID::Tiny ':std';
use Language::Expr::Interpreter::Default;

=head1 ATTRIBUTES

=head2 markers => ARRAYREF

Used to mark compile output string with various unique strings, and
later on revisit these markers and substitute for other, final
values. This technique is kind of a hack, used for subexpression,
inserting PHP use() statement (because they must be processed outward
to inward), etc.

=cut

# [[type, uuid, data], ...]
has markers => (is => 'rw', default => sub { [] });

=head2 func_mapping => HASHREF

Map Expr function to target language's function/method/property.

=cut

has func_mapping => (is => 'rw', default => sub { {} });

=head1 METHODS

=head2 new_marker(TYPE[, DATA]) => UUID

Create a new marker. Return a unique ID to be placed in compiled
output.

=cut

sub new_marker {
    my ($self, $type, $data) = @_;
    my $uuid = UUID::Tiny::create_uuid_as_string(UUID_V4);
    #my $uuid = int(9000*rand()+1000);
    #print "DEBUG: Creating new marker: type=$type, uuid=$uuid, data=", ($data // "undef"), "\n\n";
    push @{ $self->markers }, [$type, $uuid, $data];
    $uuid;
}

=head2 marker_ids() => ARRAY

Return an array of all marker IDs.

=cut

sub marker_ids {
    my ($self) = @_;
    map {$_->[1]} @{ $self->markers };
}

=head2 marker_ids_re() => STRING

Return a regex that matches marker IDs.

=cut

sub marker_ids_re {
    my ($self) = @_;
    my $re = "(?:" . join("|", map {$_->[1]} @{ $self->markers }) . ")";
    qr/$re/;
}

__PACKAGE__->meta->make_immutable;
no Any::Moose;
1;