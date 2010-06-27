use boolean;
use Data::Walk;
use Clone qw/clone/;

sub convert_json_booleans {
    #my $arg = shift;

    # JSON::XS apparently needs cloning, otherwise we get
    # 'Modification of a read-only value attempted' when trying to
    # rebless its booleans

    my $arg = clone(shift);
    #use Data::Dump qw(dump); print "# BEFORE: ", dump($arg), "\n";
    walk sub {
        if (ref($_) eq 'JSON::PP::Boolean' || ref($_) eq 'JSON::XS::Boolean') {
            bless $_, 'boolean';
        }
        return;
    }, $arg;
    #print "# AFTER: ", dump($arg), "\n";
    $arg;
}

1;
