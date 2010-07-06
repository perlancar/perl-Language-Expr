use boolean;
use Data::Rmap qw(:all);
use Clone qw/clone/;

sub prepare_testing {
}

sub end_testing {
}

sub convert_json_booleans {
    #my $arg = shift;

    # JSON::XS apparently needs cloning, otherwise we get
    # 'Modification of a read-only value attempted' when trying to
    # rebless its booleans

    my $arg = clone(shift);
    #use Data::Dump qw(dump); print "# BEFORE: ", dump($arg), "\n";
    rmap_all { $_ = $_ ? true : false if JSON::is_bool($_) } $arg;
    #print "# AFTER: ", dump($arg), "\n";
    $arg;
}

1;
