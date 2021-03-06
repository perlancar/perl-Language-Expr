package Language::Expr::JS;

# DATE
# VERSION

use 5.010;
use strict;
use warnings;
#use Log::Any qw($log);

require Exporter;
our @ISA       = qw(Exporter);
our @EXPORT_OK = qw(eval_expr_js);

sub _comment {
    my $str = shift;
    $str =~ s!^!// !g;
    $str;
}

sub eval_expr_js {
    require File::Temp;
    require JSON::MaybeXS;
    require Language::Expr::Compiler::js;
    require Nodejs::Util;

    my ($expr, $opts) = @_;
    $opts //= {};

    state $default_jsc = Language::Expr::Compiler::js->new;

    state $json = JSON::MaybeXS->new->allow_nonref;

    my $jsc = $opts->{js_compiler} // $default_jsc;

    # code to be sent to nodejs
    my $src = join(
        "",
        _comment("expr: $expr\n"),
        ($opts->{vars} ?
             _comment("declare vars\n") . join("", map { "let $_ = ".$json->encode($opts->{vars}{$_}).";\n" } sort keys %{$opts->{vars}})
             : ""),
        "console.log(JSON.stringify(",
        $jsc->compile($expr),
        "))",
    );
    my ($jsh, $jsfn) = File::Temp::tempfile();
    print $jsh $src;
    close($jsh) or die "Can't write JS code to file $jsfn: $!";

    my ($stdout, $stderr);
    Nodejs::Util::system_nodejs(
        {
            harmony_scoping => 1,
            capture_stdout => \$stdout,
            capture_stderr => \$stderr
        },
        $jsfn,
    );
    die "nodejs exists non-zero (".($? >> 8)."): $stderr" if $?;
    if ($stdout eq "undefined\n") {
        return undef;
    }
    $json->decode($stdout);
}

1;
# ABSTRACT: Evaluate Expr JavaScript code

=head1 SYNOPSIS

 use Language::Expr::JS qw(eval_expr_js);

 say eval_expr_js('"a" . "b"'); # "ab"


=head1 DESCRIPTION


=head1 FUNCTIONS

None exported by default.

=head2 eval_expr_js($expr) => str

Compile $expr to JavaScript code, then run the JavaScript code using Node.js,
and return the result.

=cut
