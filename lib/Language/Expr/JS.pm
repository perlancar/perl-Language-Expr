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
    require IPC::System::Options;
    require JSON::MaybeXS;
    require Language::Expr::Compiler::js;
    require Nodejs::Util;

    my ($expr, $opts) = @_;
    $opts //= {};

    state $nodejs_path = Nodejs::Util::get_nodejs_path();
    die "Can't find node.js in PATH" unless $nodejs_path;

    state $default_jsc = Language::Expr::Compiler::js->new;

    state $json = JSON::MaybeXS->new->allow_nonref;

    my $jsc = $opts->{js_compiler} // $default_jsc;

    # code to be sent to nodejs
    my $src = join(
        "",
        _comment("expr: $expr\n"),
        "console.log(JSON.stringify(",
        $jsc->compile($expr),
        "))",
    );
    my ($jsh, $jsfn) = File::Temp::tempfile();
    print $jsh $src;
    close($jsh) or die "Can't write JS code to file $jsfn: $!";

    my $out = IPC::System::Options::readpipe($nodejs_path, $jsfn);
    $json->decode($out);
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
