package Language::Expr;
# ABSTRACT: Simple minilanguage for use in expression

# VERSION
# DATE

=head1 SYNOPSIS

    use 5.010;
    use strict;
    use warnings;
    use Language::Expr;
    my $le = Language::Expr->new;

    # evaluate expressions
    say $le->eval('1 + 2*3 + [4, 5][-1]'); # 12
    say $le->eval(q("i" . " love " .
                    {lang=>"perl", food=>"rujak"}["lang"])); # "i love perl"

    # convert Expr to Perl (string Perl code)
    say $le->perl('1 ^^ 2'); # "(1 xor 2)"

    # convert Expr to JavaScript
    say $le->js('1 . 2'); # "'' + 1 + 2"

    # convert Expr to PHP
    say $le->php('"x" x 10'); # "str_repeat(('x'), 10)"

    # convert Expr to compiled Perl code
    my $sub = $le->compile('($_[0]**2 + $_[1]**2)**0.5');
    say $sub->(3, 4); # 5

    # use variables & functions in expression (interpreted mode)
    $le->interpreted(1);
    $le->var('a' => 3, 'b' => 4);
    $le->func(pyth => sub { ($_[0]**2 + $_[1]**2)**0.5 });
    say $le->eval('pyth($a, $b)'); # 5

    # use variables & functions in expression (compiled mode, by default the Perl
    # compiler translates variables and function call as-is and runs it in
    # Language::Expr::Compiler::Perl namespace, but you can customize this, see
    # below)
    $le->interpreted(0);
    package Language::Expr::Compiler::Perl;
    sub pyth { ($_[0]**2 + $_[1]**2)**0.5 }
    our $a = 3;
    our $b = 4;
    package main;
    say $le->perl('pyth($a, $b)'); # "pyth($a, $b)"
    say $le->eval('pyth($a, $b)'); # 5

    # tell compiler to use My namespace, translate 'func()' to 'My::func()' and
    # '$var' to '$My::var'
    package My;
    sub pyth { sprintf("%.03f", ($_[0]**2 + $_[1]**2)**0.5) }
    our $a = 3;
    our $b = 4;
    package main;
    $le->compiler->hook_var (sub { '$My::'.$_[0] });
    $le->compiler->hook_func(sub { 'My::'.(shift)."(".join(", ", @_).")" });
    say $le->perl('pyth($a, $b)'); # "My::pyth($My::a, $My::b)"
    say $le->eval('pyth($a, $b)'); # "5.000"

    # enumerate variables
    use Data::Dump;
    dd $le->enum_vars('$a*$a + sqr($b)'); # ['a', 'b']

=head1 DESCRIPTION

Language::Expr defines a simple, Perl-like expression minilanguage. It
supports mathematical and string operators, arrays, hashes, variables,
and functions. See L<Language::Expr::Manual::Syntax> for description
of the language syntax.

This distribution consists of the language parser
(L<Language::Expr::Parser>), some interpreters
(Language::Expr::Interpreter::*), and some compilers
(Language::Expr::Compiler::*).

=cut

use 5.010001;
use strict;
use warnings;

use Moo;


=head1 ATTRIBUTES (MANUAL)

=head2 interpreted => BOOL

Whether to use the interpreter. By default is 0 (use the compiler,
which means Language::Expr expression will be compiled to Perl code
first before executed).

Note: The compiler is used by default because the interpreter currently lacks
subexpression (map/grep/sort) support. But the compiler cannot by default
directly use variables and functions defined by var() and func(). This slight
inconvenience might be rectified in the future.

=cut

has interpreted => (
    is => 'rw',
    default => sub{0},
);

=head2 interpreter => OBJ

Store the Language::Expr::Interpreter::Default instance.

=cut

has interpreter => (
    is => 'ro',
    lazy => 1,
    default => sub {
        require Language::Expr::Interpreter::Default;
        Language::Expr::Interpreter::Default->new;
    },
);

=head2 compiler => OBJ

Store the Language::Expr::Compiler::Perl instance.

=cut

has compiler => (
    is => 'ro',
    lazy => 1,
    default => sub {
        require Language::Expr::Compiler::Perl;
        Language::Expr::Compiler::Perl->new;
    },
);

=head2 js_compiler => OBJ

Store the Language::Expr::Compiler::JS instance.

=cut

has js_compiler => (
    is => 'ro',
    lazy => 1,
    default => sub {
        require Language::Expr::Compiler::JS;
        Language::Expr::Compiler::JS->new;
    },
);

=head2 php_compiler => OBJ

Store the Language::Expr::Compiler::PHP instance.

=cut

has php_compiler => (
    is => 'ro',
    lazy => 1,
    default => sub {
        require Language::Expr::Compiler::PHP;
        Language::Expr::Compiler::PHP->new;
    },
);

=head2 varenumer => OBJ

Store the Language::Expr::Interpreter::VarEnumer instance.

=cut

has varenumer => (
    is => 'ro',
    lazy => 1,
    default => sub {
        require Language::Expr::Interpreter::VarEnumer;
        Language::Expr::Interpreter::VarEnumer->new;
    },
);


=head1 METHODS

=head2 new()

Construct a new Language::Expr object, which is just a convenient front-end of
the Expr parser, compilers, and interpreters. You can also use the
parser/compiler/interpreter independently.

=cut

=head2 var(NAME => VALUE, ...)

Define variables. Note that variables are only directly usable in interpreted
mode (see SYNOPSIS for example on how to use variables in compiled mode).

=cut

sub var {
    my ($self, %args) = @_;
    my $itp = $self->interpreter;
    $itp->vars->{$_} = $args{$_} for keys %args;
}

=head2 func(NAME => CODEREF, ...)

Define functions. Dies if function is defined multiple times. Note that
functions are only directly usable in interpreted mode (see SYNOPSIS for example
on how to use functions in compiled mode).

=cut

sub func {
    my ($self, %args) = @_;
    my $itp = $self->interpreter;
    for (keys %args) {
        die "Function `$_` already defined" if $itp->funcs->{$_};
        die "Function `$_`: coderef required" unless ref($args{$_}) eq 'CODE';
        $itp->funcs->{$_} = $args{$_};
    }
}

=head2 eval(STR) => RESULT

Evaluate expression in STR (either using the compiler or interpreter) and return
the result. Will die if there is a parsing or runtime error. By default it uses
the compiler unless you set C<interpreted> to 1.

Also see compile() which will always use the compiler regardless of
C<interpreted> setting, and will save compilation result into a Perl subroutine
(thus is more efficient if you need to evaluate an expression repeatedly).

=cut

sub eval {
    my ($self, $str) = @_;
    my $evaluator = $self->interpreted ? $self->interpreter : $self->compiler;
    $evaluator->eval($str);
}

=head2 perl(STR) => STR

Convert expression in STR and return a string Perl code. Dies on error.
Internally just call $le->compiler->perl().

=cut

sub perl {
    my ($self, $str) = @_;
    $self->compiler->perl($str);
}

=head2 js(STR) => STR

Convert expression in STR and return a string JavaScript code. Dies on error.
Internally just call $le->js_compiler->js().

=cut

sub js {
    my ($self, $str) = @_;
    unless ($self->js_compiler) {
        require Language::Expr::Compiler::JS;
        $self->js_compiler(Language::Expr::Compiler::JS->new);
    }
    $self->js_compiler->js($str);
}

=head2 php(STR) => STR

Convert expression in STR and return a string PHP code. Dies on error.
Internally just call $le->php_compiler->php().

=cut

sub php {
    my ($self, $str) = @_;
    unless ($self->php_compiler) {
        require Language::Expr::Compiler::PHP;
        $self->php_compiler(Language::Expr::Compiler::PHP->new);
    }
    $self->php_compiler->php($str);
}

=head2 compile(STR) => CODEREF

Compile expression in STR into Perl subroutine. Dies on error. See also eval().

Inside the expression, you can use '$_[0]', '$_[1]', etc to access the
subroutine's arguments, because compile() sets $_ to @_. Example:

 my $sub = $le->compile('($_[0]**2 + $_[1]**2)**0.5');
 say $sub->(3, 4); # 5

=cut

sub compile {
    my ($self, $str) = @_;
    my $s = $self->perl($str);
    # the '$_ = \@_' trick is to make '$_[0]' in expression work like in Perl
    my $sub = eval qq(sub { local \$_ = \\\@_; $s });
    die $@ if $@;
    $sub;
}

=head2 enum_vars(STR) => ARRAYREF

Enumerate variables mentioned in expression STR. Return empty arrayref
 if no variables are mentioned.

=cut

sub enum_vars {
    my ($self, $str) = @_;
    $self->varenumer->eval($str);
}


=head1 FAQ

=head2 Why yet another simplistic (restricted, etc) language? Why not just Perl?

When first adding expression support to L<Data::Schema> (now L<Data::Sah>), I
want a language that is simple enough so I can easily convert it to Perl, PHP,
JavaScript, and others. I do not need a fully-fledged programming language. In
fact, Expr is not even Turing-complete, it does not support assignment or loops.
Nor does it allow function definition (though it allows anonymous function in
grep/map/usort). Instead, I just need some basic stuffs like
mathematical/string/logical operators, arrays, hashes, functions, map/grep/usort.
This language will mostly be used inside templates and schemas.

=head2 Why don't you use Language::Farnsworth, or Math::Expression, or Math::Expression::Evaluator, or $FOO?

I need several compilers and interpreters (some even with different semantics),
so it's easier to start with a simple parser of my own. And of course there is
personal preference of language syntax.

=head2 What is the difference between a compiler and interpreter?

An interpreter evaluates expression as it is being parsed, while a compiler
generates a complete Perl (or whatever) code first. Thus, if you $le->eval()
repeatedly using the interpreter mode (setting $le->interpreted(1)), you will
repeatedly parse the expression each time. This can be one or more orders of
magnitude slower compared to compiling into Perl once and then directly
executing the Perl code repeatedly.

Note that if you use $le->eval() using the default compiler mode, you do not
reap the benefits of compilation because the expression will be compiled each
time you call $le->eval(). To save the compilation result, use $le->compile() or
$le->perl() and compile the Perl code yourself using Perl's eval().

=head2 I want different syntax for (variables, foo operator, etc)!

Create your own language :-) Fork this distribution and start
modifying the Language::Expr::Parser module.

=head2 How to show details of errors in expression?

This is a TODO item.


=head1 KNOWN BUGS

Due to possible bugs in Perl's RE engine or Regexp::Grammars or my
grammar, some syntax errors will cause further parsing to
fail.


=head1 SEE ALSO

Syntax reference: L<Language::Expr::Manual::Syntax>

Modules that are using Language::Expr: L<Data::Sah> (not yet released),
L<Data::Template::Expr> (not yet released).

Other related modules: L<Math::Expression>,
L<Math::Expression::Evaluator>, L<Language::Farnsworth>

=cut

1;
