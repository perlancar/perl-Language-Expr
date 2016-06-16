package Language::Expr;

# DATE
# VERSION

use 5.010001;
use strict;
use warnings;

sub new {
    my $class = shift;
    bless {}, $class;
}

sub get_compiler {
    my ($self, $name) = @_;
    my $mod = "Language::Expr::Compiler::$name";
    (my $mod_pm = "$mod.pm") =~ s!::!/!g;
    require $mod_pm;
    $mod->new();
}

sub get_interpreter {
    my ($self, $name) = @_;
    my $mod = "Language::Expr::Interpreter::$name";
    (my $mod_pm = "$mod.pm") =~ s!::!/!g;
    require $mod_pm;
    $mod->new();
}

1;
# ABSTRACT: Simple minilanguage for use in expression

=head1 SYNOPSIS

 use Language::Expr;

 my $le = Language::Expr->new;

 # convert Expr to string Perl code
 say $le->get_compiler('perl')->compile('1 ^^ 2'); => # "(1 xor 2)"

 # convert Expr to JavaScript
 say $le->get_compiler('js')->compile('1 . 2'); # => "'' + 1 + 2"

 # evaluate Expr using the default interpreter
 say $le->get_interpreter('default')->eval('1 + 2'); # => 3

 # use variables & functions in expression (interpreter mode)
 my $pli = $le->get_interpreter('default');
 $pli->vars->{a} = 3;
 $pli->vars->{b} = 4;
 $pli->functiovars->(pyth => sub { ($_[0]**2 + $_[1]**2)**0.5 });
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

Language::Expr defines a simple, Perl-like expression minilanguage. It supports
mathematical and string operators, arrays, hashes, variables, and functions. See
L<Language::Expr::Manual::Syntax> for description of the language syntax.

This distribution consists of the language parser (L<Language::Expr::Parser>),
some interpreters (Language::Expr::Interpreter::*), and some compilers
(Language::Expr::Compiler::*).


=head1 ATTRIBUTES


=head1 METHODS

=head2 new()

Construct a new Language::Expr object, which is just a convenient front-end of
the Expr parser, compilers, and interpreters. You can also use the
parser/compiler/interpreter independently.

=head2 var(NAME => val, ...)

Define variables. Note that variables are only directly usable in interpreted
mode (see L</"SYNOPSIS"> for example on how to use variables in compiled mode).

=head2 func(NAME => coderef, ...)

Define functions. Dies if function is defined multiple times. Note that
functions are only directly usable in interpreted mode (see L</"SYNOPSIS"> for
example on how to use functions in compiled mode).

=head2 eval($expr) => any

Evaluate expression in C<$expr> (either using the compiler or interpreter) and
return the result. Will die if there is a parsing or runtime error. By default
it uses the compiler unless you set C<interpreted> to 1.

Also see C<compile()> which will always use the compiler regardless of
C<interpreted> setting, and will save compilation result into a Perl subroutine
(thus is more efficient if you need to evaluate an expression repeatedly).

=head2 get_compiler($name) => obj

Get compiler named C<$name>, e.g. C<perl>, C<js>.

=head2 enum_vars($expr) => arrayref

Enumerate variables mentioned in expression C<$expr>. Return empty arrayref if
no variables are mentioned.


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
