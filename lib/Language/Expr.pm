package Language::Expr;

use 5.010001;
use strict;
use warnings;

use Moo;

# VERSION
# DATE

has perl_compiler => (
    is => 'ro',
    lazy => 1,
    default => sub {
        require Language::Expr::Compiler::Perl;
        Language::Expr::Compiler::Perl->new;
    },
);

has js_compiler => (
    is => 'ro',
    lazy => 1,
    default => sub {
        require Language::Expr::Compiler::JS;
        Language::Expr::Compiler::JS->new;
    },
);

has php_compiler => (
    is => 'ro',
    lazy => 1,
    default => sub {
        require Language::Expr::Compiler::PHP;
        Language::Expr::Compiler::PHP->new;
    },
);

has var_enumer => (
    is => 'ro',
    lazy => 1,
    default => sub {
        require Language::Expr::Interpreter::VarEnumer;
        Language::Expr::Interpreter::VarEnumer->new;
    },
);

sub eval {
    my ($self, $str) = @_;
    eval $self->perl($str);
}

sub perl {
    my ($self, $str) = @_;
    $self->perl_compiler->compile($str);
}

sub js {
    my ($self, $str) = @_;
    $self->js_compiler->compile($str);
}

sub php {
    my ($self, $str) = @_;
    $self->php_compiler->compile($str);
}

sub enum_vars {
    my ($self, $str) = @_;
    $self->var_enumer->eval($str);
}

1;
# ABSTRACT: Simple minilanguage for use in expression

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

 # use variables & functions in expression (by default the Perl compiler
 # translates variables and function call as-is and runs it in
 # Language::Expr::Compiled namespace, but you can customize this, see below)
 package Language::Expr::Compiled;
 sub pyth { ($_[0]**2 + $_[1]**2)**0.5 }
 our $a = 3;
 our $b = 4;
 package main;
 say $le->perl('pyth($a, $b)'); # "pyth($a, $b)"
 say $le->eval('pyth($a, $b)'); # 5

 # tell compiler to use My::Expr namespace, so 'func()' resolves to
 # 'My::Expr::func()' and '$var' to '$My::Expr::var'
 package My::Expr;
 sub pyth { sprintf("%.03f", ($_[0]**2 + $_[1]**2)**0.5) }
 our $a = 3;
 our $b = 4;
 package main;
 $le->perl_compiler->hook_var (sub { '$My::Expr::'.$_[0] });
 $le->perl_compiler->hook_func(sub { 'My::Expr::'.(shift)."(".join(", ", @_).")" });
 say $le->perl('pyth($a, $b)'); # "My::Expr::pyth($My::a, $My::b)"
 say $le->eval('pyth($a, $b)'); # "5.000"

 # enumerate variables
 use Data::Dump;
 dd $le->enum_vars('$a*$a + sqr($b)'); # ['a', 'b']


=head1 DESCRIPTION

Language::Expr defines a simple, Perl-like expression minilanguage. It supports
mathematical and string operators, arrays, hashes, variables, and functions. See
L<Language::Expr::Manual::Syntax> for description of the language syntax.

This distribution consists of the language parser (L<Language::Expr::Parser>),
some compilers (Language::Expr::Compiler::*) and interpreters
(Language::Expr::Interpreter::*).


=head1 ATTRIBUTES

=head2 perl_compiler => OBJ

Store the L<Language::Expr::Compiler::Perl> instance (instantiated lazily).

=head2 js_compiler => OBJ

Store the L<Language::Expr::Compiler::JS> instance (instantiated lazily).

=head2 php_compiler => OBJ

Store the L<Language::Expr::Compiler::PHP> instance (instantiated lazily).

=head2 var_enumer => OBJ

Store the L<Language::Expr::Interpreter::VarEnumer> instance (instantiated
lazily).


=head1 METHODS

=head2 new()

Construct a new Language::Expr object, which is just a convenient front-end of
the Expr parser, compilers, and interpreters. You can also use the
parser/compiler/interpreter independently.

=head2 perl($str) => STR

Convert expression in C<$str> and return a string Perl code. Dies on error. A
shortcut for C<< $le->perl_compiler->compile() >>.

=head2 js($str) => STR

Convert expression in C<$str> and return a string JavaScript code. Dies on
error. Internally just call C<< $le->js_compiler->compile() >>.

=head2 php($str) => STR

Convert expression in C<$str> and return a string PHP code. Dies on error.
Internally just call C<< $le->php_compiler->compile() >>.

=head2 enum_vars($str) => ARRAYREF

Enumerate variables mentioned in expression C<$str>. Dies on error. Return empty
arrayref if no variables are mentioned. Internally just call C<<
$le->var_enumer->compile() >>.


=head1 FAQ

=head2 Why yet another simplistic (restricted, etc) language? Why not just Perl?

When first adding expression support to L<Data::Schema> (now L<Data::Sah>), I
want a language that is simple enough so I can easily convert it to Perl, PHP,
JavaScript, and others. I do not need a fully-fledged programming language. In
fact, Expr is not even Turing-complete, it does not support assignment or loops.
Nor does it allow function definition (though it allows anonymous function in
grep/map/usort). Instead, I just need some basic stuffs like
mathematical/string/logical operators, arrays, hashes, functions,
map/grep/usort. This language will mostly be used inside templates and schemas.

=head2 Why don't you use Language::Farnsworth, or Math::Expression, or Math::Expression::Evaluator, or $FOO?

I need several compilers and interpreters (some even with different semantics),
so it's easier to start with a simple parser of my own. And of course there is
personal preference of language syntax.

=head2 I want different syntax for (variables, foo operator, etc)!

Create your own language :-) Fork this distribution and start modifying the
Language::Expr::Parser module.

=head2 How to get a better error message?

This is a TODO item.


=head1 SEE ALSO

Syntax reference: L<Language::Expr::Manual::Syntax>

Other related modules: L<Math::Expression>, L<Math::Expression::Evaluator>,
L<Language::Farnsworth>
