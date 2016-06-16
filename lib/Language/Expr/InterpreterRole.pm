package Language::Expr::InterpreterRole;

# DATE
# VERSION

use 5.010;
use strict;
use warnings;

use Role::Tiny;
use Role::Tiny::With;

with 'Language::Expr::EvaluatorRole';
requires 'eval';

1;
# ABSTRACT: Role for Language::Expr::Interpreter::*
