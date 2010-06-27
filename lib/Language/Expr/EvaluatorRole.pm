package Language::Expr::EvaluatorRole;
# ABSTRACT: Specification for Language::Expr interpreter/compiler

use Any::Moose '::Role';

requires 'rule_pair_simple';
requires 'rule_pair_string';
requires 'rule_or_xor';
requires 'rule_and';
requires 'rule_bit_or_xor';
requires 'rule_bit_and';
requires 'rule_comparison3';
requires 'rule_comparison';
requires 'rule_bit_shift';
requires 'rule_add';
requires 'rule_mult';
requires 'rule_unary';
requires 'rule_power';
requires 'rule_subscripting';
requires 'rule_array';
requires 'rule_hash';
requires 'rule_undef';
requires 'rule_squotestr';
requires 'rule_dquotestr';
requires 'rule_var';
requires 'rule_func';
requires 'rule_func_map';
requires 'rule_func_grep';
requires 'rule_func_usort';
requires 'rule_bool';
requires 'rule_num';
requires 'rule_parenthesis';
requires 'expr_preprocess';
requires 'expr_postprocess';

no Any::Moose;
1;
