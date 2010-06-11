#!perl -T

# for testing stuffs that are not interpreted yet.

use strict;
use warnings;
use Test::More tests => 9*3;
use Test::Exception;
use Language::Expr::Parser;
use Language::Expr::DummyItp;
use POSIX;

my @data = (
    # map
    {category=>'map', text=>'map {}, []', error=>qr/invalid syntax/i}, # lack parenthesis
    {category=>'map', text=>'map({1<}, [])', error=>qr/invalid syntax/i}, # invalid subexpression

    {category=>'map', text=>'map()'}, # lack arguments. won't be parsed as map(), but ok
    {category=>'map', text=>'map({}, [])'}, # empty subexpression. won't be parsed as map(), but ok
    {category=>'map', text=>'map(1, [])'}, # not subexpression. won't be parsed as map(), but ok

    {category=>'map', text=>'map({$_*2}, {})'}, # although doesn't make sense, parses
    {category=>'map', text=>'map({$_*2}, [])'},
    {category=>'map', text=>'map({$_*2}, [1,2,3])'},
    {category=>'map', text=>'map({ map({$_[0]}, [$_]) }, [1,2,3])'}, # nested map

    # grep
    {category=>'grep', text=>'grep {}, []', error=>qr/invalid syntax/i}, # lack parenthesis
    {category=>'grep', text=>'grep({1<}, [])', error=>qr/invalid syntax/i}, # invalid subexpression

    {category=>'grep', text=>'grep()'}, # lack arguments. won't be parsed as grep(), but ok
    {category=>'grep', text=>'grep({}, [])'}, # empty subexpression. won't be parsed as grep(), but ok
    {category=>'grep', text=>'grep(1, [])'}, # not subexpression. won't be parsed as grep(), but ok

    {category=>'grep', text=>'grep({$_>1}, {})'}, # although doesn't make sense, parses
    {category=>'grep', text=>'grep({$_>1}, [])'},
    {category=>'grep', text=>'grep({$_>1}, [1,2,3])'},
    {category=>'grep', text=>'grep({ grep({$_[0] > 1}, [$_])[0] }, [1,2,3])'}, # nested grep

    # usort
    {category=>'usort', text=>'usort {}, []', error=>qr/invalid syntax/i}, # lack parenthesis
    {category=>'usort', text=>'usort({1<}, [])', error=>qr/invalid syntax/i}, # invalid subexpression

    {category=>'usort', text=>'usort()'}, # lack arguments. won't be parsed as usort(), but ok
    {category=>'usort', text=>'usort({}, [])'}, # empty subexpression. won't be parsed as usort(), but ok
    {category=>'usort', text=>'usort(1, [])'}, # not subexpression. won't be parsed as usort(), but ok

    {category=>'usort', text=>'usort({uc($a) cmp uc($b)}, {})'}, # although doesn't make sense, parses
    {category=>'usort', text=>'usort({uc($a) cmp uc($b)}, [])'},
    {category=>'usort', text=>'usort({uc($a) cmp uc($b)}, [1,2,3])'},
    {category=>'usort', text=>'usort({ usort({rand()}, [$_, $_+1, $_+2]) }, [1,2,3])'}, # nested usort
);

my $itp = new Language::Expr::DummyItp;

for (@data) {
    if ($_->{error}) {
        throws_ok { Language::Expr::Parser::parse_expr($_->{text}, $itp) } $_->{error},
            "$_->{category} ($_->{text}) (error: $_->{error})";
    } else {
        lives_ok { Language::Expr::Parser::parse_expr($_->{text}, $itp) }
            "$_->{category} ($_->{text}) (parses)";
    }
}

