#!/usr/bin/perl

BEGIN {
    use strict;
    use warnings;
    use Test::More 'no_plan';
use lib 'lib';
    use_ok 'Lingua::TokenParse';
};

my %lexicon;
@lexicon{qw(part i tion on)} = qw(a b c d);

my $obj = Lingua::TokenParse->new(
#    verbose => 1,
);
isa_ok $obj, 'Lingua::TokenParse', 'With no arguments';

is scalar( @{ $obj->parts } ), 0, 'no parts';
is scalar( @{ $obj->combinations } ), 0, 'no combinations';
is scalar( keys %{ $obj->knowns } ), 0, 'no knowns';
is scalar( keys %{ $obj->definitions } ), 0, 'no definitions';

$obj = Lingua::TokenParse->new(
#    verbose => 1,
    word => 'partition',
    lexicon => \%lexicon,
    score => 0,
);
isa_ok $obj, 'Lingua::TokenParse', 'With arguments';

# Parse again but with a constraint.
my $rule = qr/\.on$/;
$obj->constraints( [ $rule ] );
$obj->parse;
ok not (grep { /$rule/ } @{ $obj->combinations }),
    'constrained combinations';
ok not (grep { /$rule/ } keys %{ $obj->knowns }),
    'constrained knowns';
