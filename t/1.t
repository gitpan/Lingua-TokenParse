use strict;
use Test::More tests => 8;

BEGIN { use_ok('Lingua::TokenParse') };

# Testing variables {{{
my $parts = [  # {{{
    [
        'p',
        'pa',
        'par',
        'part',
        'parti',
        'partit',
        'partiti',
        'partitio',
        'partition',
    ],
    [
        'a',
        'ar',
        'art',
        'arti',
        'artit',
        'artiti',
        'artitio',
        'artition',
    ],
    [
        'r',
        'rt',
        'rti',
        'rtit',
        'rtiti',
        'rtitio',
        'rtition',
    ],
    [
        't',
        'ti',
        'tit',
        'titi',
        'titio',
        'tition',
    ],
    [
        'i',
        'it',
        'iti',
        'itio',
        'ition',
    ],
    [
        't',
        'ti',
        'tio',
        'tion',
    ],
    [
        'i',
        'io',
        'ion',
    ],
    [
        'o',
        'on',
    ],
    [
        'n',
    ],
];  # }}}

my $combinations = [  # {{{
    'p.a.r.t.i.t.i.o.n',
    'p.a.r.t.i.t.i.on',
    'p.a.r.t.i.t.io.n',
    'p.a.r.t.i.t.ion',
    'p.a.r.t.i.ti.o.n',
    'p.a.r.t.i.ti.on',
    'p.a.r.t.i.tio.n',
    'p.a.r.t.i.tion',
    'p.a.r.t.it.i.o.n',
    'p.a.r.t.it.i.on',
    'p.a.r.t.it.io.n',
    'p.a.r.t.it.ion',
    'p.a.r.t.iti.o.n',
    'p.a.r.t.iti.on',
    'p.a.r.t.itio.n',
    'p.a.r.t.ition',
    'p.a.r.ti.t.i.o.n',
    'p.a.r.ti.t.i.on',
    'p.a.r.ti.t.io.n',
    'p.a.r.ti.t.ion',
    'p.a.r.ti.ti.o.n',
    'p.a.r.ti.ti.on',
    'p.a.r.ti.tio.n',
    'p.a.r.ti.tion',
    'p.a.r.tit.i.o.n',
    'p.a.r.tit.i.on',
    'p.a.r.tit.io.n',
    'p.a.r.tit.ion',
    'p.a.r.titi.o.n',
    'p.a.r.titi.on',
    'p.a.r.titio.n',
    'p.a.r.tition',
    'p.a.rt.i.t.i.o.n',
    'p.a.rt.i.t.i.on',
    'p.a.rt.i.t.io.n',
    'p.a.rt.i.t.ion',
    'p.a.rt.i.ti.o.n',
    'p.a.rt.i.ti.on',
    'p.a.rt.i.tio.n',
    'p.a.rt.i.tion',
    'p.a.rt.it.i.o.n',
    'p.a.rt.it.i.on',
    'p.a.rt.it.io.n',
    'p.a.rt.it.ion',
    'p.a.rt.iti.o.n',
    'p.a.rt.iti.on',
    'p.a.rt.itio.n',
    'p.a.rt.ition',
    'p.a.rti.t.i.o.n',
    'p.a.rti.t.i.on',
    'p.a.rti.t.io.n',
    'p.a.rti.t.ion',
    'p.a.rti.ti.o.n',
    'p.a.rti.ti.on',
    'p.a.rti.tio.n',
    'p.a.rti.tion',
    'p.a.rtit.i.o.n',
    'p.a.rtit.i.on',
    'p.a.rtit.io.n',
    'p.a.rtit.ion',
    'p.a.rtiti.o.n',
    'p.a.rtiti.on',
    'p.a.rtitio.n',
    'p.a.rtition',
    'p.ar.t.i.t.i.o.n',
    'p.ar.t.i.t.i.on',
    'p.ar.t.i.t.io.n',
    'p.ar.t.i.t.ion',
    'p.ar.t.i.ti.o.n',
    'p.ar.t.i.ti.on',
    'p.ar.t.i.tio.n',
    'p.ar.t.i.tion',
    'p.ar.t.it.i.o.n',
    'p.ar.t.it.i.on',
    'p.ar.t.it.io.n',
    'p.ar.t.it.ion',
    'p.ar.t.iti.o.n',
    'p.ar.t.iti.on',
    'p.ar.t.itio.n',
    'p.ar.t.ition',
    'p.ar.ti.t.i.o.n',
    'p.ar.ti.t.i.on',
    'p.ar.ti.t.io.n',
    'p.ar.ti.t.ion',
    'p.ar.ti.ti.o.n',
    'p.ar.ti.ti.on',
    'p.ar.ti.tio.n',
    'p.ar.ti.tion',
    'p.ar.tit.i.o.n',
    'p.ar.tit.i.on',
    'p.ar.tit.io.n',
    'p.ar.tit.ion',
    'p.ar.titi.o.n',
    'p.ar.titi.on',
    'p.ar.titio.n',
    'p.ar.tition',
    'p.art.i.t.i.o.n',
    'p.art.i.t.i.on',
    'p.art.i.t.io.n',
    'p.art.i.t.ion',
    'p.art.i.ti.o.n',
    'p.art.i.ti.on',
    'p.art.i.tio.n',
    'p.art.i.tion',
    'p.art.it.i.o.n',
    'p.art.it.i.on',
    'p.art.it.io.n',
    'p.art.it.ion',
    'p.art.iti.o.n',
    'p.art.iti.on',
    'p.art.itio.n',
    'p.art.ition',
    'p.arti.t.i.o.n',
    'p.arti.t.i.on',
    'p.arti.t.io.n',
    'p.arti.t.ion',
    'p.arti.ti.o.n',
    'p.arti.ti.on',
    'p.arti.tio.n',
    'p.arti.tion',
    'p.artit.i.o.n',
    'p.artit.i.on',
    'p.artit.io.n',
    'p.artit.ion',
    'p.artiti.o.n',
    'p.artiti.on',
    'p.artitio.n',
    'p.artition',
    'pa.r.t.i.t.i.o.n',
    'pa.r.t.i.t.i.on',
    'pa.r.t.i.t.io.n',
    'pa.r.t.i.t.ion',
    'pa.r.t.i.ti.o.n',
    'pa.r.t.i.ti.on',
    'pa.r.t.i.tio.n',
    'pa.r.t.i.tion',
    'pa.r.t.it.i.o.n',
    'pa.r.t.it.i.on',
    'pa.r.t.it.io.n',
    'pa.r.t.it.ion',
    'pa.r.t.iti.o.n',
    'pa.r.t.iti.on',
    'pa.r.t.itio.n',
    'pa.r.t.ition',
    'pa.r.ti.t.i.o.n',
    'pa.r.ti.t.i.on',
    'pa.r.ti.t.io.n',
    'pa.r.ti.t.ion',
    'pa.r.ti.ti.o.n',
    'pa.r.ti.ti.on',
    'pa.r.ti.tio.n',
    'pa.r.ti.tion',
    'pa.r.tit.i.o.n',
    'pa.r.tit.i.on',
    'pa.r.tit.io.n',
    'pa.r.tit.ion',
    'pa.r.titi.o.n',
    'pa.r.titi.on',
    'pa.r.titio.n',
    'pa.r.tition',
    'pa.rt.i.t.i.o.n',
    'pa.rt.i.t.i.on',
    'pa.rt.i.t.io.n',
    'pa.rt.i.t.ion',
    'pa.rt.i.ti.o.n',
    'pa.rt.i.ti.on',
    'pa.rt.i.tio.n',
    'pa.rt.i.tion',
    'pa.rt.it.i.o.n',
    'pa.rt.it.i.on',
    'pa.rt.it.io.n',
    'pa.rt.it.ion',
    'pa.rt.iti.o.n',
    'pa.rt.iti.on',
    'pa.rt.itio.n',
    'pa.rt.ition',
    'pa.rti.t.i.o.n',
    'pa.rti.t.i.on',
    'pa.rti.t.io.n',
    'pa.rti.t.ion',
    'pa.rti.ti.o.n',
    'pa.rti.ti.on',
    'pa.rti.tio.n',
    'pa.rti.tion',
    'pa.rtit.i.o.n',
    'pa.rtit.i.on',
    'pa.rtit.io.n',
    'pa.rtit.ion',
    'pa.rtiti.o.n',
    'pa.rtiti.on',
    'pa.rtitio.n',
    'pa.rtition',
    'par.t.i.t.i.o.n',
    'par.t.i.t.i.on',
    'par.t.i.t.io.n',
    'par.t.i.t.ion',
    'par.t.i.ti.o.n',
    'par.t.i.ti.on',
    'par.t.i.tio.n',
    'par.t.i.tion',
    'par.t.it.i.o.n',
    'par.t.it.i.on',
    'par.t.it.io.n',
    'par.t.it.ion',
    'par.t.iti.o.n',
    'par.t.iti.on',
    'par.t.itio.n',
    'par.t.ition',
    'par.ti.t.i.o.n',
    'par.ti.t.i.on',
    'par.ti.t.io.n',
    'par.ti.t.ion',
    'par.ti.ti.o.n',
    'par.ti.ti.on',
    'par.ti.tio.n',
    'par.ti.tion',
    'par.tit.i.o.n',
    'par.tit.i.on',
    'par.tit.io.n',
    'par.tit.ion',
    'par.titi.o.n',
    'par.titi.on',
    'par.titio.n',
    'par.tition',
    'part.i.t.i.o.n',
    'part.i.t.i.on',
    'part.i.t.io.n',
    'part.i.t.ion',
    'part.i.ti.o.n',
    'part.i.ti.on',
    'part.i.tio.n',
    'part.i.tion',
    'part.it.i.o.n',
    'part.it.i.on',
    'part.it.io.n',
    'part.it.ion',
    'part.iti.o.n',
    'part.iti.on',
    'part.itio.n',
    'part.ition',
    'parti.t.i.o.n',
    'parti.t.i.on',
    'parti.t.io.n',
    'parti.t.ion',
    'parti.ti.o.n',
    'parti.ti.on',
    'parti.tio.n',
    'parti.tion',
    'partit.i.o.n',
    'partit.i.on',
    'partit.io.n',
    'partit.ion',
    'partiti.o.n',
    'partiti.on',
    'partitio.n',
    'partition',
];  # }}}

my $knowns = {  # {{{
    'part.i.tion' => [1, 1],
    'part.i.t.i.on' => [0.8, 0.888888888888889]
};  # }}}

my $definitions = {  # {{{
    'a' => undef,
    'ar' => undef,
    'art' => undef,
    'arti' => undef,
    'artit' => undef,
    'artiti' => undef,
    'artitio' => undef,
    'artition' => undef,
    'i' => 'b',
    'io' => undef,
    'ion' => undef,
    'it' => undef,
    'iti' => undef,
    'itio' => undef,
    'ition' => undef,
    'n' => undef,
    'o' => undef,
    'on' => 'd',
    'p' => undef,
    'pa' => undef,
    'par' => undef,
    'part' => 'a',
    'parti' => undef,
    'partit' => undef,
    'partiti' => undef,
    'partitio' => undef,
    'partition' => undef,
    'r' => undef,
    'rt' => undef,
    'rti' => undef,
    'rtit' => undef,
    'rtiti' => undef,
    'rtitio' => undef,
    'rtition' => undef,
    't' => undef,
    'ti' => undef,
    'tio' => undef,
    'tion' => 'c',
    'tit' => undef,
    'titi' => undef,
    'titio' => undef,
    'tition' => undef,
};  # }}}

my $array_out = [  # {{{
    'part.i.tion [1.00, 1.00]
a + b + c',
    'part.i.t.i.on [0.80, 0.89]
a + b + ? + b + d'
];  # }}}

my $scalar_out =  # {{{
'Combination [fragment familiarity, character familiarity]
Fragment definitions (with the fragment separator, a period for known
but not defined, and a question mark for unknowns).

part.i.tion [1.00, 1.00]
a + b + c

part.i.t.i.on [0.80, 0.89]
a + b + ? + b + d';
# }}}
# }}}

my %lexicon;
@lexicon{qw(part i tion on)} = qw(a b c d);

my $obj = Lingua::TokenParse->new(
    word => 'partition',
    lexicon => \%lexicon,
    score => 0,
);

is_deeply $obj->parts, $parts, 'word partitions';
is_deeply $obj->combinations, $combinations, 'all combinations';
is_deeply $obj->knowns, $knowns, 'known combinations';
is_deeply $obj->definitions, $definitions, 'fragment definitions';
is_deeply [ $obj->output_knowns ], $array_out,
    'scored and defined combinations in array context';
is scalar $obj->output_knowns, $scalar_out,
    'scored and defined combinations in scalar context';

$obj->rules([ qr/\.on$/ ]);
$obj->parse;
is_deeply $obj->knowns, { 'part.i.tion' => [1, 1] },
    'trimming regexp';
