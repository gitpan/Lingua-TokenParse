use 5.005;
use ExtUtils::MakeMaker;
WriteMakefile(
    'NAME'         => 'Lingua::TokenParse',
    'VERSION_FROM' => 'lib/Lingua/TokenParse.pm',
    'PREREQ_PM'    => {
        'Test::More' => 0,
    },
    ($] >= 5.005
        ? (ABSTRACT_FROM => 'lib/Lingua/TokenParse.pm',
           AUTHOR        => 'Gene Boggs <gene@cpan.org>')
        : ()
    ),
);
