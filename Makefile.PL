use 5.005;
use ExtUtils::MakeMaker;
WriteMakefile(
    'NAME'         => 'Lingua::TokenParse',
    'VERSION_FROM' => 'TokenParse.pm',
    'PREREQ_PM'    => {},
    ($] >= 5.005
        ? (ABSTRACT_FROM => 'TokenParse.pm',
           AUTHOR        => 'Gene Boggs <gene@cpan.org>')
        : ()
    ),
);
