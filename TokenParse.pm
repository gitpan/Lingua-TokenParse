package Lingua::TokenParse;

use strict;
use vars qw($VERSION);
$VERSION = '0.04';

# NOTE: The {{{ and }}} things are "editor code fold markers".  They
# are merely a convenience for people who don't care to scroll through
# reams of source, like me.

# Globals used by the build_combinations method.
my (@parsed, @new, $prev);

sub new {  # {{{
    my ($class, %args) = @_;

    my $self = {
        # The word to parse!
        word         => $args{word} || undef,
        # We need to use the length of our word in a few methods.
        word_length  => exists $args{word} ? length ($args{word}) : undef,
        # The list of known tokens.
        lexicon      => $args{lexicon} || {},
        # The list of all word parts.
        parts        => [],
        # The list of all possible parts combinations.
        combinations => [],
        # The scored list of the known parts combinations.
        knowns       => {},
        # The list of definitions of the known and unknown fragments
        # in knowns.
        definitions  => {},
    };

    bless $self, $class;

    if ($args{word} and $args{lexicon}) {
        $self->build_parts;
        $self->build_combinations;
        $self->build_knowns;
        $self->build_definitions;
    }

    return $self;
}  # }}}

# Accessors {{{
sub word {  # {{{
    my $self = shift;
    if (@_) {
        $self->{word} = shift;
        $self->{word_length} = length $self->{word};
    }
    return $self->{word};
}  # }}}

sub lexicon {  # {{{
    my $self = shift;
    $self->{lexicon} = shift if @_;
    return $self->{lexicon};
}  # }}}

sub parts {  # {{{
    my $self = shift;
    $self->{parts} = shift if @_;
    return $self->{parts};
}  # }}}

sub combinations {  # {{{
    my $self = shift;
    $self->{combinations} = shift if @_;
    return $self->{combinations};
}  # }}}

sub knowns {  # {{{
    my $self = shift;
    $self->{knowns} = shift if @_;
    return $self->{knowns};
}  # }}}

sub definitions {  # {{{
    my $self = shift;
    $self->{definitions} = shift if @_;
    return $self->{definitions};
}  # }}}
# }}}

sub reset_parse {  # {{{
    my $self = shift;
    $self->parts([]);
    $self->combinations([]);
    $self->knowns({});
    $self->definitions({});
    @parsed = ();
    @new    = ();
}  # }}}

sub build_parts {  # {{{
    my $self = shift;

    my $len = length $self->word;

    for my $i (0 .. $len - 1) {
        for my $j (1 .. $len - $i) {
            push @{ $self->parts->[$i] }, substr $self->word, $i, $j;
        }
    }
}  # }}}

sub build_combinations {  # {{{
    my ($self, $i) = @_;
    $i = 0 unless defined $i;
    $prev = 0 unless defined $prev;

    for (@{ $self->parts->[$i] }) {
        # Find the end-position of the stem.
        my $n = $i + length;

        # XXX Ugly mystery-hack:
        # Yank-off the last two stems found, if we are at an "overlap point".
        splice @new, -2 if $prev > $i;

#print "$_ - i: $i, n: $n, prev: $prev, new: ". @new ."\n";

        $prev = $i;

        splice @new, @new, $n, $_;

        push @{ $self->combinations }, join '.', @new if $n == length ($self->word);

        $self->build_combinations($n);
    }
}  # }}}

sub build_knowns {  # {{{
    my $self = shift;

    # Show familiar combinations for each "raw" combination.
    for my $combo (@{ $self->combinations }) {
        my $sum = 0;

        # Get the bits of the combination.
        my @chunks = split /\./, $combo;

        for (@chunks) {
            # Handle hyphens in lexicon entries.
            my $flag = 0;
            if (exists $self->lexicon->{$_}) {
                $flag++;
            }
            elsif (exists $self->lexicon->{"$_-"}) {
                $flag++;
                $_ = "$_-";
            }
            elsif (exists $self->lexicon->{"-$_"}) {
                $flag++;
                $_ = "-$_";
            }

            # Sum the combination familiarity value.
            $sum++ if $flag;
        }

        $combo = join '.', @chunks;

        # Save this combination with the familiarity ratio as the
        # value.
        $self->knowns->{$combo} = $sum / @chunks
            if $sum and !exists $self->knowns->{$combo};
    }
}  # }}}

sub build_definitions {  # {{{
    my $self = shift;

    # Make a familiar combination from each "raw" combination.
    for my $combo (keys %{ $self->knowns }) {
        # Get the bits of the combination.
        my @chunks = split /\./, $combo;

        # Save entries with their definitions as the values.
        for (@chunks) {
            # Handle hyphens in lexicon entries.
            if (exists $self->lexicon->{$_}) {
                $self->definitions->{$_} = $self->lexicon->{$_};
            }
            elsif (exists $self->lexicon->{"$_-"}) {
                $self->definitions->{$_} = $self->lexicon->{"$_-"};
            }
            elsif (exists $self->lexicon->{"-$_"}) {
                $self->definitions->{$_} = $self->lexicon->{"-$_"};
            }
            else {
                $self->definitions->{$_} = undef;
            }
        }
    }
}  # }}}

sub output_knowns {  # {{{
    my $self = shift;

    for (reverse
         sort { $self->knowns->{$a} <=> $self->knowns->{$b} }
         keys %{ $self->knowns }
    ) {
        printf "%s: %0.2f\n", $_, $self->knowns->{$_};
    }
}  # }}}

1;
__END__

=head1 NAME

Lingua::TokenParse - Parse a word into scored, fragment combinations

=head1 SYNOPSIS

  use Lingua::TokenParse;

  my $word = 'partition';
  my %lexicon;
  @lexicon{qw(part i tion on)} = ();

  my $obj = Lingua::TokenParse->new(
      word => $word,
      lexicon => \%lexicon,
  );

  $obj->output_knowns;

  # Okay.  Now, let's parse a new word.
  $obj->reset_parse;

  $obj->word('metaphysical');

  $obj->lexicon({
      'meta-' => 'more comprehensive',
      'phys'  => 'natural science, singular',
      '-ic'   => 'being, containing',
      '-al'   => 'relating to, characterized by',
  });

  $obj->build_parts;
  $obj->build_combinations;
  $obj->build_knowns;
  $obj->build_definitions;

  $obj->output_knowns;

  print "@$_\n" for @{ $obj->parts };  # This has nice looking output.
  use Data::Dumper;
  print Dumper $obj->parts;
  print Dumper $obj->combinations;
  print Dumper $obj->knowns;
  print Dumper $obj->definitions;

=head1 ABSTRACT

This class represents a Lingua::TokenParse object and contains 
methods to parse a given word into familiar combinations based on a 
lexicon of known word parts.

=head1 DESCRIPTION

A word like "partition" is actually composed of a few different word
parts.  Given a lexicon of known fragments, it is possible to 
partition this word into combinations of these (possibly overlapping)
parts.  Each of these combinations can be given a score, which 
represents a measure of familiarity.

Currently, this familiarity mesasure is a simple ratio of known to 
unknown parts.

* Please check out the sample code in the distribution's eg/ 
directory for exciting examples of how this module can be used.

=head1 METHODS

=head2 new()

  $obj = Lingua::TokenParse->new(
      word    => $word,
      lexicon => \%lexicon,
  );

Return a new Lingua::TokenParse object.

This method will automatically call the partition methods (detailed 
below) if a word and lexicon are provided.

=head2 reset_parse()

Reset the lists used to parse a word into fragment combinations.

That method must be called prior to reparsing a new word in the same 
session.

=head2 build_parts()

  $obj->build_parts();

Construct an array of the word partitions, accessed via the parts() 
method.

=head2 build_combinations()

  $obj->build_combinations();

Recursively compute the array of all possible word part combinations,
accessed via the combinations() method.

=head2 build_knowns()

  $obj->build_knowns();

Compute the familiar word part combinations, accessed via the knowns()
method.

This method handles word parts containing prefix and suffix hyphens,
which are found in the dict.org sever (with web1913 database).  These
hyphens actually "encode" information about what is a syntactically 
legal word combination.  Which can be used to score (or just throw 
out bogus combinations).

=head2 build_definitions()

  $obj->build_definitions();

Construct a hash of the definitions of the word parts in each 
combination in the keys of the knowns hash.

=head2 output_knowns()

  $obj->output_knowns();

Convenience method to output the familiar word part combinations with
their familiarity scores rounded to two decimals.

=head1 ACCESSORS

These accessors both get and set their respective values.  Note 
that, if you set any of these after construction, you must manually 
run the partition methods.  Also, note that it is pretty useless to
set the parts, combinations and knowns lists, as they are computed 
by the partition methods.

=head2 word()

  $word = $obj->word($word);

The actual word to partition.

=head2 lexicon()

  $lexicon = $obj->lexicon($lexicon);

The hash reference of word parts (keys) with their (optional) 
definitions (values).

=head2 parts()

  $parts = $obj->parts();

The array reference of word partitions.

Note that this method is only useful for fetching, since the parts 
are computed by the build_parts() method.

=head2 combinations()

  $combinations = $obj->combinations();

The array reference of all possible word part combinations.

Note that this method is only useful for fetching, since the 
combinations are computed by the build_combinations() method.

=head2 knowns()

  $knowns = $obj->knowns();

The hash reference of known combinations (keys) with their 
familiarity scores (values).  Note that only the non-zero scored 
combinations are kept.

Note that this method is only useful for fetching, since the knowns
are computed by the build_knowns() method.

=head2 definitions()

  $definitions = definitions();

The hash reference of the definitions provided for each fragment of 
the combinations in the knowns hash.  Note that the unknown 
fragments are defined as an empty string.

=head1 DEPENDENCIES

None

=head1 DISCLAIMER

This module uses some clunky, inefficient algorithms.  For instance,
a 50 letter word (like a medical term) just might take until the end
of time to parse and possibly longer.  Please write to me with 
improvements!

=head1 TO DO

Trim the known combinations (and definition list) down to ones that
concatinate the adjacent unknown fragments together.  Then throw out 
the ones with known fragments within that "unknown chunk".

Calculate familiarity with more granularity.  Possibly with a
multidimensional measure.

Output some type of concatinated, known combination definition.

Handle the build_combinations method and related globals correctly.

Compute the time required for a given parse.

Synthesize a term list based on word part (thesaurus) definitions.
(That is, go in reverse! Non-trivial!)

=head1 DEDICATION

My Grandmother and English teacher - Frances Jones

=head1 AUTHOR

Gene Boggs E<lt>cpan@ology.netE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright 2003 by Gene Boggs

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself. 

=cut
