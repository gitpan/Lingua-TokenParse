package Lingua::TokenParse;

use strict;
use vars qw($VERSION);
$VERSION = '0.02';

sub new {  # {{{
    my ($class, %args) = @_;

    my $self = {
        word => $args{word} || undef,     # The word to parse!
        lexicon => $args{lexicon} || {},  # The list of known tokens.
        parts => [],         # The list of all word parts.
        combinations => [],  # The list of all possible parts combinations.
        knowns => {},        # The scored list of the known parts combinations.
    };

    bless $self, $class;

    if ($args{word} and $args{lexicon}) {
        $self->build_parts;
        $self->successors;
        $self->trim_combinations;
    }

    return $self;
}  # }}}

sub word {  # {{{
    my $self = shift;
    $self->{word} = shift if @_;
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

sub build_parts {  # {{{
    my $self = shift;

    my $len = length $self->word;

    for my $i (0 .. $len - 1) {
        for my $j (1 .. $len - $i) {
            push @{ $self->parts->[$i] }, substr $self->word, $i, $j;
        }
    }
}  # }}}

sub trim_combinations {  # {{{
    my $self = shift;

    # Make a familiar combination from each "raw" combination.
    for my $combo (@{ $self->combinations }) {
        my $sum = 0;

        # Get the bits of the combination.
        my @chunks = split /\./, $combo;

        # Compute the combination familiarity value and flag the
        # unknown bits.
        for (@chunks) {
            if (exists $self->lexicon->{$_}) {
                $sum++;
            }
            else {
                $_ .= '~';
            }
        }

        # Mash the adjacent unknown bits together.
        my (@seen, $unknown);
        for (@chunks) {
            if (/~$/) {
                s/~$//;
                $unknown .= $_;
            }
            else {
                push @seen, $unknown if $unknown;
                push @seen, $_;
                $unknown = '';
            }
        }
        push @seen, $unknown if $unknown;

        # Save this combination with the familiarity ratio for the
        # value.
        $self->knowns->{ join '.', @seen } = $sum / @seen if $sum;
    }
}  # }}}

# Globals used by the successors method.
my (@parsed, @new, $prev);
sub successors {  # {{{
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

        $self->successors($n);
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

Lingua::TokenParse - Parse a word into scored, familiar combinations

=head1 SYNOPSIS

  use Lingua::TokenParse;

  my %lexicon;
  @lexicon{qw(part i tion on)} = ();

  my $obj = Lingua::TokenParse->new(
      word    => 'partition',
      lexicon => \%lexicon,
  );

  $obj->output_knowns;

=head1 ABSTRACT

This class represents a Lingua::TokenParse object and contains 
methods to parse a given word into familiar combinations based on a 
lexicon of known word parts.

=head1 DESCRIPTION

A word like "partition" is actually composed of a few different word
parts.  Given a lexicon of known word parts, it is possible to 
partition this word into combinations of these (possibly overlapping)
parts.  Each of these combinations can be given a score, which 
represents a measure of familiarity.

Currently, this familiarity mesasure is a simple ratio of known to 
unknown parts.

=head1 METHODS

=head2 new()

  $obj = Lingua::TokenParse->new(
      word    => $word,
      lexicon => \%lexicon,
  );

Return a new Lingua::TokenParse object.

This method will automatically call the partition methods (detailed 
below) if a word and lexicon are provided.

=head2 build_parts()

  $obj->build_parts();

Construct an array of the word partitions.

=head2 successors()

  $obj->successors();

Recursively compute the array of all possible word part combinations.

=head2 trim_combinations()

  $obj->trim_combinations();

Compute the familiar word part combinations.

=head2 output_knowns()

  $obj->output_knowns();

Convenience method to output the familiar word part combinations.

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
combinations are computed by the successors() method.

=head2 knowns()

  $knowns = $obj->knowns();

The hash reference of known combinations (keys) with their 
familiarity scores (values).  Note that only the non-zero scored 
combinations are kept.

Note that this method is only useful for fetching, since the knowns
are computed by the trim_combinations() method.

=head1 DEPENDENCIES

None

=head1 TO DO

Handle the successor method and globals correctly.

Return word part definitions.

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