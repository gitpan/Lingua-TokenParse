package Lingua::TokenParse;

use strict;
use vars qw($VERSION);
$VERSION = '0.12.2';

# NOTE: The {{{ and }}} things are "editor code fold markers".  They
# are merely a convenience for people who don't care to scroll through
# reams of source, like me.  vim++ (C<http://www.vim.org>, of course.)
#
# Also note that things are sorted in loops for debugging purposes only.

sub new {  # {{{
    my ($class, %args) = @_;

    my $self = {
        # The word to parse!
        word         => $args{word} || undef,
        # We need to use the length of our word in a few methods.
        _word_length => exists $args{word} ? length ($args{word}) : 0,
        # Known tokens.
        lexicon      => $args{lexicon} || {},
        # All word parts.
        parts        => [],
        # All possible parts combinations.
        combinations => [],
        # Scored list of the known parts combinations.
        knowns       => {},
        # Definitions of the known and unknown fragments in knowns.
        definitions  => {},
        # Fragment definition separator.
        separator    => $args{separator} || ' + ',
        # Known-but-not-defined definition output string.
        not_defined  => $args{not_defined} || '.',
        # Unknown definition output string.
        unknown      => $args{unknown} || '?',
        # Known trimming regexp rules.
        rules        => [],
        # Globals used by the build_combinations method (and
        # initialized by the _reset_parse method).
        _new  => [],
        _prev => 0,
    };

    bless $self, $class;

    $self->parse if $self->word and $self->lexicon;

    return $self;
}  # }}}

# Accessors {{{
sub word {  # {{{
    my $self = shift;
    if (@_) {
        $self->{word} = shift;
        $self->{_word_length} = length $self->{word};
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

sub separator {  # {{{
    my $self = shift;
    $self->{separator} = shift if @_;
    return $self->{separator};
}  # }}}

sub not_defined {  # {{{
    my $self = shift;
    $self->{not_defined} = shift if @_;
    return $self->{not_defined};
}  # }}}

sub unknown {  # {{{
    my $self = shift;
    $self->{unknown} = shift if @_;
    return $self->{unknown};
}  # }}}

sub rules {  # {{{
    my $self = shift;
    $self->{rules} = shift if @_;
    return $self->{rules};
}  # }}}
# }}}

sub _reset_parse {  # {{{
    my $self = shift;
    $self->parts([]);
    $self->combinations([]);
    $self->knowns({});
    $self->definitions({});
    $self->{_new} = [];
}  # }}}

sub parse {  # {{{
    my $self = shift;
    $self->_reset_parse;
    $self->build_parts;
    $self->build_combinations;
    $self->build_knowns;
    $self->build_definitions;
    $self->trim_knowns;
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
    $self->{_prev} = 0 unless defined $self->{_prev};

    for (@{ $self->parts->[$i] }) {
        # Find the end-position of the stem.
        my $n = $i + length;

        # XXX This is an ugly mystery-hack:
        # Yank-off the last two stems found, if we are at an
        # "overlap point".
        splice @{ $self->{_new} }, -2 if $self->{_prev} > $i;

#print "$_ - i: $i, n: $n, prev: $self->{_prev}, new: ". @{ $self->{_new} } ."\n";

        $self->{_prev} = $i;

        splice @{ $self->{_new} }, @{ $self->{_new} }, $n, $_;

        push @{ $self->combinations }, join '.', @{ $self->{_new} }
            if $n == length ($self->word);

        $self->build_combinations($n);
    }
}  # }}}

sub build_knowns {  # {{{
    my $self = shift;

    # Show familiar combinations for each "raw" combination.
    for my $combo (@{ $self->combinations }) {
        # Skip combinations that have already been seen.
        next if exists $self->knowns->{$combo};

        my $sum = 0;
        my ($frag_sum, $char_sum) = (0, 0);

        # Get the bits of the combination.
        my @chunks = split /\./, $combo;

        for (@chunks) {
            # Handle hyphens in lexicon entries.
            ($_, my $combo_seen) = _hyphenate($_, $self->lexicon, 0);

            # Sum the combination familiarity value.
            if ($combo_seen) {
                $frag_sum++;
                $char_sum += length;
            }
        }

        $combo = join '.', @chunks;

        # Save this combination with its familiarity ratio.
        $self->knowns->{$combo} = [
            $frag_sum / @chunks,
            $char_sum / $self->{_word_length}
        ];
    }
}  # }}}

sub build_definitions {  # {{{
    my $self = shift;
    # Save combination entries with their definitions as the values.
    for my $combo (keys %{ $self->knowns }) {
        for (split /\./, $combo) {
            $self->definitions->{$_} =
                exists $self->lexicon->{$_}
                    ? $self->lexicon->{$_} : undef
        }
    }
}  # }}}

sub trim_knowns {  # {{{
    my $self = shift;

    my %trimmed;

    # Make a familiar combination from each "raw" combination.
    for my $combo (keys %{ $self->knowns }) {
        # Skip combinations that have already been seen.
        next if exists $trimmed{$combo};

        # Get the bits of the combination.
        my @chunks = split /\./, $combo;
        my @seen = ();
        my $unknown = '';

        # Concatinate adjacent unknowns.
        for (@chunks) {
            if (defined $self->definitions->{$_}) {
                push @seen, scalar _hyphenate($unknown, $self->lexicon)
                    if $unknown;
                push @seen, $_;
                $unknown = '';
            }
            else {
                $unknown = $unknown . $_;
            }
        }
        push @seen, scalar _hyphenate($unknown, $self->lexicon)
            if $unknown;

        # Add the combo to the trimmed combinations list and assign
        # the score from the "all possible combinations" list.
        $combo = join '.', @seen;
        $trimmed{$combo} = $self->knowns->{$combo};
    }

    # Delete trimmed combinations that have defined lexicon entries
    # embedded in the unknowns.
    for my $combo (sort keys %trimmed) {
        # Initialize the "bogus combination flag" for use with the
        # index() function.
        my $seen_position = -1;

        # Inspect each unknown chunk.
        for my $chunk (split /\./, $combo) {
            next if defined $self->definitions->{$chunk};

            # Do we contain a defined lexicon entry?
            for my $entry (
                grep { defined $self->definitions->{$_} }
                    sort keys %{ $self->definitions }
            ) {
                # Strip off the stem-hyphen, if it exists.
                $entry =~ s/-//;

                # Set the seen_position flag if we contain a known
                # fragment.
                $seen_position = index $chunk, $entry;

                # Bail out, if we found a bogus combination.
                last if $seen_position >= 0;
            }

            # Bail out, if we found a bogus combination.
            last if $seen_position >= 0;
        }

        # Remove combinations that were flagged as being bogus.
        delete $trimmed{$combo} if $seen_position >= 0;
    }

    # Execute user provided regular expressions.
    for my $combo (sort keys %trimmed) {
        my $matched = 0;

        # Flag matching rules as bogus.
        for my $rule (@{ $self->rules }) {
            if ($combo =~ /$rule/) {
                $matched++;
                last;
            }
        }

        # Remove this combination if it was flagged as being bogus.
        delete $trimmed{$combo} if $matched;
    }

    # Set the knowns list to the new trimmed list.
    $self->knowns(\%trimmed);
}  # }}}

# Update the given string with it's actual lexicon value and increment
# the seen flag.
sub _hyphenate {  # {{{
    my ($string, $lexicon, $combo_seen) = @_;

    if (exists $lexicon->{$string}) {
        $combo_seen++ if defined $combo_seen;
    }
    elsif (exists $lexicon->{"-$string"}) {
        $combo_seen++ if defined $combo_seen;
        $string = "-$string";
    }
    elsif (exists $lexicon->{"$string-"}) {
        $combo_seen++ if defined $combo_seen;
        $string = "$string-";
    }

    return wantarray ? ($string, $combo_seen) : $string;
}  # }}}

sub output_knowns {  # {{{
    my $self = shift;
    my @out = ();

    my $header = <<HEADER;
Combination [fragment familiarity, character familiarity]
Fragment definitions (with the fragment separator, a period for known
but not defined, and a question mark for unknowns).

HEADER

    for (reverse sort {
            $self->knowns->{$a}[0] <=> $self->knowns->{$b}[0]
            ||
            $self->knowns->{$a}[1] <=> $self->knowns->{$b}[1]
        } keys %{ $self->knowns }
    ) {
        my @definition;
        for my $chunk (split /\./) {
            push @definition,
                defined $self->definitions->{$chunk}
                    ? $self->definitions->{$chunk}
                        ? $self->definitions->{$chunk}
                        : $self->not_defined
                    : $self->unknown;
        }

        push @out, sprintf qq/%s [%s]\n%s/,
            $_,
            join (', ', map { sprintf '%0.2f', $_ } @{ $self->knowns->{$_} }),
            join ($self->separator, @definition);
    }

    return wantarray ? @out : $header . join "\n\n", @out;
}  # }}}

1;
__END__

=head1 NAME

Lingua::TokenParse - Parse a word into scored, fragment combinations

=head1 SYNOPSIS

  use Lingua::TokenParse;

  my $word = 'partition';
  my %lexicon;
  @lexicon{qw(art ion ti)} = qw(foo bar baz);
  my $obj = Lingua::TokenParse->new(
      word    => $word,
      lexicon => \%lexicon,
  );
  print scalar $obj->output_knowns;

  # Okay.  Now, let's parse a new word.
  $obj->word('metaphysical');
  $obj->lexicon({
      'meta-' => 'more comprehensive',
      'ta'    => '',
      'phys'  => 'natural science, singular',
      '-ic'   => 'being, containing',
      '-al'   => 'relating to, characterized by',
  });
  $obj->rules([ qr/^me\./ ]);  # Remove combos that start with "me."
  $obj->parse;
  my @knowns = $obj->output_knowns;

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

This familiarity measure is a set of simple ratios of known to 
unknown parts.

Note that the lexicon must have definitions for each entry, in order 
to have the current trim_knowns method do the right thing.  The 
definition can be an empty string (i.e. '').  If the definition is 
undefined, the fragment is considered an unknown.

Please see the sample code in the distributions eg/ directory for 
examples of how this module can be used.

=head1 METHODS

=head2 new()

  $obj = Lingua::TokenParse->new(
      word        => $word,
      lexicon     => \%lexicon,
      separator   => $separator,
      not_defined => $not_defined,
      unknown     => $unknown,
  );

Return a new Lingua::TokenParse object.

This method will automatically call the partition methods (detailed 
below) if a word and lexicon are provided.

The word can be any string, however, you will want to make sure that 
it does not include the same characters you use for the separator,
not_defined and unknown strings (described below).

The lexicon must be a hash reference with word fragments as keys and
definitions their respecive values.  Definitions must be defined in 
order for the trim_knowns method work properly.

The separator is the string used to separate fragment definitions in
the output_knowns method.  The default is a plus symbol surrounded 
by single spaces (' + ').

The not_defined argument is the the string used by the output_knowns 
method to indicate a known fragment that has no 
definition.  The default is a period (.).

The unknown argument is the the string used by the output_knowns 
method to indicate an unknown fragment.  The default is the question
mark (?).

=head2 parse()

  $obj->parse();

This method resets the partition lists and then calls all the 
indiviual parsing methods that are detailed below.

Call this method after resetting the object with a new word and 
optionally, a new lexicon.

=head2 build_parts()

  $obj->build_parts();

Construct an array of the word partitions, accessed via the parts
method.

=head2 build_combinations()

  $obj->build_combinations();

Recursively compute the array of all possible word part combinations,
accessed via the combinations method.

=head2 build_knowns()

  $obj->build_knowns();

Compute the familiar word part combinations, accessed via the knowns
method.

This method handles word parts containing prefix and suffix hyphens,
which encode information about what is a syntactically illegal word 
combination, which can be used to score (or even throw out bogus
combinations).

=head2 build_definitions()

  $obj->build_definitions();

Construct a hash of the definitions of the word parts in each 
combination in the keys of the knowns hash.  This hash is accessed
via the definitions method.

=head2 trim_knowns()

  $obj->trim_knowns();

Trim the hash of known combinations by concatinating adjacent unknown
fragments and throwing out combinations with a score of zero.

The end of this method is where user defined rules are processed.

=head2 output_knowns()

  print scalar $obj->output_knowns();

  @knowns = $obj->output_knowns();

Convenience method to return the familiar word part combinations with
their familiarity scores (rounded to two decimals) and fragment 
definitions.

In scalar context, a single, newline separated string is returned.
In array context, each of these scored combinations, with their 
fragment definitions is a separate entry in an array.

Here is the format of the output:

  Combination [fragment familiarity, character familiarity]
  Fragment definitions (with the defined fragment separator and a ?
  character for unknowns).

=head1 ACCESSORS

These accessors both get and set their respective values.  Note 
that, if you set the word, lexicon or rules after construction, you 
must manually initialize the parse lists and run the partition 
methods (via the parse method).

Also, note that it is useless to set the parts, combinations and 
knowns lists, since they are computed by the partition methods.

=head2 word()

  $word = $obj->word($word);

The actual word to partition.

Ths word can be any string, however, you will want to make sure that 
it does not include the same characters you use for the separator,
not_defined and unknown strings.

=head2 lexicon()

  $lexicon = $obj->lexicon(\%lexicon);

The lexicon is a hash reference with word fragments as keys and
definitions their respecive values.  Definitions must be defined in 
order for the trim_knowns method to work properly.

=head2 parts()

  $parts = $obj->parts();

The array reference of word partitions.

Note that this method is only useful for fetching, since the parts 
are computed by the build_parts method.

=head2 combinations()

  $combinations = $obj->combinations();

The array reference of all possible word part combinations.

Note that this method is only useful for fetching, since the 
combinations are computed by the build_combinations method.

=head2 knowns()

  $knowns = $obj->knowns();

The hash reference of known combinations (keys) with their 
familiarity scores (values).  Note that only the non-zero scored 
combinations are kept.

Note that this method is only useful for fetching, since the knowns
are computed by the build_knowns method.

=head2 definitions()

  $definitions = $obj->definitions();

The hash reference of the definitions provided for each fragment of 
the combinations in the knowns hash.  Note that the values of unknown
fragments are set to undef.

=head2 rules()

  $rules = $obj->rules(\@rules);

An optional, user defined array reference of regular expressions to
apply to the list of known combinations.  If a match is successful, 
the entry is removed from the list.

To reiterate, this is a negative, pruning device, that is used in the
trim_knowns method.

=head2 separator()

  $separator = $obj->separator($separator);

The separator is the string used to separate fragment definitions in
the output_knowns method.  The default is a plus symbol surrounded 
by single spaces (' + ').

=head2 not_defined()

  $not_defined = $obj->not_defined($not_defined);

The not_defined argument is the the string used by the output_knowns 
method to indicate a known fragment that has no 
definition.  The default is a period (.).

=head2 unknown()

  $unknown = $obj->unknown($unknown);

The unknown argument is the the string used by the output_knowns 
method to indicate an unknown fragment.  The default is the question
mark (?).

=head1 DEPENDENCIES

None

=head1 DISCLAIMER

This module uses some clunky, inefficient algorithms.  For instance,
a 50 letter word (like a medical term) just might take until the end
of time to parse and possibly longer.  Please write to me with much 
needed improvements.

=head1 TO DO

Compute the time required for a given parse.

Synthesize a term list based on word part (thesaurus) definitions.
(That is, go in reverse. Non-trivial!)

=head1 DEDICATION

My Grandmother and English teacher - Frances Jones

=head1 AUTHOR

Gene Boggs E<lt>cpan@ology.netE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright 2003 by Gene Boggs

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself. 

=cut
