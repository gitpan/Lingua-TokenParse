# $Id: TokenParse.pm,v 1.11 2004/05/29 23:08:36 gene Exp $

package Lingua::TokenParse;
$VERSION = '0.1501';
use strict;
use warnings;
use Carp;
use Math::BaseCalc;

sub new {  # {{{
    my $proto = shift;
    my $class = ref $proto || $proto;
    my $self  = {
        verbose      => 0,
        # The word to parse!
        word         => undef,
        # We need to use this.
        word_length  => 0,
        # Known tokens.
        lexicon      => {},
        # All word parts.
        parts        => [],
        # All possible parts combinations.
        combinations => [],
        # Scored list of the known parts combinations.
        knowns       => {},
        # Definitions of the known and unknown fragments in knowns.
        definitions  => {},
        # Fragment definition separator.
        separator    => ' + ',
        # Known-but-not-defined definition output string.
        not_defined  => '.',
        # Unknown definition output string.
        unknown      => '?',
        # Known trimming regexp rules.
        constraints  => [],
        @_,  # slurp anything else and override defaults.
    };
    bless $self, $class;
    $self->_init();
    return $self;
}  # }}}

sub _init {  # {{{
    my $self = shift;
    warn "Entering _init()\n" if $self->{verbose};
    $self->parse( $self->{word} ) if $self->{word} && $self->{lexicon};
}  # }}}

# Accessors {{{
sub word {  # {{{
    my $self = shift;
    warn "word()\n" if $self->{verbose};
    if( @_ ) {
        $self->{word} = shift;
        warn "New word=$self->{word}\n" if $self->{verbose};
        $self->{word_length} = length $self->{word};
        warn "Length=$self->{word_length}\n" if $self->{verbose};
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

sub constraints {  # {{{
    my $self = shift;
    $self->{constraints} = shift if @_;
    return $self->{constraints};
}  # }}}
# }}}

sub parse {  # {{{
    my $self = shift;
    warn "Enter parse()\n" if $self->{verbose};
    $self->word( shift ) if @_;
    croak 'No word provided.' unless defined $self->{word};
    # Reset our data structures.
    $self->parts([]);
    $self->combinations([]);
    $self->knowns({});
    $self->definitions({});
    # Build new ones based on the word.
    $self->build_parts;
    $self->build_combinations;
    $self->build_knowns;
    $self->build_definitions;
    $self->trim_knowns;
}  # }}}

sub build_parts {  # {{{
    my $self = shift;
    warn "Entering build_parts() word=$self->{word} length=$self->{word_length}\n"
        if $self->{verbose};
    for my $i (0 .. $self->{word_length} - 1) {
        for my $j (1 .. $self->{word_length} - $i) {
            push @{ $self->{parts}[$i] },
                substr $self->{word}, $i, $j;
        }
    }
    if($self->{verbose}) {
        warn 'Parts: ';
        warn "\t@$_\n" for @{ $self->{parts} };
    }
}  # }}}

sub build_combinations {  # {{{
    my $self = shift;

    # field size for binary iteration (digits of precision)
    my $y  = $self->{word_length} - 1;
    # total number of combinations
    my $z  = 2 ** $y - 1;
    # field size for the count
    my $lz = length $z;
    # field size for a combination
    my $m  = $self->{word_length} + $y;
    warn "Combinations-1=$z, Field size=$m\n" if $self->{verbose};

    # Truth is a single partition character: the lowly dot.
    my $c = Math::BaseCalc->new( digits => [ 0, '.' ] );

    # Build a word part combination for each iteration.
    for my $n ( 0 .. $z ) {
        # Iterate in base two.
        my $i = $c->to_base( $n );

        # Get the binary digits as an array.
        my @i = split //, sprintf( '%0'.$y.'s', $i );

        # Join the character and digit arrays into a partitioned word.
        my $t = '';
        # ..by stepping over the characters and peeling off a digit.
        for( split //, $self->{word} ) {
            # Zero values become ''. Haha!  Truth prevails.
            $t .= $_ . (shift( @i ) || '');
        }

        unless( grep { $t =~ /$_/ } @{ $self->{constraints} } ) {
            # Preach it.
            printf '%'.$lz.'d) %0'.$y.'s => %'.$m."s\n", $n, $i, $t
                if $self->{verbose};
            push @{ $self->combinations }, $t;
        }
    }
}  # }}}

sub build_knowns {  # {{{
    my $self = shift;

    # Show familiar combinations for each "raw" combination.
    for my $combo (@{ $self->{combinations} }) {
        # Skip combinations that have already been seen.
        next if exists $self->{knowns}{$combo};

        my $sum = 0;
        my ($frag_sum, $char_sum) = (0, 0);

        # Get the bits of the combination.
        my @chunks = split /\./, $combo;

        for (@chunks) {
            # XXX What does this do again?
            # Handle hyphens in lexicon entries.
            ($_, my $combo_seen) = _hyphenate($_, $self->lexicon, 0);

            # Sum the combination familiarity values.
            if ($combo_seen) {
                $frag_sum++;
                $char_sum += length;
            }
        }

        # Stick our combination back together.
        $combo = join '.', @chunks;

        # Save this combination and its familiarity ratios.
        $self->{knowns}{$combo} = [
            $frag_sum / @chunks,
            $char_sum / $self->{word_length}
        ];
    }
}  # }}}

sub build_definitions {  # {{{
    my $self = shift;
    # Save combination entries with their definitions as the values.
    for my $combo (keys %{ $self->{knowns} }) {
        for my $part (split /\./, $combo) {
            $self->{definitions}{$part} = $self->{lexicon}{$part}
                if $self->{lexicon}{$part};
        }
    }
}  # }}}

sub trim_knowns {  # {{{
    my $self = shift;

    my %trimmed;

    # Make a familiar combination from each "raw" combination.
    for my $combo (keys %{ $self->{knowns} }) {
        # Skip combinations that have already been seen.
        next if exists $trimmed{$combo};

        # Get the bits of the combination.
        my @chunks = split /\./, $combo;
        my @seen = ();
        my $unknown = '';

        # Concatinate adjacent unknowns.
        for (@chunks) {
            if (defined $self->{definitions}{$_}) {
                push @seen, scalar _hyphenate($unknown, $self->{lexicon})
                    if $unknown;
                push @seen, $_;
                $unknown = '';
            }
            else {
                $unknown = $unknown . $_;
            }
        }
        push @seen, scalar _hyphenate($unknown, $self->{lexicon})
            if $unknown;

        # Add the combo to the trimmed combinations list and assign
        # the score from the "all possible combinations" list.
        $combo = join '.', @seen;
        $trimmed{$combo} = $self->{knowns}{$combo}
            if exists $self->{knowns}{$combo};
    }

    # Delete trimmed combinations that have defined lexicon entries
    # embedded in the unknowns.
    for my $combo (sort keys %trimmed) {
        # Initialize the "bogus combination flag" for use with the
        # index() function.
        my $seen_position = -1;

        # Inspect each unknown chunk.
        for my $chunk (split /\./, $combo) {
            next if defined $self->{definitions}{$chunk};

            # Do we contain a defined lexicon entry?
            for my $entry (
                grep { defined $self->{definitions}{$_} }
                    sort keys %{ $self->{definitions} }
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

    # Set the knowns list to the new trimmed list.
    $self->knowns(\%trimmed);
}  # }}}

sub learn {  # {{{
    my ($self, %args) = @_;
    # Get the list of (partially) unknown stem combinations.
    # Loop through each looking in %args or prompting for a definition.
}  # }}}

# Update the given string with its actual lexicon value and increment
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
Combination [frag familiarity, char familiarity]
Fragment definitions

HEADER

    for my $known (
        reverse sort {
            $self->{knowns}{$a}[0] <=> $self->{knowns}{$b}[0] ||
            $self->{knowns}{$a}[1] <=> $self->{knowns}{$b}[1]
        } keys %{ $self->{knowns} }
    ) {
        my @definition;
        for my $chunk (split /\./, $known) {
            push @definition,
                defined $self->{definitions}{$chunk}
                    ? $self->{definitions}{$chunk}
                        ? $self->{definitions}{$chunk}
                        : $self->{not_defined}
                    : $self->{unknown};
        }

        push @out, sprintf qq/%s [%s]\n%s/,
            $known,
            join (', ', map { sprintf '%0.2f', $_ }
                @{ $self->{knowns}{$known} }),
            join ($self->{separator}, @definition);
    }

    return wantarray ? @out : $header . join "\n\n", @out;
}  # }}}

1;
__END__

=head1 NAME

Lingua::TokenParse - Parse a word into scored, fragment combinations

=head1 SYNOPSIS

  use Lingua::TokenParse;

  my $obj = Lingua::TokenParse->new(
      word => 'antidisthoughtlessfulneodeoxyribonucleicfoo'
  );
  $obj->lexicon({
      'a'    => 'not',
      'anti' => 'opposite',
      'di'   => 'two',
      'dis'  => 'away',
      'eo'   => 'hmmmmm',
      'ful'  => 'with',
      'les'  => 'without',
      # etc...
  });
  $obj->constraints([ qr/eo./ ]);
  $obj->parse;
  print Dumper($obj->knowns);

=head1 DESCRIPTION

This class represents a Lingua::TokenParse object and contains 
methods to parse a given word into familiar combinations based
on a lexicon of known word parts.

Words like "partition" and "automobile" are composed of different
word parts.  Given a lexicon of known fragments, one can partition
a word into a list of its (possibly overlapping) fragment
combinations.

Each of these combinations can be given a score, which represents a 
measure of word familiarity.  This measure is a set of ratios of
known to unknown parts.

The lexicon is a simple I<fragment => definition> list and must have
a definition for each entry.  This definition can be an empty string
(i.e. ''), but if it is undefined the fragment is considered an
unknown.

Please see the sample code in the distribution C<eg/> directory for 
examples of how this module can be used.

=head1 METHODS

=head2 new

  $obj = Lingua::TokenParse->new(
      word => $word,
      lexicon => \%lexicon,
  );

Return a new Lingua::TokenParse object.

This method will automatically call the partition methods (detailed 
below) if a word and lexicon are provided.

The C<word> can be any string, however, you will want to make sure that 
it does not include the same characters you use for the C<separator>,
C<not_defined> and C<unknown> strings (described below).

The C<lexicon> must be a hash reference with word fragments as keys and
definitions their respective values.  Definitions must be defined in 
order for the trim_knowns method work properly.

=head2 parse

  $obj->parse;
  $obj->parse($word);

This method resets the partition lists and then calls all the 
individual parsing methods that are detailed below.

If a string is provided the word to parse is first set to that.

=head2 build_parts

  $obj->build_parts;

Construct an array of the word partitions, accessed via the parts
method.

=head2 build_combinations

  $obj->build_combinations;

Compute the array of all possible word part combinations, excluding
constraints and accessed via the combinations method.

=head2 build_knowns

  $obj->build_knowns;

Compute the familiar word part combinations, accessed via the knowns
method.

This method handles word parts containing prefix and suffix hyphens,
which encode information about what is a syntactically illegal word 
combination, which can be used to score (or even throw out bogus
combinations).

=head2 build_definitions

  $obj->build_definitions;

Construct a hash of the definitions of the word parts in each 
combination in the keys of the knowns hash.  This hash is accessed
via the definitions method.

=head2 trim_knowns

  $obj->trim_knowns;

Trim the hash of known combinations by concatinating adjacent unknown
fragments and throwing out combinations with a score of zero.

=head1 CONVENIENCE METHOD

=head2 output_knowns

  @ = $obj->output_knowns;
  print Dumper \@knowns;

  # Look at the "even friendlier output."
  print scalar $obj->output_knowns(
      separator   => $separator,
      not_defined => $not_defined,
      unknown     => $unknown,
  );

This method returns the familiar word part combinations in a couple
"human accessible" formats.  Each have familiarity scores rounded to
two decimals and fragment definitions shown in a readable layout

=over 4

=item separator

The the string used between fragment definitions.  Default is a plus
symbol surrounded by single spaces: ' + '.

=item not_defined

Indicates a known fragment that has no definition.  Default is a
single period: '.'.

=item unknown

Indicates an unknown fragment.  The default is the question mark: '?'.

=back

=head1 ACCESSORS

=head2 word

  $word = $obj->word;
  $obj->word($word);

The actual word to partition which can be any string.

=head2 lexicon

  $lexicon = $obj->lexicon;
  $obj->lexicon(\%lexicon);

The lexicon is a hash reference with word fragments as keys and
definitions their respective values.

=head2 parts

  $parts = $obj->parts;

The array reference of all possible word partitions.

=head2 combinations

  $combinations = $obj->combinations;

The array reference of all possible word part combinations.

=head2 knowns

  $knowns = $obj->knowns;

The hash reference of known (non-zero scored) combinations with their
familiarity values.

=head2 definitions

  $definitions = $obj->definitions;

The hash reference of the definitions provided for each fragment of 
the combinations with the values of unknown fragments set to undef.

=head2 constraints

  $constraints = $obj->constraints;
  $obj->constraints(\@regexps);

An optional, user defined array reference of regular expressions to
apply to the list of known combinations.  This is acts as a negative
pruning device.  Taht is, if a match is successful, the entry is
excluded from the list.

=head1 TO DO

Compute the time required for a given parse.

Make a method to request definitions for unknown fragments and call
it... C<learn()>.

Use traditional stemming to trim down the common knowns and see if
the score is the same...

Synthesize a term list based on a thesaurus of word-part definitions.
That is, go in reverse.  Non-trivial!

=head1 SEE ALSO

L<Math::BaseCalc>

=head1 DEDICATION

For my Grandmother and English teacher Frances Jones.

=head1 THANK YOU

Thank you to Luc St-Louis for helping me increase the speed while
eliminating the exponential memory footprint.  I wish I knew your
email address so I could tell you.  :-) B<lucs++>

=head1 AUTHOR

Gene Boggs E<lt>gene@cpan.orgE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2003-2004 by Gene Boggs

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself. 

=cut
