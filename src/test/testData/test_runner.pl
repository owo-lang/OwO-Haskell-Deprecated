#!/usr/bin/perl
use strict;
use warnings FATAL => 'all';
use Term::ANSIColor;
use v5.10;

my $version = 'owo --version';
say "$version: @{[ `$version` ]}";
my @failure = ();
my $noTerm = scalar @ARGV && $ARGV[0] eq '--no-terminal';

foreach my $fixture (map {substr $_, 0, -1} split(/[ \t\n]+/, `ls -G -d */`)) {
    say "Fixture $fixture:";
    `touch $fixture.flags`;
    my $flags = `cat $fixture.flags`;
    foreach my $case (split(/[ \t\n]+/, `ls -G $fixture/*.owo`)) {
        say " Case $case:";
        my $out = $case =~ s/\.owo/\.out/rg;
        my $diff = `owo $flags -c $case | diff - $out`;
        if (length $diff) {
            push @failure, $case;
            map {say(colored("  $_", 'red'))} split(/\n/, $diff);
            print colored("  Replace the golden value (y/N)? ", 'cyan');
            !$noTerm && getc eq 'y' ? `owo $flags -c $case > $out`
                : say colored('  Leave it alone.', 'yellow');
        } else {
            say colored('  Passed!', 'green');
        }
    }
}

say '';
if (scalar @failure) {
    my $pretty = join("\n ", @failure);
    say colored("Failing tests:\n $pretty", 'red');
    die;
} else {
    say colored('All tests passed!', 'green');
}
