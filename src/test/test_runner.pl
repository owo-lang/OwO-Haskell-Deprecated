#!/usr/bin/perl

use strict;
use warnings FATAL => 'all';
use Term::ANSIColor;
use v5.10;
$|++;

my $version = 'owo --version';
say "$version: @{[ `$version` ]}";
my @failure = ();
my $success = 0;
my $isCI = defined $ENV{'CI'};

sub ntr {return colored $_[0], 'green';}
sub red {return colored $_[0], 'red';}
sub redy {return colored $_[0], 'bold red';}

foreach my $fixture (map {substr $_, 0, -1}
        split /[ \t\n]+/, `ls -t -d testData/*/`) {
    say colored("Fixture $fixture:", 'yellow');
    my $fixtureFlags = -e "$fixture.flags" ? `cat $fixture.flags` : '';
    foreach my $case (split /[ \t\n]+/, `ls -t -G $fixture/*.owo`) {
        my $out = $case =~ s/\.owo/\.out/rg;
        my $flagFile = $case =~ s/\.owo/\.flags/rg;
        my $caseFlags = -e $flagFile ? `cat $flagFile` : '';
        `touch $out`;
        my $flags = "$fixtureFlags $caseFlags" =~ s/[\n|\r]//rg;
        my $cmd = "owo $flags -c $case";
        my $diff = `$cmd 2>&1 | diff --strip-trailing-cr - $out`;
        if (length $diff) {
            push @failure, $case;
            say red(" Failed $case:");
            map {say red("  $_")} split /\n/, $diff;
            next if $isCI;
            print colored('  Update the golden value (y/N)? ', 'cyan');
            (readline =~ s/[\n\r]//rg) eq 'y' ? `owo $flags -c $case &> $out`
                : say colored(<<"HINT", 'bold yellow');
  Leaving it alone.
  To update the golden value, run `test_runner.pl` in `src/test` directly.
  Command: $cmd
HINT
        } else {
            say ntr(" Passed $case");
            $success++;
        }
    }
}

my $failed = scalar @failure;
say 'Result: ', $failed ? redy('FAILED.') : ntr('ok.'),
    ntr(" $success passed,"),
    colored(" $failed failed.", $failed ? 'bold red' : 'white');
if ($failed) {
    my $pretty = join "\n ", @failure;
    say red("Failing tests:\n $pretty");
    die;
}
