use Test::More tests => 1;
use utf8;

# Test::More explains how to fix wide character in print issues
my $builder = Test::More->builder;
binmode $builder->output,         ":utf8";
binmode $builder->failure_output, ":utf8";
binmode $builder->todo_output,    ":utf8";

my $output = `perl halberdsnhelmets.pl '/translation?'`;
my %translations = split(/\n/, $output);

my $empty = 0;
for my $english (keys %translations) {
  if (not $translations{$english}) {
    $empty++;
    diag("$english is missing a German translation");
  }
}
ok($empty == 0, "No missing German translations.");
