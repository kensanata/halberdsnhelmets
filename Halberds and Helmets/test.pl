#!/bin/env perl
use Modern::Perl '2015';
use Test::More;
use List::Compare;
open(my $fh, '<:encoding(UTF-8)', 'Halberds-and-Helmets-Ref-Guide.ltx')
    or die "Cannot read file: $!";

undef $/;
my $text = <$fh>;

ok($text =~ s/(.)\n([^\n\\])/$1 $2/mg > 0, "fixed all continuation lines");

my @lines = split(/\n/, $text);
ok(@lines > 0, "file was read");

$" = ', '; # interpolating arrays in strings
my @ords = $text =~ m/(\d+(?:st|nd|rd|th))/g;
ok(@ords == 0, "ordinals are all correct"
   . (@ords == 0 ? "" : " (@ords)"));

my @broken_units = $text =~ /(\b\d+\s*(?:m|ft|s|min|h|cp|sp|gp|gp|silver|gold|electrum|platinum)\b)/g;
ok(@broken_units == 0, "units are all correct"
   . (@broken_units == 0 ? "" : " (@broken_units)"));

my @broken_saves = $text =~ /(\bsave\s+vs\.\s+\w+\b)/g;
ok(@broken_saves == 0, "saves are all correct"
   . (@broken_saves == 0 ? "" : " (@broken_saves)"));

my %index;
for (@lines) {
  if (/^\\makeindex.name=([a-z]+)/) { $index{$1}++; }
}

ok(keys %index, "index entries were found");
for my $index (keys %index) {
  ok(grep(/^\\newcommand\{\\$index\}/, @lines), "\\$index command defined");
}
for my $index (keys %index) {
  ok(grep(/^\\printindex\[$index\]/, @lines), "$index index is printed");
}

my $section;
my %h;
for (@lines) {
  if (/^\\section\{([[:alpha:], ]+)\}/) { $section = $1; %h = () };
  if (/^\\([a-z]+)\{([^\}!]+)(![^\}]*)?\}/) {
    if($index{$1}) {
      like($section, qr/^$2/, "$section starts with $2 in the $1 index"); 
      $h{$1} = 1;
    }
  }
  if (/^\\textbf\{Terrain\}: ([a-z, ]+)/) {
    if ($1 ne "none") {
      my @tags = split(/, /, $1);
      delete $h{animal}; # not a terrain
      my @keys = keys %h;
      my $lc = List::Compare->new(\@tags, \@keys);
      ok($lc->get_unique == 0,
	 "$section: all terrain tags appear in the respective index (@tags)");
      ok($lc->get_complement == 0,
	 "$section: all terrain indexes appear as tags (@keys)");
      my @sorted = sort(@tags);
      ok((grep{ $tags[$_] ne $sorted[$_] } 0 .. $#tags) == 0,
	 "terrains for $section are listed in order (@sorted)");
    }
  }
}

for (@lines) {
  $section = $1 if /^\\section\{([[:alpha:], ]+)\}/;
  next unless / *HD \d/;
  like($_, qr/ AC \d+ /, "ac for $section is provided");
  like($_, qr/ [FEHD]\d+ /, "save for $section is provided") unless $section eq "Hydra" or $section eq "Golem";
  like($_, qr/ MV \d+ /, "move for $section is provided");
  like($_, qr/ ML \d+ /, "morale for $section is provided");
  like($_, qr/ XP \d+/, "xp for $section is provided") unless $section eq "Hydra";
}

done_testing;
