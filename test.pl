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
ok(@broken_units == 0, 'units use \SI{number}{unit}'
   . (@broken_units == 0 ? "" : " (@broken_units)"));

my @broken_saves = $text =~ /(\bsave\s+vs\.\s+\w+\b)/g;
ok(@broken_saves == 0, "saves use ~"
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
  if (/^\\section\{([[:alpha:], ]+)\}/) {
    is(scalar(keys %h), 0, "$section has a terrain line") if $section;
    $section = $1;
    %h = ();
  } elsif (/^\\textbf\{Terrain\}: ([a-z, ]+)/) {
    if ($1 ne "none") {
      my @tags = split(/, /, $1);
      delete $h{animal}; # not a terrain
      delete $h{undead}; # not a terrain
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
    %h = ();
  } elsif (/^\\([a-z]+)\{([^\}!]+)(![^\}]*)?\}/) {
    if($index{$1}) {
      like($section, qr/^$2/, "$section starts with $2 in the $1 index");
      $h{$1} = 1;
    }
  }
}

my $hd;
for (@lines) {
  $section = $1 if /^\\section\{([[:alpha:], ]+)\}/;
  next unless ($hd) = / *HD (\d+)/;
  like($_, qr/\bAC\s+\d+\b/, "ac for $section is provided");
  like($_, qr/\b[FMEHDT]\d+\b/, "save for $section is provided") unless $section eq "Hydra" or $section eq "Golem";
  like($_, qr/\bMV\s+\d+\b/, "move for $section is provided");
  like($_, qr/\bML\s+\d+\b/, "morale for $section is provided");
  like($_, qr/\bXP\s+\d+\b/, "xp for $section is provided") unless $section eq "Hydra";
  like($_, qr/\bXP\s+${hd}00\b/, "xp for $section is HD×100") unless $section eq "Hydra";
}

my %treasure;
my $in;
my @monsters;
my $monster;
for (@lines) {
  $in = 0 if $in and (/^\\chapter/);
  $in = 1 if !$in and (/^\\chapter\{Monsters\}/);
  next unless $in;
  if (/^\\section\{([[:alpha:], ]+)\}/) {
    $monster = $1;
    push(@monsters, $monster);
  }
  next unless /\\(notreasure|poor|average|rich|robber|ancient|scout|terror|dead|dragon|dwarf|stronghold)\b/;
  my $type = $1;
  ok(!$treasure{$monster}, "just one treasure for $monster is provided ($type)");
  $treasure{$monster} = $type;
}

for my $monster (@monsters) {
  ok($treasure{$monster}, "treasure for $monster is provided");
}

my %types; # the lines per treasure type listing the monsters
for (@lines) {
  $in = 0 if $in and (/^\\chapter/);
  $in = 1 if !$in and (/^\\chapter\{Treasure\}/);
  next unless $in;
  if (/^\\textbf\{([[:alpha:], ]+)\}/) {
    my $type = lc($1);
    $type = "notreasure" if $type eq "none";
    if ($type eq "dragon") {
      $types{$type} = "dragon";
    } elsif ($type eq "dwarf") {
      $types{$type} = "dwarf";
    } else {
      $types{$type} = $_;
    }
  }
}

# check that every monster's treasure type is listed appropriately
for my $monster (keys %treasure) {
  my $type = $treasure{$monster};
  $monster =~ s/(.*), (Large|Giant)/$2 $1/;
  $monster =~ s/(.*), People/$1 people/;
  $monster = lc($monster);
  like($types{$type}, qr($monster), "$monster is listed under $type in the appendix");
}

# Check that we're not using the wrong quote as an apostrophe.
my $n = grep(/’/, @lines);
is($n, 0, "U+2019 (RIGHT SINGLE QUOTATION MARK) isn't used");
# Use ' instead. We cannot U+02BC as recommended in
# https://tedclancy.wordpress.com/2015/06/03/which-unicode-character-should-represent-the-english-apostrophe-and-why-the-unicode-committee-is-very-wrong/
# because we're going to get "not set up for use with LaTeX."

done_testing;
