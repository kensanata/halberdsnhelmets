#!/bin/env perl
use Modern::Perl '2015';
use Test::More;
open(my $fh, '<:encoding(UTF-8)', 'Halberds-and-Helmets-Ref-Guide.ltx')
    or die "Cannot read file: $!";

undef $/;
my $text = <$fh>;

ok($text =~ s/(.)\n([^\n\\])/$1 $2/mg > 0, "fixed all continuation lines");

my @lines = split(/\n/, $text);
ok(@lines > 0, "file was read");

$" = ', '; # interpolating arrays in strings
my @ords = $text =~ m/(\d(?:st|nd|rd|th))/g;
ok(@ords == 0, "ordinals are all correct"
   . (@ords == 0 ? "" : " (@ords)"));

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
    my @tags = split(/, /, $1);
    for my $tag (@tags) {
      next if $tag eq "none";
      ok($h{$tag}, "\\$tag exists for $section");
    }
    for my $key (keys %h) {
      next if $key eq 'animal';
      ok(grep(/^$key/, @tags), "terrain $key is listed in $section (@tags)");
    }
  }
}

for (@lines) {
  $section = $1 if /^\\section\{([[:alpha:], ]+)\}/;
  next unless /^ *HD \d/;
  like($_, qr/ AC \d+ /, "ac for $section is provided");
  like($_, qr/ [FEHD]\d+ /, "save for $section is provided") unless $section eq "Hydra";
  like($_, qr/ MV \d+ /, "move for $section is provided");
  like($_, qr/ ML \d+ /, "morale for $section is provided");
  like($_, qr/ XP \d+/, "xp for $section is provided") unless $section eq "Hydra";
}

done_testing;
