#!/usr/bin/env perl

# Copyright (C) 2016 Alex Schroeder <alex@gnu.org>

# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# this program. If not, see <http://www.gnu.org/licenses/>.

use utf8;
use Test::More;
use Test::Mojo;
use FindBin;
use strict;
use warnings;

$ENV{MOJO_HOME} = "$FindBin::Bin/..";
require "$FindBin::Bin/../halberdsnhelmets.pl";

my $t = Test::Mojo->new;

$t->get_ok('/decode/de?code=BADA97H46-P8NQLR244McCV')
    ->status_is(200)
    ->header_is('Content-Type' => 'text/html;charset=UTF-8');

sub simplify {
  my $str = shift;
  $str =~ s/^\n*//;
  $str =~ s/^name:.*\n//m;
  $str =~ s/^charsheet:.*\n//m;
  $str =~ s/^rules:.*\n//m;
  $str =~ s/^portrait:.*\n//m;
  $str =~ s/^property: \d+ Gold\n//m;
  $str =~ s/^abilities: Code: .*\n//m;
  return $str;
}

my $new = simplify($t->tx->res->dom->at('textarea')->content);
my $original = simplify(<<EOT);
name: Diara
str: 11
dex: 10
con: 13
int: 10
wis: 9
cha: 7
level: 1
xp: 0
thac0: 19
class: Halbling
hp: 4
ac: 6
property: Rucksack
property: Feldrationen (1 Woche)
property: Lederrüstung
property: Schild
property: Langschwert
property: Schleuder
property: Beutel mit 30 Steinen
property: Dolch (2)
property: Laterne
property: Ölflasche
property: Holzstab
property: Spiegel
property: 4 Gold
abilities: 1/6 für normale Aufgaben
abilities: 2/6 um Geräusche zu hören
abilities: 2/6 für Verstecken und Schleichen
abilities: 5/6 für Verstecken und Schleichen im Freien
abilities: +1 für Fernwaffen
abilities: Rüstung -2 bei Gegnern über Menschengrösse
abilities: Code: BADA97H46-P8NQLR2McCV
charsheet: Charakterblatt.svg
portrait: https://campaignwiki.org/face/render/alex/eyes_all_39.png_,mouth_all_109.png,chin_woman_32.png,ears_all_21.png,nose_woman_elf_11.png,hair_woman_72.png
breath: 13
poison: 8
petrify: 10
wands: 9
spells: 12
EOT

sub line {
  my ($str, $i) = @_;
  my $offset = $i;
  while ($offset > 0 and substr($str, $offset, 1) ne "\n") { $offset-- };
  $offset++;
  my $length = $i - $offset;
  while ($offset + $length < length($str) and substr($str, $offset + $length, 1) ne "\n") { $length++ };
  return substr($str, $offset, $length);
}


for (my $i = 0; $i < length($new); $i++) {
  if (substr($original, $i, 1) ne substr($new, $i, 1)) {
    die "pos $i:\n"
	. "< " . line($original, $i) . "\n"
	. "> " . line($new, $i) . "\n";
  }
}
is($new, $original, "matches original character sheet");

done_testing;
