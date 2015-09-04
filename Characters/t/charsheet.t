use strict;
use warnings;
use Test::More tests => 1;

my $output = `perl halberdsnhelmets.pl name=Markus charsheet=file:swn_warrior.svg`;
my ($header, $body) = split("\r\n\r\n", $output, 2);
like($output, qr/sodipodi:docname="swn_warrior.svg"/, 'the correct SVG file was loaded');
