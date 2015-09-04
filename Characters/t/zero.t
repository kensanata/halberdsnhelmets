use strict;
use warnings;
use Test::More tests => 1;
use XML::LibXML;

my $parser = XML::LibXML->new();
my $output = `perl halberdsnhelmets.pl ac=0`;
my ($header, $body) = split("\r\n\r\n", $output, 2);
my $doc = $parser->parse_string($body);

my $svg = XML::LibXML::XPathContext->new;
$svg->registerNs("svg", "http://www.w3.org/2000/svg");

is($svg->findvalue('//svg:text[@id="ac"]', $doc), '0', 'ac zero is shown');
