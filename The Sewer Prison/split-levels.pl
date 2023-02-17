#!/usr/bin/env perl
use Modern::Perl;
use XML::LibXML;
my $svg_file = shift;
die "Missing SVG file argument\n" unless $svg_file;
die "Cannot read $svg_file\n" unless -r $svg_file;
my $pattern = shift;
die "Missing result file pattern argument\n" unless $pattern;
die "Pattern $pattern does not contain %d for the level\n" unless $pattern =~ /%d/;
my $dom = XML::LibXML->load_xml(location => $svg_file);
$dom->documentElement->setAttribute('class', 'white');
my $xpc = XML::LibXML::XPathContext->new;
$xpc->registerNs('svg', 'http://www.w3.org/2000/svg');
my $levels = $xpc->find('//svg:g[@id="levels"]', $dom)->shift();
my $n = 0;
while (1) {
  my $found = 0;
  for my $node ($levels->getChildrenByTagName('g')) {
    if ($node->getAttribute('id') eq "level$n") {
      $node->setAttribute('opacity', '1');
      $found = 1;
    } else {
      $node->setAttribute('opacity', '0');
    }
  }
  last unless $found;
  my $filename = sprintf($pattern, $n+1);
  open(my $fh, '>', $filename) or die "Cannot open $filename for writing: $!";
  print $fh $dom->toString;
  close($fh);
  $n++;
}
