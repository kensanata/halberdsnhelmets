use XML::LibXML;
my ($file, $index, $merged) = ('Hellebarden-und-Helme.html', 'theindex.html', 'merged.html');
my $parser = XML::LibXML->new();

print "Reading $file\n";
undef $/;
open (F, '<:encoding(UTF-8)', $file) or die "Cannot open $file: $!\n";
my $str = <F>;
close (F);
$str =~ s!<a name="([^"]+)" id="\1">!<a name="$1">!g;
my $file_doc = $parser->load_html(string => $str) or die "Cannot parse $file: $@\n";
my $file_div = $file_doc->findnodes('//div[@id="content"]')->[0] or die "Cannot find content div\n";

print "Reading $index\n";
my $index_doc = $parser->parse_html_file($index) or die "Cannot parse $file: $@\n";
my $index_div = $index_doc->findnodes('//div[@id="content"]')->[0] or die "Cannot find index div\n";

print "Removing index ids\n";
for my $element ($index_div->getElementsByTagName('*')) {
    $element->removeAttribute('id');
}

print "Making links local\n";
for my $element ($index_div->getElementsByTagName('a')) {
  my $href = $element->getAttribute('href');
  $href =~ s/^Hellebarden-und-Helme\.html//;
  $element->setAttribute('href', $href);
}

print "Downgrading index headings\n";
for my $element ($index_div->getElementsByTagName('h2')) {
    $element->setNodeName('strong');
}

print "Transforming DOM\n";
for my $child ($index_div->childNodes()) {
    $file_div->addChild($child);
}

# print "Adding entry to table of contents\n";
# my $ul = $file_doc->findnodes('//div[@id="text-table-of-contents"]/ul')->[0];
# my $a = $file_doc->createElement('a');
# $a->setAttribute('href', "#idx-1");
# $a->appendText('Index');
# my $li = $file_doc->createElement('li');
# $li->appendChild($a);
# $ul->appendChild($li);

print "Cleaning up XHTML\n";
my $xhtml = $file_doc->toString();

my $re = quotemeta q{
<?xml version="1.0" encoding="utf-8"??>};
$xhtml =~ s!$re!!;

$re = quotemeta q{<html xmlns="http://www.w3.org/1999/xhtml" xmlns="http://www.w3.org/1999/xhtml" lang="de" xml:lang="de" xml:lang="de">};
$xhtml =~ s!$re!<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="de">!;

print "Writing $merged\n";
open (F, '>', $merged);
print F $xhtml;
close (F);
