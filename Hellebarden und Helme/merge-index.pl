use XML::LibXML;
my ($file, $index) = @ARGV;

my $parser = XML::LibXML->new();
print "Reading files\n";
my $file_doc = $parser->parse_html_file($file) or die "Cannot parse $file: $@\n";
my $index_doc = $parser->parse_html_file($index) or die "Cannot parse $file: $@\n";
