#!/usr/bin/perl

# Copyright (C) 2012  Alex Schroeder <alex@gnu.org>

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

use CGI qw(-utf8);
use XML::LibXML;
use LWP::UserAgent;
use utf8;

# see __DATA__ at the end of the file
my %Translation = map { chop($_); $_; } <DATA>;

# globals
my $q = new CGI;
my %char = ($q->Vars);
my ($lang) = $q->path_info =~ m!/(en|de)\b!;
$lang = "en" unless $lang;
my $filename = $char{charsheet} || T('Charactersheet.svg');
my $url = "http://campaignwiki.org/halberdsnhelmets";
my $email = "kensanata\@gmail.com";
my $author = "Alex Schroeder";
my $contact = "http://www.emacswiki.org/alex/Contact";
my $example = "$url/$lang?name=Tehah;class=Elf;level=1;xp=100;ac=9;hp=5;str=15;dex=9;con=15;int=10;wis=9;cha=7;breath=15;poison=12;petrify=13;wands=13;spells=15;property=Zauberbuch%20%28Gerdana%29%3a%5C%5C%E2%80%A2%20Einschl%C3%A4ferndes%20Rauschen;abilities=Ghinorisch%5C%5CElfisch;thac0=19";

sub T {
  my $en = shift;
  my $suffix;
  # handle (2) suffixes
  if ($en =~ /(.*)( \(\d+\))$/) {
    $en = $1;
    $suffix = $2;
  }
  if ($Translation{$en} and $lang eq "de") {
    $en = $Translation{$en};
  }
  # utf8::encode($en);
  for (my $i = 0; $i < scalar @_; $i++) {
    my $s = $_[$i];
    $en =~ s/%$i/$s/g;
  }
  return $en . $suffix;
}

sub footer {
  print $q->hr();
  print $q->p($q->a({-href=>$contact}, $author),
	      "<" . $q->a({-href=>"mailto:$email"}, $email) . ">",
	      $q->br(),
	      $q->a({-href=>$url . "/$lang"}, 'Character Sheet Generator'),
	      $q->a({-href=>$url . "/more/$lang"}, T('More')),
	      $q->a({-href=>$url . "/source"}, T('Source')),
	      ($lang eq "en"
	       ? $q->a({-href=>$url . "/de"}, T('German'))
	       : $q->a({-href=>$url . "/en"}, T('English'))));
}

sub error {
  my ($title, $text) = @_;
  print $q->header(-status=>"400 Bad request");
  print $q->start_html($title);
  print $q->h1($title);
  print $q->p($text);
  footer();
  print $q->end_html;
  exit;
}

sub header {
  my $header = shift;
  print $q->header(-charset=>"utf-8");
  binmode(STDOUT, ":utf8");
  print $q->start_html(T('Character Sheet Generator'));
  if ($header) {
    print $q->h1(T($header));
  } elsif (defined $header) {
    # '' = no header
  } else {
    print $q->h1(T('Character Sheet Generator'));
  }
}

sub svg_read {
  my $doc;
  my $parser = XML::LibXML->new();
  if (-f $filename) {
    open(my $fh, "<:utf8", $filename);
    $doc = $parser->parse_fh($fh);
    close($fh);
  } else {
    my $ua = LWP::UserAgent->new;
    my $response = $ua->get($filename);
    $response->is_success or error($response->status_line, $filename);
    $doc = $parser->parse_string($response->decoded_content);
  }
  return $doc;
}

sub svg_write {
  my $svg = shift;
  binmode(STDOUT, ":perlio");
  print $q->header(-type=>"image/svg+xml",
		   -charset=>"utf-8");
  print $svg->toString();
}

sub replace_text {
  my ($nodes, $str) = @_;
  # delete the text nodes of the tspan and add a new text node
  for my $node ($nodes->get_nodelist) {
    $node->removeChildNodes();
    $node->appendText(T($str));
  }
}

sub replace_multiline_text {
  my ($nodes, $str) = @_;
  my @line = split(/\\\\/, $str);

  # determine dy
  my $firstline = $nodes->get_node(1);
  my $secondline = $nodes->get_node(2);
  my $dy = $secondline->getAttribute("y") - $firstline->getAttribute("y");

  # get rid of the extra lines (tspan elements)
  my $parent = $firstline->parentNode;
  for (my $pos = 2; $pos <= $nodes->size(); $pos++) {
    $parent->removeChild($nodes->get_node($pos));
  }

  $firstline->removeChildNodes();
  $firstline->appendText(shift @line);

  my $tspan = $firstline;
  foreach my $line (@line) {
    $tspan = $tspan->cloneNode();
    $tspan->setAttribute("y", $tspan->getAttribute("y") + $dy);
    $tspan->appendText(T($line));
    $parent->appendChild($tspan);
  }
}

sub link_to {
  my $path = shift;
  my $link = $url;
  $link .= "/$path" if $path;
  $link .= "/$lang" if $lang;

  my $thac0 = defined $char{thac0};

  my @keys;
  foreach (keys %char) {
    next if $thac0 and /^(melee|range)\d+$/;
    next if /-bonus$/;
    next unless defined $char{$_};
    push(@keys, $_);
  }

  return "$link?"
    . join(";",
	   map { "$_=" . url_encode($char{$_}) }
	   @keys);
}

sub svg_transform {
  my $doc = shift;

  my $svg = XML::LibXML::XPathContext->new;
  $svg->registerNs("svg", "http://www.w3.org/2000/svg");

  for my $id (keys %char) {
    next unless $id =~ /^[-a-z0-9]+$/;
    my $nodes = $svg->find(qq{//svg:text[\@id="$id"]/svg:tspan}, $doc);

    if ($nodes->size() == 1) {
      replace_text($nodes, $char{$id});
    } elsif ($nodes->size() > 1) {
      replace_multiline_text($nodes, $char{$id});
    }
  }

  my $nodes = $svg->find(qq{//svg:a[\@id="link"]/attribute::xlink:href}, $doc);
  for my $node ($nodes->get_nodelist) {
    $node->setValue(link_to("link"));
  }
  return $doc;
}

sub bonus {
  my $n = shift;
  return "-3" if $n <=  3;
  return "-2" if $n <=  5;
  return "-1" if $n <=  8;
  return "" if $n <= 12;
  return "+1" if $n <= 15;
  return "+2" if $n <= 17;
  return "+3";
}

sub moldvay {
  for my $id (qw(str dex con int wis cha)) {
    if ($char{$id} and not $char{"$id-bonus"}) {
      $char{"$id-bonus"} =  bonus($char{$id});
    }
  }
  if (not $char{loyalty}) {
    $char{loyalty} =  7 + $char{"cha-bonus"};
  }
  if ($char{thac0}) {
    for (my $n = 0; $n <= 9; $n++) {
      my $val = $char{thac0} - $n - $char{"str-bonus"};
      $val = 20 if $val > 20;
      $val =  1 if $val <  1;
      $char{"melee$n"} =  $val unless $char{"melee$n"};
      $val = $char{thac0} - $n - $char{"dex-bonus"};
      $val = 20 if $val > 20;
      $val =  1 if $val <  1;
      $char{"range$n"} =  $val unless $char{"range$n"};
    }
  }
  if ($char{"str-bonus"} and not $char{damage}) {
    # hack alert!
    my $damage = 1 . T('d6');
    $char{damage} = $damage . $char{"str-bonus"}
      . " / " . $damage;
  }

  saves();
}

sub complete {
  my ($one, $two) = @_;
  if ($char{$one} and not $char{$two}) {
    if ($char{$one} > 20) {
      $char{$two} = 0;
    } else {
      $char{$two} = 20 - $char{$one};
    }
  }
}

sub pendragon {
  if ($char{str} and $char{siz} and not $char{damage}) {
    $char{damage} = int(($char{str}+$char{siz}) / 6 + 0.5) . T('d6');
  }
  if ($char{con} and $char{siz} and not $char{hp}) {
    $char{hp} = $char{con}+$char{siz};
  }
  if ($char{hp} and not $char{unconscious}) {
    $char{unconscious} = int($char{hp} / 4 + 0.5);
  }
  my @traits = qw(chaste lustful
		  energetic lazy
		  forgiving vengeful
		  generous selfish
		  honest deceitful
		  just arbitrary
		  merciful cruel
		  modest proud
		  pious worldly
		  prudent reckless
		  temperate indulgent
		  trusting suspicious
		  valorous cowardly);
  while (@traits) {
    my $one = shift(@traits);
    my $two = shift(@traits);
    complete($one, $two);
    complete($two, $one);
  }
}

sub compute_data {
  if ($char{rules} eq 'pendragon') {
    pendragon();
  } elsif ($char{rules} eq 'moldvay') {
    moldvay();
  } else {
    moldvay();
  }
}

sub equipment {
  my $xp = $char{xp};
  my $level = $char{level};
  my $class = $char{class};
  return if $xp or $level > 1 or not $class;

  my $money = roll_3d6() * 10;
  my @property = (T('(starting gold: %0)', $money));

  $money -= 20;
  push(@property, T('backpack'), T('iron rations (1 week)'));
  push(@property, T('spell book')) if $class eq T('magic-user');
  ($money, @property) = buy_armor($money, $class, @property);
  ($money, @property) = buy_weapon($money, $class, @property);
  ($money, @property) = buy_tools($money, $class, @property);
  ($money, @property) = buy_light($money, $class, @property);
  ($money, @property) = buy_gear($money, $class, @property);
  ($money, @property) = buy_protection($money, $class,
				       @property);
  push(@property, T('%0 gold', $money));
  $char{property} =  join("\\\\", @property);
}

sub buy_tools {
  my ($money, $class, @property) = @_;
  if ($class eq T('cleric')
      and $money >= 25) {
    $money -= 25;
    push(@property, T('holy symbol'));
  } elsif ($class eq T('thief')
      and $money >= 25) {
    $money -= 25;
    push(@property, T('thieves’ tools'));
  }
  return ($money, @property);
}

sub buy_light {
  my ($money, $class, @property) = @_;
  if ($money >= 12) {
    $money -= 12;
    push(@property, T('lantern'));
    push(@property, T('flask of oil'));
    if ($money >= 2) {
      $money -= 2;
      add(T('flask of oil'), @property);
    }
  } elsif ($money >= 1) {
    $money -= 1;
    push(@property, T('torches'));
  }
  return ($money, @property);
}

sub buy_gear {
  my ($money, $class, @property) = @_;
  %price = (T('rope') => 1,
	    T('iron spikes and hammer') => 3,
	    T('wooden pole') => 1);
  my $item = one(affordable($money, %price));

  if ($item and $money >= $price{$item}) {
    $money -= $price{$item};
    push(@property, $item);
  }
  return ($money, @property);
}

sub buy_protection {
  my ($money, $class, @property) = @_;
  %price = (T('holy water') => 25,
	    T('wolfsbane') => 10,
	    T('mirror') => 5);
  my $item = one(affordable($money, %price));

  if ($item and $money >= $price{$item}) {
    $money -= $price{$item};
    push(@property, $item);
  }
  return ($money, @property);
}

sub buy_armor {
  my ($money, $class, @property) = @_;
  my $budget = $money / 2;
  $budget = int($budget / 10) * 10;
  $money -= $budget;

  my $dex = $char{dex};
  my $ac = 9 - bonus($dex);

  if ($class ne T('magic-user')
      and $class ne T('thief')
      and $budget >= 60) {
    $budget -= 60;
    push(@property, T('plate mail'));
    $ac -= 6;
  } elsif ($class ne T('magic-user')
      and $class ne T('thief')
      and $budget >= 40) {
    $budget -= 40;
    push(@property, T('chain mail'));
    $ac -= 4
  } elsif ($class ne T('magic-user')
      and $budget >= 20) {
    $budget -= 20;
    push(@property, T('leather armor'));
    $ac -= 2;
  }

  if ($class ne T('magic-user')
      and $class ne T('thief')
      and $budget >= 10) {
    $budget -= 10;
    push(@property, T('shield'));
    $ac -= 1;
  }

  if ($class ne T('magic-user')
      and $class ne T('thief')
      and $budget >= 10) {
    $budget -= 10;
    push(@property, T('helmet'));
  }

  $char{ac} =  $ac;

  $money += $budget;

  return ($money, @property);
}

sub one {
  my $i = int(rand(scalar @_));
  return $_[$i];
}

sub affordable {
  my ($money, %price) = @_;
  foreach my $item (keys %price) {
    if ($price{$item} > $money) {
      delete $price{$item};
    }
  }
  return keys %price;
}

sub member {
  my $element = shift;
  foreach (@_) {
    return 1 if $element eq $_;
  }
}

sub buy_weapon {
  my ($money, $class, @property) = @_;
  my $budget = $money / 2;
  $money -= $budget;

  my $str = $char{str};
  my $dex = $char{dex};
  my $hp  = $char{hp};
  my $shield = member(T('shield'), @property);

  if ($class eq T('cleric')) {
    if ($budget >= 5) {
      $budget -= 5;
      push(@property, one(T('mace'), T('war hammer')));
    } elsif ($budget >= 3) {
      $budget -= 3;
      push(@property, one(T('club'), T('staff')));
    }
  } elsif ($class eq T('magic-user')
      and $budget >= 3) {
    $budget -= 3;
    push(@property, T('dagger'));
  } elsif ($class eq T('fighter')
	   and good($str)
	   and $hp > 6
	   and not $shield
	   and $budget >= 15) {
    $budget -= 15;
    push(@property, T('two handed sword'));
  } elsif ($class eq T('fighter')
	   and good($str)
	   and $hp > 6
	   and not $shield
	   and $budget >= 7) {
    $budget -= 7;
    push(@property, T('battle axe'));
  } elsif ($class eq T('fighter')
	   and average($str)
	   and not $shield
	   and $budget >= 7) {
    $budget -= 7;
    push(@property, T('pole arm'));
  } elsif ($class eq T('dwarf')
	   and not $shield
	   and $budget >= 7) {
    $budget -= 7;
    push(@property, T('battle axe'));
  } elsif ($budget >= 10
	   and d6() > 1) {
    $budget -= 10;
    push(@property, T('long sword'));
  } elsif ($budget >= 7) {
    $budget -= 7;
    push(@property, T('short sword'));
  }

  if (($class eq T('fighter') or $class eq T('elf'))
      and average($dex)
      and $budget >= 45) {
    $budget -= 45;
    push(@property, T('long bow'));
    push(@property, T('quiver of arrows'));
    if ($budget >= 5) {
      $budget -= 5;
      add(T('quiver of arrows'), @property);
    }
  } elsif (($class ne T('cleric') and $class ne T('magic-user'))
      and average($dex)
      and $budget >= 30) {
    $budget -= 30;
    push(@property, T('short bow'));
    push(@property, T('quiver of arrows'));
    if ($budget >= 5) {
      $budget -= 5;
      add(T('quiver of arrows'), @property);
    }
  } elsif (($class ne T('cleric') and $class ne T('magic-user'))
      and $budget >= 40) {
    $budget -= 40;
    push(@property, T('crossbow'));
    push(@property, T('case of bolts'));
    if ($budget >= 10) {
      $budget -= 10;
      add(T('case of bolts'), @property);
    }
  } elsif ($class ne T('magic-user')
      and $budget >= 2) {
    $budget -= 2;
    push(@property, T('sling'));
    push(@property, T('pouch of stones'));
    if ($budget >= 2) {
      $budget -= 2;
      add(T('pouch of stones'), @property);
    }
  }

  if (($class eq T('dwarf') or member(T('battle axe'), @property))
      and $budget >= 4) {
    $budget -= 4;
    push(@property, T('hand axe'));
    if ($budget >= 4) {
      $budget -= 4;
      add(T('hand axe'), @property);
    }
  } elsif ($class eq T('fighter')
	   and $budget >= 3) {
    $budget -= 3;
    push(@property, T('spear'));
  }

  if ($class ne T('cleric')
      and $budget >= 30) {
    $budget -=30;
    push(@property, T('silver dagger'));
  }

  if ($class ne T('cleric')
      and $class ne T('magic-user')
      and $budget >= 3) {
    $budget -=3;
    push(@property, T('dagger'));
    if ($budget >= 3) {
      $budget -=3;
      add(T('dagger'), @property);
    }
  }

  $money += $budget;

  return ($money, @property);
}

sub add {
  my $item = shift;
  foreach (@_) {
    if ($_ eq $item) {
      if (/\(\d+\)$/) {
	my $n = $1++;
	s/\(\d+\)$/($n)/;
      } else {
	$_ .= " (2)";
      }
      last;
    }
  }
}

sub saves {
  my $class = $char{class};
  my $level = $char{level};
  return unless $class and $level >= 1 and $level <= 3;
  my ($breath, $poison, $petrify, $wands, $spells);
  if ($class eq T('cleric')) {
    ($breath, $poison, $petrify, $wands, $spells) =
      (16, 11, 14, 12, 15);
  } elsif ($class eq T('dwarf') or $class eq T('halfling')) {
    ($breath, $poison, $petrify, $wands, $spells) =
      (13, 8, 10, 9, 12);
  } elsif ($class eq T('elf')) {
    ($breath, $poison, $petrify, $wands, $spells) =
      (15, 12, 13, 13, 15);
  } elsif ($class eq T('fighter')) {
    ($breath, $poison, $petrify, $wands, $spells) =
      (15, 12, 14, 13, 16);
  } elsif ($class eq T('magic-user')) {
    ($breath, $poison, $petrify, $wands, $spells) =
      (16, 13, 13, 13, 14);
  } elsif ($class eq T('thief')) {
    ($breath, $poison, $petrify, $wands, $spells) =
      (16, 14, 13, 15, 13);
  }

  $char{breath} =  $breath;
  $char{poison} =  $poison;
  $char{petrify} =  $petrify;
  $char{wands} =  $wands;
  $char{spells} =  $spells;
}

sub svg_show_id {
  my $doc = shift;

  my $svg = XML::LibXML::XPathContext->new;
  $svg->registerNs("svg", "http://www.w3.org/2000/svg");

  for my $node ($svg->find(qq{//svg:text/svg:tspan/..}, $doc)->get_nodelist) {
    my $id = $node->getAttribute("id");
    next if $id =~ /^text[0-9]+(-[0-9]+)*$/; # skip Inkscape default texts
    next unless $id =~ /^[-a-z0-9]+$/;
    $node->removeChildNodes();
    $node->appendText($id);
    my $style = $node->getAttribute("style");
    $style =~ s/font-size:\d+px/font-size:8px/;
    $style =~ s/fill:#\d+/fill:magenta/;
    $node->setAttribute("style", $style);
  }

  return $doc;
}

sub d4 {
  return 1 + int(rand(4));
}

sub d6 {
  return 1 + int(rand(6));
}

sub d8 {
  return 1 + int(rand(8));
}

sub roll_3d6 {
  return d6() + d6() + d6();
}

sub best {
  my $best = 0;
  my $max = $_[0];
  for (my $i = 1; $i < 6; $i++) {
    if ($_[$i] > $max) {
      $best = $i;
      $max = $_[$best];
    }
  }
  my @stat = qw(str dex con int wis cha);
  return $stat[$best];
}

sub above {
  my $limit = shift;
  my $n = 0;
  for (my $i = 0; $i <= $#_; $i++) {
    $n++ if $_[$i] > $limit;
  }
  return $n;
}

sub good {
  return above(12, @_);
}

sub average {
  return above(8, @_);
}

sub random_parameters {
  my ($str, $dex, $con, $int, $wis, $cha) =
    (roll_3d6(), roll_3d6(), roll_3d6(),
     roll_3d6(), roll_3d6(), roll_3d6());

  $char{str} = $str;
  $char{dex} =  $dex;
  $char{con} =  $con;
  $char{int} =  $int;
  $char{wis} =  $wis;
  $char{cha} =  $cha;

  $char{level} =  "1";
  $char{xp} =  "0";
  $char{thac0} =  19;

  my $class = $char{class};
  my $best = best($str, $dex, $con, $int, $wis, $cha);

  if (not $class) {
    if (average($con) and $best eq "str") {
      $class = T('dwarf');
    } elsif (average($int)
	     and good($str, $dex)
	     and d6() > 2) {
      $class = T('elf');
    } elsif (average($str, $dex, $con) == 3
	     and good($str, $dex, $con)
	     and d6() > 2) {
      $class = T('halfling');
    } elsif (average($str, $dex, $con) >= 2
	     and ($best eq "str" or $best eq "con")
	     or good($str, $dex, $con) >= 2) {
      $class = T('fighter');
    } elsif ($best eq "int") {
      $class = T('magic-user');
    } elsif (($best eq "wis" or $best eq "cha")
	     and d6() > 2) {
      $class = T('cleric');
    } elsif ($best eq "dex") {
      $class = T('thief');
    } else {
      $class = one(T('cleric'), T('magic-user'), T('fighter'), T('thief'));
    }
  }

  $char{class} =  $class;

  my $hp = $char{hp};
  if (not $hp) {

    if ($class eq T('fighter') or $class eq T('dwarf')) {
      $hp = d8();
    } elsif ($class eq T('magic-user') or $class eq T('thief')) {
      $hp = d4();
    } else {
      $hp = d6();
    }

    $hp += bonus($con);
    $hp = 1 if $hp < 1;
  }

  $char{hp} =  $hp;

  my $abilities = T('1/6 for normal tasks');
  if ($class eq T('elf')) {
    $abilities .= "\\\\" . T('2/6 to hear noise');
    $abilities .= "\\\\" . T('2/6 to find secret or concealed doors');
  } elsif ($class eq T('dwarf')) {
    $abilities .= "\\\\" . T('2/6 to hear noise');
    $abilities .= "\\\\" . T('2/6 to find secret constructions and traps');
  } elsif ($class eq T('halfling')) {
    $abilities .= "\\\\" . T('2/6 to hear noise');
    $abilities .= "\\\\" . T('2/6 to hide and sneak');
    $abilities .= "\\\\" . T('5/6 to hide and sneak outdoors');
    $abilities .= "\\\\" . T('+1 bonus to ranged weapons');
    $abilities .= "\\\\" . T('AC -2 vs. opponents larger than humans');
  } elsif ($class eq T('thief')) {
    $abilities .= "\\\\" . T('2/6 to hear noise');
    $abilities .= "\\\\" . T('+4 to hit and double damage backstabbing');
  } elsif ($class eq T('magic-user')) {
    $abilities .= "\\\\" . T('Spells:') . " "
      . one(T('charm person'),
	    T('detect magic'),
	    T('floating disc'),
	    T('hold portal'),
	    T('light'),
	    T('magic missile'),
	    T('protection from evil'),
	    T('read languages'),
	    T('read magic'),
	    T('shield'),
	    T('sleep'),
	    T('ventriloquism'));
  }
  push(@property, T('spell book')) ;

  $char{abilities} = $abilities;

  equipment();
}

sub characters {
  header('');
  for (my $i = 0; $i < 50; $i++) {
    $q->delete_all();
    %char = ();
    print $q->start_pre({-style=>'display: inline-block; padding: 0 1em; width: 25em; border-left: 1px dotted grey; vertical-align: top; font-size: 8pt; '});
    random_parameters();
    print "Str Dex Con Int Wis Cha HP AC Class\n";
    printf "%3d", $char{"str"};
    printf " %3d", $char{dex};
    printf " %3d", $char{con};
    printf " %3d", $char{int};
    printf " %3d", $char{wis};
    printf " %3d", $char{cha};
    printf " %2d", $char{hp};
    printf " %2d", $char{ac};
    print " " . $char{class};
    print "\n";
    print map { "  $_\n" }
      split(/\\\\/, $char{property});
    print $q->end_pre();
  }
  print $q->div({-style=>'clear: both;'});
  footer();
}

sub stats {
  print $q->header(-type=>"text/plain",
		   -charset=>"utf8");
  binmode(STDOUT, ":utf8");

  my (%class, %property);
  for (my $i = 0; $i < 1000; $i++) {
    $q = new CGI;
    random_parameters();
    $class{$char{class}}++;
    foreach (split(/\\\\/, $char{property})) {
      $property{$_}++;
    }
  }
  my $n;

  print "Classes\n";
  foreach (sort { $class{$b} <=> $class{$a} } keys %class) {
    printf "%25s %4d\n", $_, $class{$_};
    $n += $class{$_};
  }
  printf "%25s %4d\n", "total", $n;

  print "Property\n";
  foreach (sort { $property{$b} <=> $property{$a} }
	   keys %property) {
    next if /starting gold:/ or /gold$/;
    next if /Startgold:/ or /Gold$/;
    printf "%25s %4d\n", $_, $property{$_};
  }
}

sub translation {
  print $q->header(-type=>"text/plain",
		   -charset=>"utf-8");
  binmode(STDOUT, ":utf8");

  my $str = source();
  my %data;
  while ($str =~ /'(.+?)'/g) {
    next if $1 eq "(.+?)";
    $data{$1} = $Translation{$1};
  }
  foreach (sort keys %data) {
    print "$_\n";
    print $Translation{$_} . "\n";
  }
  foreach (sort keys %Translation) {
    if (not exists $data{$_}) {
      print "$_\n";
      print "NOT USED: " . $Translation{$_} . "\n";
    }
  }
}

sub show_link {
  header();
  print $q->h2(T('Bookmark'));
  print $q->p(T('Bookmark the following link to your %0.',
		$q->a({-href=>link_to()},
		      T('Character Sheet'))));
  print $q->h2(T('Edit'));
  print $q->p(T('Use the following form to make changes to your character sheet.'),
	      T('You can also copy and paste it on to a %0 page to generate an inline character sheet.',
		$q->a({-href=>"http://campaignwiki.org/"}, "Campaign Wiki")));
  my $str = T('Character:') . "\n";
  my $rows;
  for my $key (keys %char) {
    for my $val (split(/\\\\/, $char{$key})) {
      # utf8::decode($val);
      $str .= "$key: $val\n";
      $rows++;
    }
  }
  print $q->start_form(-method=>"get", -action=>"$url/redirect/$lang", -accept_charset=>"UTF-8");
  print $q->textarea(-name    => "input",
		     -default => $str,
		     -rows    => $rows + 3,
		     -columns => 55);
  print $q->p($q->submit);
  print $q->end_form;
  footer();
}

sub source {
  seek DATA, 0, 0;
  undef $/;
  my $str = <DATA>;
  return $str;
}

sub default {
  header();
  print $q->p(T('This is the %0 character sheet generator.',
		$q->a({-href=>T('http://campaignwiki.org/wiki/Halberds%C2%A0and%C2%A0Helmets/')},
		      T('Halberts and Helmets'))));
  print $q->start_form(-method=>"get", -action=>"$url/random/$lang", -accept_charset=>"UTF-8"),
    T('Name:'), " ", $q->textfield("name"), " ", $q->submit, $q->end_form;
  print $q->p(T('The character sheet contains a link in the bottom right corner which allows you to bookmark and edit your character.'));
  footer();
  print $q->end_html;
}

sub more {
  header();
  print $q->p(T('The generator works by using a template (%0) and replacing some placeholders.',
		$q->a({-href=>"/" . T('Charactersheet.svg')}, T('Charactersheet.svg'))),
	      T('The template uses the %0 font.',
		$q->a({-href=>"/Purisa.ttf"}, "Purisa")),
	      T('You provide values for the placeholders by providing URL parameters (%0).',
		$q->a({-href=>$example}, T('example'))),
	      T('The script can also show %0.',
		$q->a({-href=>"$url/show/$lang"},
		      T('which parameters go where'))),
	      T('Also note that the parameters need to be UTF-8 encoded.'),
	      T('If the template contains a multiline placeholder, the parameter may also provide multiple lines separated by two backslashes.'));
  print $q->p(T('In addition to that, some parameters are computed unless provided:'));
  my @doc = qw(str str-bonus
	       dex dex-bonus
	       con con-bonus
	       int int-bonus
	       wis wis-bonus
	       cha cha-bonus
	       cha-bonus loyalty
	       str-bonus damage
	       thac0 melee0-9&nbsp;&amp;&nbsp;range0-9);
  print "<ul>";
  while (@doc) {
    print $q->li(shift(@doc), "&rarr;", shift(@doc));
  }
  print "</ul>";
  print $q->p(T('The script can also generate a %0, a %1, or %2.',
		$q->a({-href=>"$url/random/$lang"}, T('random character')),
		$q->a({-href=>"$url/characters/$lang"}, T('bunch of characters')),
		$q->a({-href=>"$url/stats/$lang"}, T('some statistics'))));
  footer();
}

sub url_encode {
  my $str = shift;
  return '' unless defined $str;
  utf8::encode($str);
  my @letters = split(//, $str);
  my %safe = map {$_ => 1} ("a" .. "z", "A" .. "Z", "0" .. "9", "-", "_", ".", "!", "~", "*", "\"", "(", ")", "#");
  foreach my $letter (@letters) {
    $letter = sprintf("%%%02x", ord($letter)) unless $safe{$letter};
  }
  return join('', @letters);
}

sub redirect {
  $_ = $char{input};
  my @param;
  my $last;
  while (/^([-a-z0-9]*): *(.*?)\r$/gm) {
    if ($1 eq $last or $1 eq "") {
      $param[$#param] .= "\\\\" . url_encode($2);
    } else {
      push(@param, $1 . "=" . url_encode($2));
      $last = $1;
    }
  }
  print $q->redirect("$url/$lang?" . join(";", @param));
}

sub main {
  if ($q->path_info eq "/source") {
    print "Content-type: text/plain; charset=UTF-8\r\n\r\n",
      source();
  } elsif ($q->path_info =~ m!/show\b!) {
    svg_write(svg_show_id(svg_read()));
  } elsif ($q->path_info =~ m!/random\b!) {
    random_parameters();
    compute_data();
    svg_write(svg_transform(svg_read()));
  } elsif ($q->path_info =~ m!/characters\b!) {
    characters();
  } elsif ($q->path_info =~ m!/translation\b!) {
    translation();
  } elsif ($q->path_info =~ m!/stats\b!) {
    stats();
  } elsif ($q->path_info =~ m!/link\b!) {
    show_link();
  } elsif ($q->path_info =~ m!/redirect\b!) {
    redirect();
  } elsif ($q->path_info =~ m!/more\b!) {
    more();
  } elsif (%char) {
    compute_data();
    svg_write(svg_transform(svg_read()));
  } else {
    default();
  }
}

main();

# strings in sinqle quotes are translated into German if necessary
# use %0, %1, etc. for parameters

__DATA__
%0 gold
%0 Gold
(starting gold: %0)
(Startgold: %0)
+1 bonus to ranged weapons
+1 für Fernwaffen
+4 to hit and double damage backstabbing
+4 und Schaden ×2 für hinterhältigen Angriff
d6
W6
AC -2 vs. opponents larger than humans
Rüstung -2 bei Gegnern über Menschengrösse
Also note that the parameters need to be UTF-8 encoded.
Die Parameter müssen UTF-8 codiert sein.
Bookmark
Lesezeichen
Bookmark the following link to your %0.
Den Charakter kann man einfach aufbewahren, in dem man sich das %0 als Lesezeichen speichert.
Character Sheet
Charakterblatt
Character Sheet Generator
Charakterblatt Generator
Character:
Charakter:
Charactersheet.svg
Charakterblatt.svg
Edit
Bearbeiten
English
Englisch
German
Deutsch
Halberts and Helmets
Hellebarden und Helme
If the template contains a multiline placeholder, the parameter may also provide multiple lines separated by two backslashes.
Die Vorlage kann auch mehrzeilige Platzhalter enthalten. Der entsprechende Parameter muss die Zeilen dann durch doppelte Backslashes trennen.
In addition to that, some parameters are computed unless provided:
Zudem werden einige Parameter berechnet, sofern sie nicht angegeben wurden:
More
Weitere Informationen
Name:
Name:
Source
Quellcode
Spells:
Zaubersprüche:
The character sheet contains a link in the bottom right corner which allows you to bookmark and edit your character.
Auf dem generierten Charakterblatt hat es unten rechts einen Link mit dem man sich ein Lesezeichen erstellen kann und wo der Charakter bearbeitet werden kann.
The generator works by using a template (%0) and replacing some placeholders.
Das funktioniert über eine Vorlage (%0) und dem Ersetzen von Platzhaltern.
The script can also generate a %0, a %1, or %2.
Das Skript kann auch %0, %1 oder %2 generieren.
The script can also show %0.
Das Skript kann auch %0 zeigen.
The template uses the %0 font.
Die Vorlage verwendet die Schriftart %0.
This is the %0 character sheet generator.
Dies ist der %0 Charaktergenerator.
Use the following form to make changes to your character sheet.
Mit dem folgenden Formular lassen sich leicht Änderungen am Charakter machen.
You can also copy and paste it on to a %0 page to generate an inline character sheet.
Man kann diesen Text auch auf einer %0 Seite verwenden, um das Charakterblatt einzufügen.
You provide values for the placeholders by providing URL parameters (%0).
Den Platzhaltern werden über URL Parameter Werte zugewiesen (%0).
backpack
Rucksack
battle axe
Streitaxt
bunch of characters
einige Charaktere
case of bolts
Kiste mit Bolzen
chain mail
Kettenhemd
charm person
Person bezaubern
cleric
Kleriker
club
Keule
crossbow
Armbrust
dagger
Dolch
detect magic
Magie entdecken
dwarf
Zwerg
elf
Elf
example
Beispiel
fighter
Krieger
flask of oil
Ölflasche
floating disc
Schwebende Scheibe
halfling
Halbling
hand axe
Handaxt
helmet
Helm
hold portal
Portal verschliessen
holy symbol
Heiliges Symbol
holy water
Weihwasser
http://campaignwiki.org/wiki/Halberds%C2%A0and%C2%A0Helmets/
http://campaignwiki.org/wiki/Hellebarden%C2%A0und%C2%A0Helme/
iron rations (1 week)
Feldrationen (1 Woche)
iron spikes and hammer
Eisenkeile und Hammer
lantern
Laterne
leather armor
Lederrüstung
light
Licht
long bow
Langbogen
long sword
Langschwert
mace
Streitkeule
magic missile
Magisches Geschoss
magic-user
Magier
mirror
Spiegel
plate mail
Plattenpanzer
pole arm
Stangenwaffe
pouch of stones
Beutel mit Steinen
protection from evil
Schutz vor Bösem
quiver of arrows
Köcher mit Pfeilen
random character
einen zufälligen Charakter
read languages
Sprachen lesen
read magic
Magie lesen
rope
Seil
shield
Schild
short bow
Kurzbogen
short sword
Kurzschwert
silver dagger
Silberner Dolch
sleep
Schlaf
sling
Schleuder
some statistics
Statistiken
spear
Speer
spell book
Zauberbuch
staff
Stab
thief
Dieb
thieves’ tools
Diebeswerkzeug
torches
Fackeln
two handed sword
Zweihänder
ventriloquism
Bauchreden
war hammer
Kriegshammer
which parameters go where
welche Parameter wo erscheinen
wolfsbane
Eisenhut (sog. Wolfsbann)
wooden pole
Holzstab
⚀ for normal tasks
⚀ für normale Aufgaben
⚁ to find secret constructions and traps
⚁ um Geheimbauten und Fallen zu finden
⚁ to find secret or concealed doors
⚁ um geheime und versteckte Türen zu finden
⚁ to hear noise
⚁ um Geräusche zu hören
⚁ to hide and sneak
⚁ für Verstecken und Schleichen
⚄ to hide and sneak outdoors
⚄ für Verstecken und Schleichen im Freien

