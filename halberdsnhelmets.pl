#!/usr/bin/perl

# Copyright (C) 2012-2013  Alex Schroeder <alex@gnu.org>

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
my @provided = $q->param;
my ($lang) = $q->path_info =~ m!/(en|de)\b!;
$lang = "en" unless $lang;
my $filename = $char{charsheet} || T('Charactersheet.svg');
my $url = "http://campaignwiki.org/halberdsnhelmets";
my $email = "kensanata\@gmail.com";
my $author = "Alex Schroeder";
my $contact = "http://www.emacswiki.org/alex/Contact";
my $example = "$url/$lang?name=Tehah;class=Elf;level=1;xp=100;ac=9;hp=5;str=15;dex=9;con=15;int=10;wis=9;cha=7;breath=15;poison=12;petrify=13;wands=13;spells=15;property=Zauberbuch%20%28Gerdana%29%3a%5C%5C%E2%80%A2%20Einschl%C3%A4ferndes%20Rauschen;abilities=Ghinorisch%5C%5CElfisch;thac0=19";
my $parser;

sub T {
  my ($en, @arg) = @_;
  my $suffix = '';
  # handle (2) suffixes
  if ($en =~ /(.*)( \(\d+\))$/) {
    $en = $1;
    $suffix = $2;
  }
  if ($Translation{$en} and $lang eq "de") {
    $en = $Translation{$en};
  }
  # utf8::encode($en);
  for (my $i = 0; $i < scalar @arg; $i++) {
    my $s = $arg[$i];
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
	      $q->a({-href=>$url . "/help/$lang"}, T('Help')),
	      $q->a({-href=>$url . "/source"}, T('Source')),
	      $q->a({-href=>"https://github.com/kensanata/halberdsnhelmets"}, T("GitHub")),
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
  my ($node, $str) = @_;
  my @line = split(/\\\\/, $str);

  # is this multiline in the template
  # (ignore text nodes, go for tspans only)
  my $dy;
  my $tspans = $node->find(qq{svg:tspan});
  if ($tspans->size() > 1) {

    $dy = $tspans->get_node(2)->getAttribute("y")
      - $tspans->get_node(1)->getAttribute("y");
  } else {
    # mismatch, attempt readable compromise
    @line = (join(", ", @line));
  }

  # delete the tspan nodes of the text node
  $node->removeChildNodes();

  $parser = XML::LibXML->new() unless $parser;

  my $tspan = XML::LibXML::Element->new("tspan");
  $tspan->setAttribute("x", $node->getAttribute("x"));
  $tspan->setAttribute("y", $node->getAttribute("y"));

  while (my $line = shift(@line)) {
    $fragment = $parser->parse_balanced_chunk(T($line));
    foreach my $child ($fragment->childNodes) {
      my $tag = $child->nodeName;
      if ($tag eq "strong" or $tag eq "b") {
	my $node = XML::LibXML::Element->new("tspan");
	$node->setAttribute("style", "font-weight:bold");
	$node->appendText($child->textContent);
	$tspan->appendChild($node);
      } elsif ($tag eq "a") {
	$child->setAttributeNS("http://www.w3.org/1999/xlink", "xlink:href",
			       $child->getAttribute("href"));
	$child->removeAttribute("href");
	$tspan->appendChild($child);
      } else {
	$tspan->appendText($child->textContent);
      }
    }
    $node->appendChild($tspan);
    if (@line) {
      $tspan = $tspan->cloneNode();
      $tspan->setAttribute("y", $tspan->getAttribute("y") + $dy);
    }
  }
}

sub link_to {
  my $path = shift;
  my $link = $url;
  $link .= "/$path" if $path;
  $link .= "/$lang" if $lang;

  return "$link?"
    . join(";",
	   map { "$_=" . url_encode($char{$_}) }
	   @provided);
}

sub svg_transform {
  my $doc = shift;

  my $svg = XML::LibXML::XPathContext->new;
  $svg->registerNs("svg", "http://www.w3.org/2000/svg");

  for my $id (keys %char) {
    next unless $id =~ /^[-a-z0-9]+$/;
    my $nodes = $svg->find(qq{//svg:text[\@id="$id"]}, $doc);
    for my $node ($nodes->get_nodelist) {
      replace_text($node, $char{$id}, $doc);
      next;
    }
    $nodes = $svg->find(qq{//svg:image[\@id="$id"]}, $doc);
    for my $node ($nodes->get_nodelist) {
      $node->setAttributeNS("http://www.w3.org/1999/xlink",
			    "xlink:href", $char{$id});
      next;
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

sub cha_bonus {
  my $n = shift;
  return "-2" if $n <=  3;
  return "-1" if $n <=  8;
  return "" if $n <= 12;
  return "+1" if $n <= 17;
  return "+2";
}

sub moldvay {
  for my $id (qw(str dex con int wis cha)) {
    if ($char{$id} and not $char{"$id-bonus"}) {
      $char{"$id-bonus"} = bonus($char{$id});
    }
  }
  if ($char{cha} and not $char{reaction}) {
    $char{reaction} = cha_bonus($char{cha});
  }
  if (not $char{loyalty}) {
    $char{loyalty} =  7 + $char{"cha-bonus"};
  }
  if (not $char{hirelings}) {
    $char{hirelings} =  4 + $char{"cha-bonus"};
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

sub crypt_bonus {
  my $n = shift;
  return "-1" if $n <=  8;
  return "" if $n <= 12;
  return "+1" if $n <= 15;
  return "+2" if $n <= 17;
  return "+3";
}

sub crypts_n_things {
  if ($char{str} and not $char{"to-hit"}) {
    $char{"to-hit"} = crypt_bonus($char{str});
  }
  if ($char{str} and not $char{"damage-bonus"}) {
    $char{"damage-bonus"} = crypt_bonus($char{str});
  }
  if ($char{dex} and not $char{"missile-bonus"}) {
    $char{"missile-bonus"} = crypt_bonus($char{dex});
  }
  if ($char{dex} and not $char{"ac-bonus"}) {
    $char{"ac-bonus"} = crypt_bonus($char{dex});
  }
  if ($char{con} and not $char{"con-bonus"}) {
    $char{"con-bonus"} = crypt_bonus($char{con});
  }
  if ($char{int} and not $char{understand}) {
    if ($char{int} <= 7) {
      $char{understand} = "0%";
    } elsif ($char{int} <= 9) {
      $char{understand} = 5 * ($char{int} - 7) . "%";
    } elsif ($char{int} <= 16) {
      $char{understand} = 5 * ($char{int} - 6) . "%";
    } else {
      $char{understand} = 15 * ($char{int} - 13) . "%";
    }
  }
  if ($char{cha} and not $char{charm}) {
    if ($char{cha} <= 4) {
      $char{charm} = "10%";
    } elsif ($char{cha} <= 6) {
      $char{charm} = "20%";
    } elsif ($char{cha} <= 8) {
      $char{charm} = "30%";
    } elsif ($char{cha} <= 12) {
      $char{charm} = "40%";
    } elsif ($char{cha} <= 15) {
      $char{charm} = "50%";
    } elsif ($char{cha} <= 17) {
      $char{charm} = "60%";
    } elsif ($char{cha} <= 18) {
      $char{charm} = "75%";
    }
  }
  if ($char{cha} and not $char{hirelings}) {
    if ($char{cha} <= 4) {
      $char{hirelings} = 1;
    } elsif ($char{cha} <= 6) {
      $char{hirelings} = 2;
    } elsif ($char{cha} <= 8) {
      $char{hirelings} = 3;
    } elsif ($char{cha} <= 12) {
      $char{hirelings} = 4;
    } elsif ($char{cha} <= 15) {
      $char{hirelings} = 5;
    } elsif ($char{cha} <= 17) {
      $char{hirelings} = 6;
    } elsif ($char{cha} <= 18) {
      $char{hirelings} = 7;
    }
  }
  if ($char{wis} and not $char{sanity}) {
    $char{sanity} = $char{wis};
  }
}

sub pendragon {
  if ($char{str} and $char{siz} and not $char{damage}) {
    $char{damage} = int(($char{str}+$char{siz}) / 6 + 0.5) . T('d6');
  }
  if ($char{str} and $char{con} and not $char{healing}) {
    $char{healing} = int(($char{str}+$char{con}) / 10 + 0.5);
  }
  if ($char{str} and $char{dex} and not $char{move}) {
    $char{move} = int(($char{str}+$char{dex}) / 10 + 0.5);
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

sub acks {
  for my $id (qw(str dex con int wis cha)) {
    if ($char{$id} and not $char{"$id-bonus"}) {
      $char{"$id-bonus"} = bonus($char{$id});
    }
  }
  if ($char{attack} and not $char{melee}) {
    $char{melee} =  $char{attack} + $char{"str-bonus"};
  }
  if ($char{attack} and not $char{missile}) {
    $char{missile} =  $char{attack} + $char{"dex-bonus"};
  }
}

sub compute_data {
  if (not exists $char{rules} or not defined $char{rules}) {
    moldvay();
  } elsif ($char{rules} eq "pendragon") {
    pendragon();
  } elsif ($char{rules} eq "moldvay") {
    moldvay();
  } elsif ($char{rules} eq "labyrinth lord") {
    moldvay();
  } elsif ($char{rules} eq "crypts-n-things") {
    crypts_n_things();
  } elsif ($char{rules} eq "acks") {
    acks();
  } else {
    moldvay();
  }
}

sub equipment {
  my $xp = $char{xp};
  my $level = $char{level};
  my $class = $char{class};
  return if $xp or $level > 1 or not $class;

  my $money = 0;
  if ($char{rules} eq "labyrinth lord") {
    $money += roll_3d8() * 10;
  } else {
    $money += roll_3d6() * 10;
  }
  my @property = (T('(starting gold: %0)', $money));

  push(@property, T('spell book')) if $class eq T('magic-user');

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
  provide("property",  join("\\\\", @property));
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

  my ($leather, $chain, $plate);
  if ($char{rules} eq "labyrinth lord") {
    ($leather, $chain, $plate) = (6, 70, 450);
  } else {
    ($leather, $chain, $plate) = (20, 40, 60);
  }

  if ($class ne T('magic-user')
      and $class ne T('thief')
      and $budget >= $plate) {
    $budget -= $plate;
    push(@property, T('plate mail'));
    $ac -= 6;
  } elsif ($class ne T('magic-user')
      and $class ne T('thief')
      and $budget >= $chain) {
    $budget -= $chain;
    push(@property, T('chain mail'));
    $ac -= 4
  } elsif ($class ne T('magic-user')
      and $budget >= $leather) {
    $budget -= $leather;
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

  provide("ac",  $ac);

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

  my ($club, $mace, $warhammer, $staff, $dagger, $twohanded, $battleaxe,
      $polearm, $longsword, $shortsword);

  if ($char{rules} eq "labyrinth lord") {
    ($club, $mace, $warhammer, $staff, $dagger, $twohanded, $battleaxe,
     $polearm, $longsword, $shortsword) =
       (3, 5, 7, 2, 3, 15, 6, 7, 10, 7);
  } else {
    ($club, $mace, $warhammer, $staff, $dagger, $twohanded, $battleaxe,
     $polearm, $battleaxe, $longsword, $shortsword) =
       (3, 5, 5, undef, 3, 15, 7, 7, 10, 7);
  }

  if ($class eq T('cleric')) {
    if ($mace == $warhammer && $budget >= $mace) {
      $budget -= $mace;
      push(@property, one(T('mace'), T('war hammer')));
    } elsif ($budget >= $mace) {
      $budget -= $mace;
      push(@property, T('mace'));
    } elsif ($staff == $club && $budget >= $club) {
      $budget -= $club;
      push(@property, one(T('club'), T('staff')));
    } elsif ($budget >= $club) {
      $budget -= $club;
      push(@property, T('staff'));
    }
  } elsif ($class eq T('magic-user')
      and $budget >= $dagger) {
    $budget -= $dagger;
    push(@property, T('dagger'));
  } elsif ($class eq T('fighter')
	   and good($str)
	   and $hp > 6
	   and not $shield
	   and $budget >= $twohanded) {
    $budget -= $twohanded;
    push(@property, T('two handed sword'));
  } elsif ($class eq T('fighter')
	   and good($str)
	   and $hp > 6
	   and not $shield
	   and $budget >= $battleaxe) {
    $budget -= $battleaxe;
    push(@property, T('battle axe'));
  } elsif ($class eq T('fighter')
	   and average($str)
	   and not $shield
	   and $budget >= $polearm) {
    $budget -= $polearm;
    push(@property, T('pole arm'));
  } elsif ($class eq T('dwarf')
	   and not $shield
	   and $budget >= $battleaxe) {
    $budget -= $battleaxe;
    push(@property, T('battle axe'));
  } elsif ($budget >= $longsword
	   and d6() > 1) {
    $budget -= $longsword;
    push(@property, T('long sword'));
  } elsif ($budget >= $shortsword) {
    $budget -= $shortsword;
    push(@property, T('short sword'));
  }

  my ($longbow, $arrows, $shortbow, $crossbow, $quarrels);
  if ($char{rules} eq "labyrinth lord") {
    ($longbow, $arrows, $shortbow, $crossbow, $quarrels, $sling)
      = (40, 5, 25, 25, 9, 2);
  } else {
    ($longbow, $arrows, $shortbow, $crossbow, $quarrels, $sling)
      = (40, 5, 25, 30, 10, 2);
  }

  if (($class eq T('fighter') or $class eq T('elf'))
      and average($dex)
      and $budget >= ($longbow + $arrows)) {
    $budget -= ($longbow + $arrows);
    push(@property, T('long bow'));
    push(@property, T('quiver with 20 arrows'));
    if ($budget >= $arrows) {
      $budget -= $arrows;
      add(T('quiver with 20 arrows'), @property);
    }
  } elsif (($class ne T('cleric') and $class ne T('magic-user'))
      and average($dex)
      and $budget >= ($shortbow + $arrows)) {
    $budget -= ($shortbow + $arrows);
    push(@property, T('short bow'));
    push(@property, T('quiver with 20 arrows'));
    if ($budget >= $arrows) {
      $budget -= $arrows;
      add(T('quiver with 20 arrows'), @property);
    }
  } elsif (($class ne T('cleric') and $class ne T('magic-user'))
      and $budget >= ($crossbow + $bolts)) {
    $budget -= ($crossbow + $bolts);
    push(@property, T('crossbow'));
    push(@property, T('case with 30 bolts'));
  } elsif ($class ne T('magic-user')
      and $budget >= $sling) {
    $budget -= $sling;
    push(@property, T('sling'));
    push(@property, T('pouch with 30 stones'));
  }

  my ($handaxe, $spear);
  if ($char{rules} eq "labyrinth lord") {
    ($handaxe, $spear) = (1, 3);
  } else {
    ($handaxe, $spear) = (4, 3);
  }

  if (($class eq T('dwarf') or member(T('battle axe'), @property))
      and $budget >= $handaxe) {
    $budget -= $handaxe;
    push(@property, T('hand axe'));
    if ($budget >= $handaxe) {
      $budget -= $handaxe;
      add(T('hand axe'), @property);
    }
  } elsif ($class eq T('fighter')
	   and $budget >= $spear) {
    $budget -= $spear;
    push(@property, T('spear'));
  }

  if ($class ne T('cleric')
      and $budget >= (10 * $dagger)) {
    $budget -= (10 * $dagger);
    push(@property, T('silver dagger'));
  }

  if ($class ne T('cleric')
      and $class ne T('magic-user')
      and $budget >= $dagger) {
    $budget -=$dagger;
    push(@property, T('dagger'));
    if ($budget >= $dagger) {
      $budget -=$dagger;
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

  provide("breath",  $breath);
  provide("poison",  $poison);
  provide("petrify",  $petrify);
  provide("wands",  $wands);
  provide("spells",  $spells);
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
    $style =~ s/fill:#\d+/fill:magenta/ or $style .= ";fill:magenta";
    $node->setAttribute("style", $style);
  }

  for my $node ($svg->find(qq{//svg:image}, $doc)->get_nodelist) {
    my $id = $node->getAttribute("id");
    next if $id =~ /^text[0-9]+(-[0-9]+)*$/; # skip Inkscape default texts
    next unless $id =~ /^[-a-z0-9]+$/;
    my $text = XML::LibXML::Element->new("text");
    $text->setAttribute("x", $node->getAttribute("x") + 5);
    $text->setAttribute("y", $node->getAttribute("y") + 10);
    $text->appendText($id);
    $text->setAttribute("style", "font-size:8px;fill:magenta");
    $node->addSibling($text);
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

sub roll_3d8 {
  return d8() + d8() + d8();
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

sub provide {
  my ($key, $value) = @_;
  push(@provided, $key) unless $char{$key};
  $char{$key} = $value;
}

sub random_parameters {
  my ($str, $dex, $con, $int, $wis, $cha) =
    (roll_3d6(), roll_3d6(), roll_3d6(),
     roll_3d6(), roll_3d6(), roll_3d6());

  # if a class is provided, make sure minimum requirements are met
  my $class = $char{class};
  while ($class eq T('dwarf') and not average($con)) {
    $con = roll_3d6();
  }
  while ($class eq T('elf') and not average($int)) {
    $int = roll_3d6();
  }
  while ($class eq T('halfling') and not average($con)) {
    $con = roll_3d6();
  }
  while ($class eq T('halfling') and not average($dex)) {
    $dex = roll_3d6();
  }

  provide("str", $str);
  provide("dex", $dex);
  provide("con", $con);
  provide("int", $int);
  provide("wis", $wis);
  provide("cha", $cha);

  provide("level",  "1");
  provide("xp",  "0");
  provide("thac0",  19);

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

  provide("class",  $class);

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

  provide("hp",  $hp);

  equipment();

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
  provide("abilities", $abilities);
}

if ($lang eq "de") {
  # http://charaktereigenschaften.miroso.de/
  $_ = qq{
aalglatt abenteuerlustig abfällig abgebrüht abgehoben abgeklärt abgestumpft
absprachefähig abwartend abweisend abwägend achtsam affektiert affig aggressiv
agil akkurat akribisch aktiv albern altklug altruistisch ambitioniert
anarchisch angeberisch angepasst angriffslustig angsteinflößend angstvoll
anhänglich anmutig anpassungsfähig ansprechend anspruchslos anspruchsvoll
anstrengend anzüglich arbeitswütig arglistig arglos argwöhnisch arrogant artig
asketisch athletisch attraktiv aufbegehrend aufbrausend aufdringlich aufgedreht
aufgeregt aufgeschlossen aufgeweckt aufhetzerisch aufmerksam
aufmerksamkeitsbedürftig aufmüpfig aufopfernd aufrichtig aufschneiderisch
aufsässig ausdauernd ausdruckslos ausdrucksstark ausfallend ausgeflippt
ausgefuchst ausgeglichen ausländerfeindlich ausnutzbar autark authentisch
autonom autoritär außergewöhnlich barbarisch barmherzig barsch bedacht
bedrohlich bedrückt bedächtig bedürfnislos beeinflussbar befangen befehlerisch
begeistert begeisterungsfähig begierig begnügsam begriffsstutzig behaglich
beharrlich behende beherrscht beherzt behutsam behäbig beirrbar belastbar
belebend beliebt bemüht bequem berechnend beredsam berüchtigt bescheiden
besessen besitzergreifend besonders besonnen besorgt besserwissend
besserwisserisch bestechend bestechlich bestialisch bestimmend bestimmerisch
beständig betriebsam betrügerisch betörend bewandert bewusst bezaubernd bieder
bigott bissig bitter bizarr blasiert blass blauäugig blumig blutrünstig bockig
bodenständig borniert boshaft brav breitspurig brisant brummig brutal bärbeißig
bösartig böse böswillig chaotisch charismatisch charmant chauvinistisch
cholerisch clever cool couragiert damenhaft dankbar defensiv dekadent
demagogisch demütig depressiv derb desorganisiert despotisch destruktiv
determinativ devot dezent dezidiert diabolisch dickhäutig dickköpfig diffus
diktatorisch diplomatisch direkt diskret distanziert distinguiert diszipliniert
disziplinlos divenhaft dogmatisch doktrinär dominant doof dramatisch
dramatisierend draufgängerisch dreist drängend dubios duckmäuserisch duldsam
dumm durchblickend durcheinander durchschaubar durchschauend durchsetzungsstark
durchtrieben dusselig dynamisch dämlich dünkelhaft dünnhäutig echt edel
effizient egoistisch egoman egozentrisch ehrenhaft ehrenwert ehrfürchtig
ehrgeizig ehrlich eifersüchtig eifrig eigen eigenartig eigenbestimmt
eigenbrödlerisch eigenmächtig eigennützig eigensinnig eigenständig eigenwillig
eilig einfach einfallslos einfallsreich einfältig einfühlsam eingebildet
eingeschüchtert einladend einnehmend einsam einsatzbereit einschüchternd
einseitig einsichtig einträchtig eintönig einzelgängerisch einzigartig eisern
eiskalt eitel ekelig elastisch elefantös elegant elitär emotional empathisch
empfindlich empfindsam empfindungsvoll emsig energetisch energiegeladen
energievoll energisch engagiert engstirnig entgegenkommend enthaltsam enthemmt
enthusiastisch entscheidungsfreudig entschieden entschlossen entspannt
enttäuscht erbarmungslos erbärmlich erfinderisch erfolgsorientiert erfrischend
ergeben erhaben erlebnisse ermutigend ernst ernsthaft erotisch erwartungsvoll
exaltiert exorbitant experimentierfreudig extravagant extravertiert
extrovertiert exzentrisch facettenreich fair falsch familiär fantasielos
fantasiereich fantasievoll fantastisch fatalistisch faul feige fein feindselig
feinfühlig feinsinnig feminin fesselnd feurig fies fixiert flatterhaft fleissig
fleißig flexibel folgsam fordernd forsch fragil frech freiheitskämfend
freiheitsliebend freimütig freizügig fremdbestimmend fremdbestimmt freudvoll
freundlich friedfertig friedlich friedliebend friedlos friedselig friedvoll
frigide frisch frohgemut frohnatur frohsinnig fromm frostig fröhlich furchtlos
furchtsam furios fügsam fürsorglich galant gallig gamsig garstig gastfreundlich
gebieterisch gebildet gebührend gedankenlos gediegen geduldig gefallsüchtig
gefährlich gefällig gefügig gefühllos gefühlsbetont gefühlskalt gefühlvoll
geheimnisvoll gehemmt gehorsam gehässig geistreich geizig geladen gelassen
geldgierig geltungssüchtig gemein gemütvoll genauigkeitsliebend generös genial
genügsam gepflegt geradlinig gerecht gerechtigkeitsliebend gerissen gescheit
geschickt geschmeidig geschwätzig gesellig gesprächig gesundheitsbewusst
gewaltsam gewalttätig gewieft gewissenhaft gewissenlos gewitzt gewöhnlich
gierig giftig glamurös glaubensstark gleichgültig gleichmütig gläubig gnadenlos
gottergeben gottesfürchtig grantig grausam grazil griesgrämig grimmig grob
grotesk großherzig großkotzig großmäulig großmütig großspurig großzügig
gräßlich größenwahnsinnig grübelnd gründlich gutgläubig gutherzig gutmütig
gönnerhaft gütig haarspalterisch habgierig habsüchtig halsstarrig harmlos
harmoniebedürftig harmoniesüchtig hart hartherzig hartnäckig hasenherzig
hasserfüllt hedonistisch heimatverbunden heimtückisch heiter hektisch
heldenhaft heldenmütig hellhörig hemmungslos herablassend herausfordernd
heroisch herrisch herrlich herrschsüchtig herzerfrischend herzlich herzlos
hetzerisch heuchlerisch hibbelig hilflos hilfsbereit hingebungsvoll
hinterfotzig hintergründig hinterhältig hinterlistig hinterwäldlerisch
hirnrissig hitzig hitzköpfig hochbegabt hochfahrend hochmütig hochnäsig
hochtrabend humorlos humorvoll hyperkorrekt hysterisch hämisch hässlich
häuslich höflich höflichkeitsliebend höhnisch hübsch ichbezogen idealistisch
ideenreich idiotisch ignorant impertinent impulsiv inbrünstig individualistisch
infam infantil initiativ inkompetent inkonsequent innovativ instinktiv integer
intelektuell intelligent intensiv interessiert intolerant intrigant
introvertiert intuitiv ironisch irre jovial jugendlich jung jähzornig
kalkulierend kalt kaltblütig kaltherzig kaltschnäuzig kapriziös kasuistisch
katzig kauzig keck kess ketzerisch keusch kinderlieb kindisch kindlich klar
kleingeistig kleinkariert kleinlaut kleinlich kleinmütig klug knackig knallhart
knickrig kokett komisch kommunikationsfähig kommunikativ kompetent kompliziert
kompromissbereit konfliktfreudig konfliktscheu konkret konsequent konservativ
konsistent konstant kontaktarm kontaktfreudig kontraproduktiv kontrareligiös
kontrolliert konziliant kooperativ kopffrorm kopflastig kordial korrekt korrupt
kosmopolitisch kraftvoll krank kratzbürstig kreativ kriecherisch
kriegstreiberisch kriminell kritisch kritkfähig kräftig kulant kultiviert
kumpelhaft kurios kämpferisch kühl kühn künstlerisch künstlich labil lachhaft
lahm lammfromm langmütig langweilig larmoyant launisch laut lebendig
lebensbejahend lebensfroh lebenslustig lebhaft leicht leichtfertig leichtfüssig
leichtgläubig leichtlebig leichtsinnig leidenschaftlich leidlich leise
leistungsbereit leistungsstark lernbereit lethargisch leutselig liberal lieb
liebenswert liebevoll lieblich lieblos locker loyal lustlos lustvoll
lösungsorientiert lügnerisch lüstern machtbesessen machtgierig machthaberisch
machthungrig mager magisch manipulativ markant martialisch maskulin
masochistisch materialistisch matriachalisch maßlos melancholisch memmenhaft
menschenscheu menschenverachtend merkwürdig mies mild militant mimosenhaft
minimalistisch misanthropisch missgünstig missmutig misstrauisch mitfühlend
mitleiderregend mitleidlos mitleidslos mitteilsam modisch mollig mondän
moralisch motivierend motiviert musikalisch mutig männerfeindlich mürrisch
mütterlich nachdenklich nachgiebig nachlässig nachsichtig nachtragend naiv
naturfreudig naturverbunden natürlich nebulös neckisch negativ neiderfüllt
neidisch nervig nervös nett neugierig neurotisch neutral nichtssagend
niedergeschlagen niederträchtig niedlich nihilistisch nonchalant normal notgeil
nutzlos nüchtern oberflächlich objektiv obszön offen offenherzig
opportunistisch oppositionell optimistisch ordentlich ordinär ordnungsfähig
ordnungsliebend organisiert orientierungslos originell paranoid passiv patent
patriarchisch patriotisch pedantisch pejorativ penibel perfektionistisch
pervers pessimistisch pfiffig pflegeleicht pflichtbewusst pflichtversessen
phantasievoll philanthropisch phlegmatisch phobisch pingelig planlos plump
polarisierend politisch positiv pragmatisch prinzipientreu problembewusst
profilierungssüchtig progressiv prollig promiskuitiv prophetisch protektiv
provokant prüde psychotisch putzig pünktlich qualifiziert quengelig querdenkend
querulant quicklebendig quirlig quälend rabiat rachsüchtig radikal raffiniert
rastlos ratgebend rational ratlos ratsuchend rau reaktionsschnell reaktionär
realistisch realitätsfremd rebellisch rechthaberisch rechtlos rechtschaffend
redegewandt redelustig redselig reflektiert rege reif reiselustig reizbar
reizend reizvoll religiös renitent reserviert resigniert resolut respektlos
respektvoll reumütig rigoros risikofreudig robust romantisch routineorientiert
ruhelos ruhig ruppig rückgratlos rücksichtslos rücksichtsvoll rüde sachlich
sadistisch sanft sanftmütig sanguinisch sardonisch sarkastisch sauertöpfisch
schadenfroh schamlos scheinheilig scheu schlagfertig schlampig schlau
schmeichelhaft schneidig schnell schnippisch schnoddrig schreckhaft schrullig
schullehrerhaft schusselig schwach schweigsam schwermütig schäbig schöngeistig
schüchtern seicht selbstbewusst selbstdarstellerisch selbstgefällig
selbstgerecht selbstherrlich selbstkritisch selbstlos selbstreflektierend
selbstsicher selbstständig selbstsüchtig selbstverliebt selbstzweifelnd seltsam
senil sensationslüstern sensibel sensitiv sentimental seriös sexistisch sexy
sicherheitsbedürftig sinnlich skeptisch skrupellos skurril smart solidarisch
solide sonnig sorgfältig sorglos sorgsam souverän sparsam spaßig spießig
spirituell spitzfindig spontan sportlich sprachbegabt spritzig sprunghaft
spröde spöttisch staatsmännisch stabil stachelig standhaft stark starr
starrköpfig starrsinnig stereotypisch stilbewusst still stilsicher stilvoll
stinkig stoisch stolz strahlend strategisch streberhaft strebsam streitsüchtig
streng strikt stumpf stur sturköpfig störend störrisch stürmisch subjektiv
subtil suchend suchtgefährdet suspekt sympathisch süchtig tadellos taff
tagträumerisch taktisch taktlos taktvoll talentiert tatkräftig tatlos teamfähig
temperamentlos temperamentvoll tiefgründig tierlieb tolerant toll tollkühn
tollpatschig tough transparent traurig treu trotzig träge träumerisch
trübsinnig tyrannisch töricht tüchtig ulkig umgänglich umsichtig umständlich
umtriebig unabhängig unanständig unantastbar unartig unaufrichtig
unausgeglichen unbedeutend unbeherrscht unbeirrbar unbelehrbar unberechenbar
unbeschreiblich unbeschwert unbesonnen unbeständig unbeugsam undankbar
undiszipliniert undurchschaubar undurchsichtig unehrlich uneigennützig uneinig
unentschlossen unerbittlich unerreichbar unerschrocken unerschütterlich
unerträglich unfair unfein unflätig unfolgsam unfreundlich ungeduldig
ungehorsam ungehörig ungerecht ungeschickt ungesellig ungestüm ungewöhnlich
ungezogen ungezügelt unglaubwürdig ungläubig unhöflich unkompliziert
unkonventionell unkonzentriert unmenschlich unnachgiebig unnahbar unordentlich
unparteiisch unproblematisch unpünktlich unrealistisch unreflektiert unruhig
unsachlich unscheinbar unschlüssig unschuldig unselbständig unsensibel unsicher
unstet unternehmungsfreudig unternehmungslustig untertänig unterwürfig untreu
unverschämt unverwechselbar unverzagt unzufrieden unzuverlässig verachtend
verantwortungsbewusst verantwortungslos verantwortungsvoll verbindlich
verbissen verbittert verbrecherisch verfressen verführerisch vergebend
vergesslich verhandlungsstark verharrend verkopft verlangend verletzbar
verletzend verliebt verlogen verlustängstlich verlässlich vermittelnd
vernetzend vernünftig verrucht verräterisch verrückt verschlagen verschlossen
verschmitzt verschroben verschüchtert versiert verspielt versponnen
verständnislos verständnisvoll verstört vertrauensvoll vertrauenswürdig
verträumt verwahrlost verwegen verwirrt verwundert verwöhnt verzweifelt
vielfältig vielschichtig vielseitig vital vorausschauend voreingenommen vorlaut
vornehm vorsichtig vorwitzig väterlich wagemutig waghalsig wahnhaft wahnsinnig
wahnwitzig wahrhaftig wahrheitsliebend wankelmütig warm warmherzig wechselhaft
wehmütig weiblich weich weinselig weise weitsichtig weltfremd weltoffen wendig
wichtigtuerisch widerlich widerspenstig widersprüchlich widerstandsfähig wild
willenlos willensschwach willensstark willig willkürlich wirsch wissbegierig
wissensdurstig witzig wohlerzogen wohlgesinnt wortkarg wählerisch würdelos
würdevoll xanthippisch zaghaft zappelig zartbesaitet zartfühlend zauberhaft
zaudernd zerbrechlich zerdenkend zerknautscht zerstreut zerstörerisch zickig
zielbewusst zielführend zielorientiert zielstrebig zimperlich zufrieden
zugeknöpft zuhörend zukunftsgläubig zupackend zurechnungsfähig zurückhaltend
zuverlässig zuversichtlich zuvorkommend zwanghaft zweifelnd zwiegespalten
zwingend zäh zärtlich zögerlich züchtig ängstlich ätzend öde überdreht
überemotional überfürsorglich übergenau überheblich überkandidelt überkritisch
überlebensfähig überlegen überlegt übermütig überragend überraschend
überreagierend überschwenglich übersensibel überspannt überwältigent};
} else {
  # http://www.roleplayingtips.com/tools/1000-npc-traits/
  $_ = qq{
able abrasive abrupt absent minded abusive accepting accident prone
accommodating accomplished action oriented active adaptable substance abusing
adorable adventurous affable affected affectionate afraid uncommited
aggressive agnostic agreeable alert alluring aloof
altruistic always hungry always late ambiguous ambitious amiable amused
amusing angry animated annoyed annoying anti-social anxious apathetic
apologetic appreciative apprehensive approachable argumentative
aristocratic arrogant artistic ashamed aspiring assertive astonished
attentive audacious austere authoritarian authoritative available
average awful awkward babbling babyish bad bashful beautiful
belligerent bewildered biter blames others blasé blowhard boastful
boisterous bold boorish bored boring bossy boundless brainy brash
bratty brave brazen bright brilliant brotherly brutish bubbly busy
calculating callous calm candid capable capricious carefree careful
careless caring caustic cautious changeable charismatic charming chaste
cheerful cheerless childish chivalrous civilised classy clean clever
close closed clumsy coarse cocky coherent cold cold hearted combative
comfortable committed communicative compassionate competent complacent
compliant composed compulsive conceited concerned condescending
confident confused congenial conscientious considerate consistent
constricting content contented contrarian contrite controlling
conversational cooperative coquettish courageous courteous covetous
cowardly cowering coy crabby crafty cranky crazy creative credible
creepy critical cross crude cruel cuddly cultured curious cutthroat
cynical dainty dangerous daring dark dashing dauntless dazzling
debonair deceitful deceiving decent decisive decorous deep defeated
defective deferential defiant deliberate delicate delightful demanding
demonic dependable dependent depressed deranged despicable despondent
detached detailed determined devilish devious devoted dignified
diligent direct disaffected disagreeable discerning disciplined
discontented discouraged discreet disgusting dishonest disillusioned
disinterested disloyal dismayed disorderly disorganized disparaging
disrespectful dissatisfied dissolute distant distraught distressed
disturbed dogmatic domineering dorky doubtful downtrodden draconian
dramatic dreamer dreamy dreary dubious dull dumb dutiful dynamic
eager easygoing eccentric educated effervescent efficient egocentric
egotistic elated eloquent embarrassed embittered embraces change eminent
emotional empathetic enchanting encouraging enduring energetic engaging
enigmatic entertaining enthusiastic envious equable erratic ethical
evasive evil exacting excellent excessive excitable excited exclusive
expansive expert extravagant extreme exuberant fabulous facetious faded
fair faith in self faithful faithless fake fanatical fanciful fantastic
fatalistic fearful fearless feisty ferocious fidgety fierce fiery
fighter filthy fine finicky flagging flakey flamboyant flashy fleeting
flexible flighty flippant flirty flustered focused foolish forceful
forgetful forgiving formal fortunate foul frank frantic fresh fretful
friendly frightened frigid frugal frustrated fuddy duddy fun fun loving
funny furious furtive fussy gabby garrulous gaudy generous genial
gentle giddy giggly gives up easily giving glamorous gloomy glorious
glum goal orientated good goofy graceful gracious grandiose grateful
greedy gregarious grieving grouchy growly gruesome gruff grumpy
guarded guilt ridden guilty gullible haggling handsome happy hard hard working
hardy harmonious harried harsh hateful haughty healthy heart broken
heartless heavy hearted hedonistic helpful helpless hesitant
high high self esteem hilarious homeless honest honor bound honorable
hopeful hopeless hormonal horrible hospitable hostile hot headed huffy
humble humorous hurt hysterical ignorant ill ill-bred imaginative
immaculate immature immobile immodest impartial impatient imperial
impolite impotent impractical impudent impulsive inactive incoherent
incompetent inconsiderate inconsistent indecisive independent indifferent
indiscrete indiscriminate indolent indulgent industrious inefficient
inept inflexible inimitable innocent inquisitive insecure insensitive
insightful insincere insipid insistent insolent instinctive insulting
intellectual intelligent intense interested interrupting intimidating
intolerant intrepid introspective introverted intuitive inventive
involved irresolute irresponsible irreverent irritable irritating
jackass jaded jealous jittery joking jolly jovial joyful joyous
judgmental keen kenderish kind hearted kittenish knowledgeable
lackadaisical lacking languid lascivious late lazy leader lean
lethargic level lewd liar licentious light-hearted likeable limited
lineat lingering lively logical lonely loquacious lordly loud
loudmouth lovable lovely loves challenge loving low confidence lowly
loyal lucky lunatic lying macho mad malice malicious manipulative
mannerly materialistic matronly matter-of-fact mature mean meek
melancholy melodramatic mentally slow merciful mercurial messy
meticulous mild mischievous miserable miserly mistrusting modern modest
moody moping moralistic motherly motivated mysterious nagging naive
narcissistic narrow-minded nasty naughty neat needs social approval
needy negative negligent nervous neurotic never hungry nibbler nice
night owl nihilistic nimble nit picker no purpose no self confidence
noble noisy nonchalant nosy not trustworthy nuanced nuisance nurturing
nut obedient obese obliging obnoxious obscene obsequious observant
obstinate odd odious open open-minded opinionated opportunistic
optimistic orcish orderly organized ornery ossified ostentatious
outgoing outrageous outspoken overbearing overweight overwhelmed
overwhelming paces pacifistic painstaking panicky paranoid particular
passionate passive passive-aggressive pathetic patient patriotic
peaceful penitent pensive perfect perfectionist performer perserverant
perseveres persevering persistent persuasive pert perverse pessimistic
petty petulant philanthropic picky pious pitiful placid plain playful
pleasant pleasing plotting plucky polite pompous poor popular positive
possessive practical precise predictable preoccupied pretentious pretty
prim primitive productive profane professional promiscuous proper
protective proud prudent psychotic puckish punctilious punctual
purposeful pushy puzzled quarrelsome queer quick quick tempered quiet
quirky quixotic rambunctious random rash rational rawboned realistic
reasonable rebellious recalcitrant receptive reckless reclusive refined
reflective regretful rejects change relaxed relents reliable relieved
religious reluctant remorseful repugnant repulsive resentful reserved
resilient resolute resourceful respectful responsible responsive
restless retiring rhetorical rich right righteous rigid risk-taking
romantic rough rowdy rude rugged ruthless sacrificing sad sadistic
safe sagely saintly salient sanctimonious sanguine sarcastic sassy
satisfied saucy savage scared scarred scary scattered scheming
scornful scrawny scruffy secretive secure sedate seductive selective
self-centered self-confident self-conscious self-controlling self-directed
self-disciplined self-giving self-reliant self-serving selfish selfless
senile sensitive sensual sentimental serene serious sexual sexy
shallow shameless sharp sharp-tongued sharp-witted sheepish shiftless
shifty short shrewd shy silent silky silly simian simple sincere
sisterly skillful sleazy sloppy slovenly slow paced slutty sly
small-minded smart smiling smooth sneaky snob sociable soft-hearted
soft-spoken solitary sore sorry sour spendthrift spiteful splendid
spoiled spontaneous spunky squeamish stately static steadfast sterile
stern stimulating stingy stoical stolid straight laced strange strict
strident strong strong willed stubborn studious stupid suave submissive
successful succinct sulky sullen sultry supercilious superstitious
supportive surly suspicious sweet sympathetic systematic taciturn tacky
tactful tactless talented talkative tall tardy tasteful temperamental
temperate tenacious tense tentative terrible terrified testy thankful
thankless thick skinned thorough thoughtful thoughtless threatening
thrifty thrilled tight timid tired tireless tiresome tolerant touchy
tough trivial troubled truculent trusting trustworthy truthful typical
ugly unappreciative unassuming unbending unbiased uncaring uncommitted
unconcerned uncontrolled unconventional uncooperative uncoordinated
uncouth undependable understanding undesirable undisciplined
unenthusiastic unfeeling unfocused unforgiving unfriendly ungrateful
unhappy unhelpful uninhibited unkind unmotivated unpredictable
unreasonable unreceptive unreliable unresponsive unrestrained unruly
unscrupulous unselfish unsure unsympathetic unsystematic unusual
unwilling upbeat upset uptight useful vacant vague vain valiant
vengeful venomous verbose versatile vigorous vindictive violent
virtuous visual vivacious volatile voracious vulgar vulnerable warlike
warm hearted wary wasteful weak weary weird well grounded whimsical
wholesome wicked wild willing wise wishy washy withdrawn witty worldly
worried worthless wretched xenophobic youthful zany zealous};
}
my @trait = split(/[ \r\n]+/, $_);

sub traits {
  my $description;
  my $d = d6();
  if ($d == 1) {
    $description = T('young woman');
  } elsif ($d == 2) {
    $description = T('young man');
  } elsif ($d == 3) {
    $description = T('woman');
  } elsif ($d == 4) {
    $description = T('man');
  } elsif ($d == 5) {
    $description = T('elderly woman');
  } elsif ($d == 6) {
    $description = T('elderly man');
  };
  $description .= ", ";
  my $trait = $trait[rand @trait];
  $description .= $trait;
  my $other = $trait[rand @trait];
  if ($other ne $trait) {
    $description .= " " . T('and') . " " . $other;
  }

  return $description;
}

sub characters {
  header('');
  my %init = map { $_ => $char{$_} } @provided;
  for (my $i = 0; $i < 50; $i++) {
    $q->delete_all();
    %char = %init;
    print $q->start_pre({-style=>"display: inline-block; padding: 0 1em; width: 30em; border-left: 1px dotted grey; vertical-align: top; font-size: 8pt; "});
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
    print traits() . "\n";
    print map { "  $_\n" }
      split(/\\\\/, $char{property});
    print $q->end_pre();
  }
  print $q->div({-style=>"clear: both;"});
  footer();
}

sub stats {
  print $q->header(-type=>"text/plain",
		   -charset=>"utf8");
  binmode(STDOUT, ":utf8");

  my (%class, %property, %init);
  %init = map { $_ => $char{$_} } @provided;
  for (my $i = 0; $i < 10000; $i++) {
    $q->delete_all();
    %char = %init;
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
  for my $key (@provided) {
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
		     -columns => 55,
		     -style   => "width: 100%", );
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
		      T('Halberds and Helmets'))));
  print $q->start_form(-method=>"get", -action=>"$url/random/$lang",
		       -accept_charset=>"UTF-8"),
    T('Name:'), " ", $q->textfield("name"), " ", $q->submit, $q->end_form;
  print $q->p(T('The character sheet contains a link in the bottom right corner which allows you to bookmark and edit your character.'));
  footer();
  print $q->end_html;
}

sub help {
  header();
  print $q->p(T('The generator works by using a template and replacing some placeholders.'));

  print $q->h2(T('Basic D&amp;D'));

  print $q->p(T('The default template (%0) uses the %1 font.',
		$q->a({-href=>"/" . T('Charactersheet.svg')},
		      T('Charactersheet.svg')),
		$q->a({-href=>"/Purisa.ttf"},
		      "Purisa")),
	      T('You provide values for the placeholders by providing URL parameters (%0).',
		$q->a({-href=>$example}, T('example'))),
	      T('The script can also show %0.',
		$q->a({-href=>"$url/show/$lang"},
		      T('which parameters go where'))),
	      T('Also note that the parameters need to be UTF-8 encoded.'),
	      T('If the template contains a multiline placeholder, the parameter may also provide multiple lines separated by two backslashes.'));
  print $q->p(T('In addition to that, some parameters are computed unless provided:'));
  print "<ul>";
  my @doc = qw(str str-bonus
	       dex dex-bonus
	       con con-bonus
	       int int-bonus
	       wis wis-bonus
	       cha cha-bonus
	       cha-bonus loyalty
	       str-bonus damage
	       thac0 melee0-9&nbsp;&amp;&nbsp;range0-9);
  while (@doc) {
    print $q->li(shift(@doc), "&rarr;", shift(@doc));
  }
  print "</ul>";

  my ($random, $bunch, $stats) =
    ($q->a({-href=>"$url/random/$lang"}, T('random character')),
     $q->a({-href=>"$url/characters/$lang"}, T('bunch of characters')),
     $q->a({-href=>"$url/stats/$lang"}, T('some statistics')));

  print $q->p(T('The script can also generate a %0, a %1, or %2.',
		$random, $bunch, $stats));

  ($random, $bunch, $stats) =
    ($q->a({-href=>"$url/random/$lang?rules=labyrinth+lord"},
	   T('random character')),
     $q->a({-href=>"$url/characters/$lang?rules=labyrinth+lord"},
	   T('bunch of characters')),
     $q->a({-href=>"$url/stats/$lang?rules=labyrinth+lord"},
	   T('some statistics')));

  print $q->p(T('As the price list for Labyrinth Lord differs from the Moldvay price list, you can also generate a %0, a %1, or %2 using Labyrinth Lord rules.',
		$random, $bunch, $stats));

  print $q->h2(T('Pendragon'));
  print $q->p(T('The script also supports Pendragon characters (but cannot generate them randomly):'),
	      T('Get started with a %0.',
		$q->a({-href=>"$url/link/$lang?rules=pendragon;charsheet=http%3a%2f%2fcampaignwiki.org%2fPendragon.svg"},
		      T('Pendragon character'))),
	      T('The script can also show %0.',
		$q->a({-href=>"$url/show/$lang?rules=pendragon;charsheet=http%3a%2f%2fcampaignwiki.org%2fPendragon.svg"},
		      T('which parameters go where'))));
  print $q->p(T('In addition to that, some parameters are computed unless provided:'));
  print "<ul>";
  @doc = qw(str+siz damage
	       str+con heal
	       str+dex move
	       siz+con hp
	       hp unconscious);
  while (@doc) {
    print $q->li(shift(@doc), "&rarr;", shift(@doc));
  }
  @doc = qw(chaste lustful
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
  while (@doc) {
    print $q->li(shift(@doc), "&harr;", shift(@doc));
  }
  print "</ul>";

  print $q->h2(T('Crypts &amp; Things'));
  print $q->p(T('The script also supports Crypts &amp; Things characters (but cannot generate them randomly):'),
	      T('Get started with a %0.',
		$q->a({-href=>"$url/link/$lang?rules=crypts-n-things;charsheet=http%3a%2f%2fcampaignwiki.org%2fCrypts-n-Things.svg"},
		      T('Crypts &amp; Things character'))),
	      T('The script can also show %0.',
		$q->a({-href=>"$url/show/$lang?rules=crypts-n-things;charsheet=http%3a%2f%2fcampaignwiki.org%2fCrypts-n-Things.svg"},
		      T('which parameters go where'))));

  print $q->p(T('In addition to that, some parameters are computed unless provided:'));
  print "<ul>";
  @doc = qw(str to-hit
	    str damage-bonus
	    dex missile-bonus
	    dex ac-bonus
	    con con-bonus
	    int understand
	    cha charm
	    cha hirelings
	    wis sanity);
  while (@doc) {
    print $q->li(shift(@doc), "&rarr;", shift(@doc));
  }
  print "</ul>";

  print $q->h2(T('Adventure Conqueror King'));
  print $q->p(T('The script also supports Adventure Conqueror King characters (but cannot generate them randomly):'),
	      T('Get started with an %0.',
		$q->a({-href=>"$url/link/$lang?rules=acks;charsheet=http%3a%2f%2fcampaignwiki.org%2fACKS.svg"},
		      T('Adventure Conqueror King character'))),
	      T('The script can also show %0.',
		$q->a({-href=>"$url/show/$lang?rules=crypts-n-things;charsheet=http%3a%2f%2fcampaignwiki.org%2fACKS.svg"},
		      T('which parameters go where'))));

  print $q->p(T('In addition to that, some parameters are computed unless provided:'));
  print "<ul>";
  @doc = qw(str str-bonus
	    int int-bonus
	    wis wis-bonus
	    dex dex-bonus
	    con con-bonus
	    cha cha-bonus
	    attack+str melee
	    attack+dex missile);
  while (@doc) {
    print $q->li(shift(@doc), "&rarr;", shift(@doc));
  }
  print "</ul>";

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
  } elsif ($q->path_info =~ m!/help\b!) {
    help();
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
1/6 for normal tasks
1/6 für normale Aufgaben
2/6 to find secret constructions and traps
2/6 um Geheimbauten und Fallen zu finden
2/6 to find secret or concealed doors
2/6 um geheime und versteckte Türen zu finden
2/6 to hear noise
2/6 um Geräusche zu hören
2/6 to hide and sneak
2/6 für Verstecken und Schleichen
5/6 to hide and sneak outdoors
5/6 für Verstecken und Schleichen im Freien
AC -2 vs. opponents larger than humans
Rüstung -2 bei Gegnern über Menschengrösse
Adventure Conqueror King
Adventure Conqueror King
Adventure Conqueror King character
Adventure Conqueror King Charakter
Also note that the parameters need to be UTF-8 encoded.
Die Parameter müssen UTF-8 codiert sein.
As the price list for Labyrinth Lord differs from the Moldvay price list, you can also generate a %0, a %1, or %2 using Labyrinth Lord rules.
Da die Preisliste für Labyrinth Lord sich von der Moldvay Liste etwas unterscheidet, kann man auch %0, %1 oder %2 mit Labyrinth Lord Regeln generieren.
Basic D&amp;D
Basic D&amp;D
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
Crypts &amp; Things
Crypts &amp; Things
Crypts &amp; Things character
Crypts &amp; Things Charakter
Edit
Bearbeiten
English
Englisch
German
Deutsch
Get started with a %0.
%0 bearbeiten.
Get started with an %0.
%0 bearbeiten.
Halberds and Helmets
Hellebarden und Helme
Help
Hilfe
If the template contains a multiline placeholder, the parameter may also provide multiple lines separated by two backslashes.
Die Vorlage kann auch mehrzeilige Platzhalter enthalten. Der entsprechende Parameter muss die Zeilen dann durch doppelte Backslashes trennen.
In addition to that, some parameters are computed unless provided:
Zudem werden einige Parameter berechnet, sofern sie nicht angegeben wurden:
Name:
Name:
Pendragon
Pendragon
Pendragon character
Pendragon Charakter
Source
Quellcode
Spells:
Zaubersprüche:
The character sheet contains a link in the bottom right corner which allows you to bookmark and edit your character.
Auf dem generierten Charakterblatt hat es unten rechts einen Link mit dem man sich ein Lesezeichen erstellen kann und wo der Charakter bearbeitet werden kann.
The default template (%0) uses the %1 font.
Die Defaultvorlage (%0) verwendet die %1 Schrift.
The generator works by using a template and replacing some placeholders.
Das funktioniert über eine Vorlage und dem Ersetzen von Platzhaltern.
The script also supports Adventure Conqueror King characters (but cannot generate them randomly):
Das Skript kann auch Charaktere für Adventure Conqueror King anzeigen (aber nicht zufällig erstellen):
The script also supports Crypts &amp; Things characters (but cannot generate them randomly):
Das Skript kann auch Charaktere für Crypts &amp; Things anzeigen (aber nicht zufällig erstellen):
The script also supports Pendragon characters (but cannot generate them randomly):
Das Skript kann auch Pendragon Charaktere anzeigen (aber nicht zufällig erstellen):
The script can also generate a %0, a %1, or %2.
Das Skript kann auch %0, %1 oder %2 generieren.
The script can also show %0.
Das Skript kann auch zeigen %0.
This is the %0 character sheet generator.
Dies ist der %0 Charaktergenerator.
Use the following form to make changes to your character sheet.
Mit dem folgenden Formular lassen sich leicht Änderungen am Charakter machen.
You can also copy and paste it on to a %0 page to generate an inline character sheet.
Man kann diesen Text auch auf einer %0 Seite verwenden, um das Charakterblatt einzufügen.
You provide values for the placeholders by providing URL parameters (%0).
Den Platzhaltern werden über URL Parameter Werte zugewiesen (%0).
and
und
backpack
Rucksack
battle axe
Streitaxt
bunch of characters
einige Charaktere
case with 30 bolts
Kiste mit 30 Bolzen
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
d6
W6
dagger
Dolch
detect magic
Magie entdecken
dwarf
Zwerg
elderly man
älterer Mann
elderly woman
ältere Frau
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
man
Mann
mirror
Spiegel
plate mail
Plattenpanzer
pole arm
Stangenwaffe
pouch with 30 stones
Beutel mit 30 Steinen
protection from evil
Schutz vor Bösem
quiver with 20 arrows
Köcher mit 20 Pfeilen
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
woman
Frau
wooden pole
Holzstab
young man
junger Mann
young woman
junge Frau
