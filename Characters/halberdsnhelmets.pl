#!/usr/bin/env perl

# Copyright (C) 2012-2016  Alex Schroeder <alex@gnu.org>

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

package HH;
use Mojolicious::Lite;
use Mojo::UserAgent;
use HTTP::AcceptLanguage;
use XML::LibXML;
use List::Util qw(shuffle);
use POSIX qw(floor ceil);
no warnings qw(uninitialized numeric);

my $home = $ENV{MOJO_HOME} || "/home/alex/farm/halberdsnhelmets";

sub translations {
  # strings in sinqle quotes are translated into German if necessary
  # use %0, %1, etc. for parameters
  my %translations = split(/\n/, q{%0 gold
%0 Gold
%0 is unknown.
%0 ist unbekannt.
%0 silver
%0 Silber
%0: How much does this cost?
%0: Wieviel kostet das?
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
Acrobatics
Akrobatik
Adventuring
Abenteurer
Alchemy
Alchemie
Alertness
Aufmerksamkeit
Animal Husbandry
Tierpflege
Animal Training (Dog)
Tiertrainer (Hunde)
Arcane Dabbling
Hexereien
Arcanist
Arkanist
Arcanist-Avenger
Arkaner Rächer
Arcanist-Guardian
Arkaner Wächter
Art
Kunst
Bargaining
Handeln
Beast Friendship
Tierfreundschaft
Blade-Initiate
Klingenkenner
Blind Fighting
Blind Kämpfen
Bribery
Bestechung
Cat Burglary
Einbruch
Catechist
Katechet
Caving
Höhlenwandern
Charactersheet.svg
Charakterblatt.svg
Classes
Klassen
Climbing
Klettern
Collegiate Wizardry
Zauberkollegium
Combat Refexes
Kampfreflexe
Combat Trickery (Disarm)
Austricksen (Entwaffnen)
Combat Trickery (Incapacitate)
Austricksen (Überwältigen)
Contortionism
Kontorsion
Craft
Handwerk
Diplomacy
Diplomatie
Disguise
Verkleiden
Dwarven Craft-Catechist
Zwergischer Werk-Katechet
ESP
Gedankenlesen
Eavesdropping
Lauschen
Endurance
Ausdauer
Engineering
Technik
Fighting Style
Kampfstil
First level spells:
Sprüche der ersten Stufe:
Footpad
Strassenräuber
Gambling
Glücksspiel
Healing
Heilung
Intimidation
Einschüchterung
Knowledge
Wissen
Labor
Arbeit
Language
Sprache
Leadership
Anführer
Lip Reading
Lippenlesen
Man-at-Arms
Landsknecht
Manual of Arms
Fechtbuch
Mapping
Kartographie
Military Strategy
Strategie
Mimicry
Mimikry
Naturalism
Naturfreund
Navigation
Navigation
Performance
Auftritt
Precise Shooting
Scharfschütze
Profession
Beruf
Property
Eigentum
Reciter
Rezitierer
Riding
Reiten
Running
Rennen
Scout
Späher
Seafaring
Seefahrt
Seduction
Verführung
Sentry
Wächter
Siege Engineering
Belagerung
Signaling
Signalisieren
Skirmishing
Plänkeln
Skulking
Schleichen
Sniping
Scharfschütze
Spells:
Zaubersprüche:
Survival
Überleben
Swashbuckling
Draufgängertum
Theology
Theologie
Thug
Schläger
Tracking
Spurenlesen
Trap Finding
Fallenfinden
Trapping
Fallenstellen
Unknown Price
Unbekannter Preis
Unknown Rules
Unbekannte Regeln
Weapon Finesse
Waffenfinesse
Weapon Focus
Waffenfokus
and
und
arcane lock
Arkanes Schloss
assassin
Assassine
backpack
Rucksack
barbarian
Barbar
bard
Barde
battle axe
Streitaxt
bladedancer
Klingentänzer
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
continual light
Ewiges Licht
crossbow
Armbrust
d6
W6
dagger
Dolch
detect evil
Böses entdecken
detect invisible
Unsichtbares entdecken
detect magic
Magie entdecken
dwarf
Zwerg
dwarven craftpriest
Zwergischer Werkpriester
dwarven vaultguard
Zwergischer Schatzwächter
elderly man
älterer Mann
elderly woman
ältere Frau
elf
Elf
elven bladedancer
Elfischer Klingentänzer
elven nightblade
Elfische Nachtklinge
elven spellsword
Elfische Zauberklinge
explorer
Forscher
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
invisibility
Unsichtbarkeit
iron rations (1 week)
Feldrationen (1 Woche)
iron spikes and hammer
Eisenkeile und Hammer
knock
Klopfen
lantern
Laterne
leather armor
Lederrüstung
levitate
Schweben
light
Licht
locate object
Objekt lokalisieren
long bow
Langbogen
long sword
Langschwert
mace
Streitkeule
mage
Magier
magic missile
Magisches Geschoss
magic-user
Magier
man
Mann
minor image
kleines Ebenbild
mirror
Spiegel
phantasmal force
phantastische Kraft
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
read languages
Sprachen lesen
read magic
Magie lesen
rope
Seil
second level spells:
Sprüche der zweiten Stufe:
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
web
Netz
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
1d10
1W10
1d4
1W4
1d6
1W6
1d8
1W6
2-handed
zweihänder
awkward
sperrig
axe
Axt
chaotic
chaotisch
close
sehr nah
evil
böse
far
fern
flail
Flegel
forceful
schwer
good
gut
great axe
Grossaxt
great hammer
Grosshammer
great sword
Zweihänder
hand
Hand
hatchet
Beil
heavy crossbow
Arbalest
human
Mensch
knife
Messer
lawful
rechtschaffen
light crossbow
Armbrust
near
nah
neutral
neutral
pick
Pickel
pierce
Stich
polearm
Stangenwaffe
precise
präzis
rare
selten
reach
Reichweite
reload
Nachladen
scale armor
Schuppenpanzer
shortbow
Kurzbogen
shortsword
Kurzschwert
sword
Schwert
throw
werfen
throwing knife
Wurfmesser
bag of books (○○○○○)
Büchertasche (○○○○○)
healing potion (heal 1d8 HP)
Heiltrank (für 1W8 LP)
magic orb
Kristallkugel
magic staff
Langer Zauberstab
magic wand
Zauberstab
power
Macht
rations
Wegzehrung
spell components (+1 power, ○○○)
Zauberzutaten (+1 Macht, ○○○)
});

  return \%translations;
}

my $translation = translations();
our $lang; # we'll set it in random_parameters

sub T {
  my ($en, @arg) = @_;
  my $suffix = '';
  # handle (2) suffixes
  if ($en =~ /(.*)( \(\d+\))$/) {
    $en = $1;
    $suffix = $2;
  }
  if ($translation->{$en} and $lang eq "de") {
    $en = $translation->{$en};
  }
  # utf8::encode($en);
  for (my $i = 0; $i < scalar @arg; $i++) {
    my $s = $arg[$i];
    $en =~ s/%$i/$s/g;
  }
  return $en . $suffix;
}

sub svg_read {
  my ($char) = @_;
  my $filename = $char->{charsheet} || 'Charactersheet.svg';
  my $doc;
  if (-f "$home/$filename") {
    $doc = XML::LibXML->load_xml(location => "$home/$filename");
  } else {
    my $ua = Mojo::UserAgent->new;
    my $tx = $ua->get($filename);
    die "«$filename»: " . $tx->res->error->{message} . "\n" unless $tx->success;
    $doc = XML::LibXML->load_xml(string => $tx->res->body);
  }
  return ($char, $doc); # used as parameters for svg_transform
}

sub replace_text {
  my ($parser, $node, $str) = @_;
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

  my $tspan = XML::LibXML::Element->new("tspan");
  $tspan->setAttribute("x", $node->getAttribute("x"));
  $tspan->setAttribute("y", $node->getAttribute("y"));

  while (@line) {
    my $line = shift(@line); # cannot have this in while cond because of "0"
    next if $line eq ''; # cannot parse empty strings
    my $fragment = $parser->parse_balanced_chunk(T($line));
    foreach my $child ($fragment->childNodes) {
      my $tag = $child->nodeName;
      if ($tag eq "strong" or $tag eq "b") {
	my $node = XML::LibXML::Element->new("tspan");
	$node->setAttribute("style", "font-weight:bold");
	$node->appendText($child->textContent);
	$tspan->appendChild($node);
      } elsif ($tag eq "em" or $tag eq "i") {
	my $node = XML::LibXML::Element->new("tspan");
	$node->setAttribute("style", "font-style:italic");
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

sub svg_transform {
  my ($self, $char, $doc) = @_;
  my $parser = XML::LibXML->new;
  my $svg = XML::LibXML::XPathContext->new;
  $svg->registerNs("svg", "http://www.w3.org/2000/svg");

  for my $id (keys %$char) {
    next unless $id =~ /^[-a-z0-9]+$/;
    my $nodes = $svg->find(qq{//svg:text[\@id="$id"]}, $doc);
    for my $node ($nodes->get_nodelist) {
      replace_text($parser, $node, $char->{$id}, $doc);
      next;
    }
    $nodes = $svg->find(qq{//svg:image[\@id="$id"]}, $doc);
    for my $node ($nodes->get_nodelist) {
      $node->setAttributeNS("http://www.w3.org/1999/xlink",
			    "xlink:href", $char->{$id});
      next;
    }
  }

  my $nodes = $svg->find(qq{//svg:a[\@id="link"]/attribute::xlink:href}, $doc);
  for my $node ($nodes->get_nodelist) {
    my $params = Mojo::Parameters->new;
    for my $key (@{$char->{provided}}) {
      $params->append($key => $char->{$key}||'');
    }
    $node->setValue($self->url_for("edit")->query($params));
  }
  return $doc;
}

sub svg_show_id {
  my ($char, $doc) = @_;

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
  my $char = shift;
  for my $id (qw(str dex con int wis cha)) {
    if ($char->{$id} and not $char->{"$id-bonus"}) {
      $char->{"$id-bonus"} = bonus($char->{$id});
    }
  }
  if ($char->{cha} and not $char->{reaction}) {
    $char->{reaction} = cha_bonus($char->{cha});
  }
  if (not $char->{loyalty}) {
    $char->{loyalty} =  7 + $char->{"cha-bonus"};
  }
  if (not $char->{hirelings}) {
    $char->{hirelings} =  4 + $char->{"cha-bonus"};
  }
  if ($char->{thac0} and not $char->{"melee-thac0"}) {
    $char->{"melee-thac0"} = $char->{thac0} - $char->{"str-bonus"};
  }
  if ($char->{thac0} and not $char->{"range-thac0"}) {
    $char->{"range-thac0"} = $char->{thac0} - $char->{"dex-bonus"};
  }
  for my $type ("melee", "range") {
    for (my $n = 0; $n <= 9; $n++) {
      my $val = $char->{"$type-thac0"} - $n;
      $val = 20 if $val > 20;
      $val =  1 if $val <  1;
      $char->{"$type$n"} = $val unless $char->{"$type$n"};
    }
  }
  if (not $char->{damage}) {
    $char->{damage} = 1 . T('d6');
  }
  if (not $char->{"melee-damage"}) {
    $char->{"melee-damage"} = $char->{damage} . $char->{"str-bonus"};
  }
  if (not $char->{"range-damage"}) {
    $char->{"range-damage"} = $char->{damage};
  }
  moldvay_saves($char);
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
  my $char = shift;
  if ($char->{str} and not $char->{"to-hit"}) {
    $char->{"to-hit"} = crypt_bonus($char->{str});
  }
  if ($char->{str} and not $char->{"damage-bonus"}) {
    $char->{"damage-bonus"} = crypt_bonus($char->{str});
  }
  if ($char->{dex} and not $char->{"missile-bonus"}) {
    $char->{"missile-bonus"} = crypt_bonus($char->{dex});
  }
  if ($char->{dex} and not $char->{"ac-bonus"}) {
    $char->{"ac-bonus"} = crypt_bonus($char->{dex});
  }
  if ($char->{con} and not $char->{"con-bonus"}) {
    $char->{"con-bonus"} = crypt_bonus($char->{con});
  }
  if ($char->{int} and not $char->{understand}) {
    if ($char->{int} <= 7) {
      $char->{understand} = "0%";
    } elsif ($char->{int} <= 9) {
      $char->{understand} = 5 * ($char->{int} - 7) . "%";
    } elsif ($char->{int} <= 16) {
      $char->{understand} = 5 * ($char->{int} - 6) . "%";
    } else {
      $char->{understand} = 15 * ($char->{int} - 13) . "%";
    }
  }
  if ($char->{cha} and not $char->{charm}) {
    if ($char->{cha} <= 4) {
      $char->{charm} = "10%";
    } elsif ($char->{cha} <= 6) {
      $char->{charm} = "20%";
    } elsif ($char->{cha} <= 8) {
      $char->{charm} = "30%";
    } elsif ($char->{cha} <= 12) {
      $char->{charm} = "40%";
    } elsif ($char->{cha} <= 15) {
      $char->{charm} = "50%";
    } elsif ($char->{cha} <= 17) {
      $char->{charm} = "60%";
    } elsif ($char->{cha} <= 18) {
      $char->{charm} = "75%";
    }
  }
  if ($char->{cha} and not $char->{hirelings}) {
    if ($char->{cha} <= 4) {
      $char->{hirelings} = 1;
    } elsif ($char->{cha} <= 6) {
      $char->{hirelings} = 2;
    } elsif ($char->{cha} <= 8) {
      $char->{hirelings} = 3;
    } elsif ($char->{cha} <= 12) {
      $char->{hirelings} = 4;
    } elsif ($char->{cha} <= 15) {
      $char->{hirelings} = 5;
    } elsif ($char->{cha} <= 17) {
      $char->{hirelings} = 6;
    } elsif ($char->{cha} <= 18) {
      $char->{hirelings} = 7;
    }
  }
  if ($char->{wis} and not $char->{sanity}) {
    $char->{sanity} = $char->{wis};
  }
}

sub complete {
  my ($char, $one, $two) = @_;
  if ($char->{$one} and not $char->{$two}) {
    if ($char->{$one} > 20) {
      $char->{$two} = 0;
    } else {
      $char->{$two} = 20 - $char->{$one};
    }
  }
}

sub pendragon {
  my $char = shift;
  if ($char->{str} and $char->{siz} and not $char->{damage}) {
    $char->{damage} = int(($char->{str}+$char->{siz}) / 6 + 0.5) . T('d6');
  }
  if ($char->{str} and $char->{con} and not $char->{healing}) {
    $char->{healing} = int(($char->{str}+$char->{con}) / 10 + 0.5);
  }
  if ($char->{str} and $char->{dex} and not $char->{move}) {
    $char->{move} = int(($char->{str}+$char->{dex}) / 10 + 0.5);
  }
  if ($char->{con} and $char->{siz} and not $char->{hp}) {
    $char->{hp} = $char->{con}+$char->{siz};
  }
  if ($char->{hp} and not $char->{unconscious}) {
    $char->{unconscious} = int($char->{hp} / 4 + 0.5);
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
    complete($char, $one, $two);
    complete($char, $two, $one);
  }
}

sub acks {
  my $char = shift;
  for my $id (qw(str dex con int wis cha)) {
    if ($char->{$id} and not $char->{"$id-bonus"}) {
      $char->{"$id-bonus"} = bonus($char->{$id});
    }
  }
  if ($char->{attack} and not $char->{melee}) {
    $char->{melee} =  $char->{attack} - $char->{"str-bonus"};
  }
  if ($char->{attack} and not $char->{missile}) {
    $char->{missile} =  $char->{attack} - $char->{"dex-bonus"};
  }

  acks_saves($char);
}

# This function computes the values that are shown but that don't need to be
# stored.
sub freebooters {
  my $char = shift;

  # attribute bonus
  for my $id (qw(str dex con int wis cha luc)) {
    if ($char->{$id} and not $char->{"$id-bonus"}) {
      $char->{"$id-bonus"} = bonus($char->{$id});
    }
  }

  # Max Load
  if (not $char->{"max-load"}) {
    if ($char->{class} eq T('fighter')) {
      $char->{"load-bonus"} = 10;
    } elsif ($char->{class} eq T('thief')) {
      $char->{"load-bonus"} = 6;
    } elsif ($char->{class} eq T('cleric')) {
      $char->{"load-bonus"} = 8;
    } elsif ($char->{class} eq T('magic-user')) {
      $char->{"load-bonus"} = 4;
    }
    $char->{"max-load"} = $char->{"str-bonus"} + $char->{"load-bonus"};
  }

  # HD type
  if ($char->{class} eq T('fighter')) {
    $char->{"hd-type"} = "10";
  } elsif ($char->{class} eq T('thief')) {
    $char->{"hd-type"} = "6";
  } elsif ($char->{class} eq T('cleric')) {
    $char->{"hd-type"} = "8";
  } elsif ($char->{class} eq T('magic-user')) {
    $char->{"hd-type"} = "4";
  }

  # character sheet
  my $charsheet = "Maezar-Freebooters.svg";
  if (not $char->{"charsheet"} and -f "$home/$charsheet") {
    $char->{"charsheet"} = $charsheet;
  }
}

# This function is called when preparing data for display in SVG.
sub compute_data {
  my ($char, $language) = @_;
  local $lang = $language; # make sure T works as intended
  if (not exists $char->{rules}
      or not defined $char->{rules}
      or $char->{rules} eq "moldvay"
      or $char->{rules} eq "halberds-n-helmets"
      or $char->{rules} eq "labyrinth lord") {
    moldvay($char);
  } elsif ($char->{rules} eq "pendragon") {
    pendragon($char);
  } elsif ($char->{rules} eq "crypts-n-things") {
    crypts_n_things($char);
  } elsif ($char->{rules} eq "acks") {
    acks($char);
  } elsif ($char->{rules} eq "freebooters") {
    freebooters($char);
  } else {
    moldvay($char);
  }
}

sub starting_gold {
  my $char = shift;
  if ($char->{rules} eq "labyrinth lord") {
    return roll_3d8() * 10;
  }
  return roll_3d6() * 10;
}

my %price_cache;

sub equipment {
  my $char = shift;
  my $xp = $char->{xp};
  my $level = $char->{level};
  my $class = $char->{class};
  return if $xp or $level > 1 or not $class;

  get_price_cache($char);
  my $money = starting_gold();
  my @property;

  # free spellbook for arcane casters
  if (member($class, T('magic-user'), T('elf'), T('mage'),
	     T('elven spellsword'), T('elven nightblade'))) {
    push(@property, T('spell book'));
  }

  ($money, @property) = buy_basics($char, $money, $class, @property);
  ($money, @property) = buy_armor($char, $money, $class, @property);
  ($money, @property) = buy_weapon($char, $money, $class, @property);
  ($money, @property) = buy_tools($char, $money, $class, @property);
  ($money, @property) = buy_light($char, $money, $class, @property);
  ($money, @property) = buy_gear($char, $money, $class, @property);
  ($money, @property) = buy_protection($char, $money, $class, @property);
  my $gold = int($money);
  my $silver = int(10 * ($money - $gold) + 0.5);;
  push(@property, T('%0 gold', $gold)) if $gold;
  push(@property, T('%0 silver', $silver)) if $silver;
  provide($char, "property",  join("\\\\", @property));
}

sub get_price_cache {
  my $char = shift;
  my $i = 0; # the default is B/X
  if ($char->{rules} eq "acks") { $i = 2; }
  elsif ($char->{rules} eq "labyrinth lord") { $i = 1; }
  %price_cache = (
    T('backpack') => [5, 2, 2]->[$i],
    # ACKS: 1-6gp for one week, LL: 5sp/day, B/X: 15gp
    T('iron rations (1 week)') => [15, 3.5, 1]->[$i],
    T('holy symbol') => 25,
    T('thieves’ tools') => 25,
    T('lantern') => [10, 9, 10]->[$i],
    T('flask of oil') => [2, 0.1, 2]->[$i],
    T('torches') => [1, 0.3, 0.1]->[$i],
    T('rope') => 1,
    T('iron spikes and hammer') => [3, 1.5, 3]->[$i],
    T('wooden pole') => [1, 0.2, 0.1]->[$i],
    T('holy water') => 25,
    T('wolfsbane') => 10,
    T('mirror') => [5, 10, 5]->[$i],
    T('leather armor') => [20, 6, 20]->[$i],
    T('chain mail') => [40, 70, 40]->[$i],
    T('plate mail') => [60, 450, 60]->[$i],
    T('shield') => 10,
    T('helmet') => 10,
    T('club') => [3, 3, 1]->[$i],
    T('mace') => 5,
    T('war hammer') => [5, 7, 5]->[$i],
    T('staff') => [2, 2, 1]->[$i],
    T('dagger') => 3,
    T('silver dagger') => 30,
    T('two handed sword') => 15,
    T('battle axe') => [7, 6, 7]->[$i],
    T('pole arm') => 7,
    T('long sword') => 10,
    T('short sword') => 7,
    T('long bow') => [40, 40, 7]->[$i],
    T('quiver with 20 arrows') => [5, 5, 1]->[$i],
    T('short bow') => [25, 25, 3]->[$i],
    T('crossbow') => [30, 25, 30]->[$i],
    T('case with 30 bolts') => [10, 9, 3]->[$i],
    T('sling') => 2,
    T('pouch with 30 stones') => 0,
    T('hand axe') => [4, 1, 4]->[$i],
    T('spear') => 3,
      );
}

sub price {
  my ($char, $item) = @_;
  my $price = $price_cache{$item};
  if (not defined $price) {
    error(T('Unknown Price'), T('%0: How much does this cost?', $item));
  }
  return $price;
}

# add($item, \@property) modifies @property directly
sub add {
  my ($item, $property) = @_;
  foreach (@$property) {
    if ($_ eq $item) {
      if (/\(\d+\)$/) {
	my $n = $1++;
	s/\(\d+\)$/($n)/;
      } else {
	$_ .= " (2)";
      }
      $item = undef;
      last;
    }
  }
  if ($item) {
    push(@$property, $item);
  }
}

# Use array references to buy one of several alternatives.
# Buy a and b, or buy c instead:
# ($money, @property) = buy($char, [[a, b], c], $money, @property)
sub buy {
  my ($char, $item, $money, @property) = @_;
  if (ref $item eq "ARRAY") {
    for my $elem (@$item) {
      if (ref $elem eq "ARRAY") {
	my $price = 0;
	for my $thing (@$elem) {
	  $price += price($char, $thing);
	}
	if ($money >= $price) {
	  $money -= $price;
	  $elem->[-1] .= " (${price}gp)" if $char->{debug};
	  foreach (@$elem) {
	    add($_, \@property);
	  }
	  last;
	}
      } else {
	my $price = price($char, $elem);
	if ($money >= $price) {
	  $money -= $price;
	  $elem .= " (${price}gp)" if $char->{debug};
	  add($elem, \@property);
	  last;
	}
      }
    }
  } else {
    my $price = price($char, $item);
    if ($money >= $price) {
      $money -= $price;
      $item .= " (${price}gp)" if $char->{debug};
      add($item, \@property);
    }
  }
  return ($money, @property);
}

sub buy_basics {
  my ($char, $money, $class, @property) = @_;
  push(@property, "- $money gp -") if $char->{debug};
  ($money, @property) = buy($char, T('backpack'), $money, @property);
  ($money, @property) = buy($char, T('iron rations (1 week)'), $money, @property);
  
  return ($money, @property);
}

sub buy_tools {
  my ($char, $money, $class, @property) = @_;
  push(@property, "- $money gp -") if $char->{debug};
  if (member($class, T('cleric'), T('dwarven craftpriest'), T('bladedancer'))) {
    ($money, @property) = buy($char, T('holy symbol'), $money, @property);
  } elsif ($class eq T('thief')) {
    ($money, @property) = buy($char, T('thieves’ tools'), $money, @property);
  }
  return ($money, @property);
}

sub buy_light {
  my ($char, $money, $class, @property) = @_;
  push(@property, "- $money gp -") if $char->{debug};
  return buy($char, [[T('lantern'), T('flask of oil')],
	      T('torches')],
	     $money, @property);
}

sub buy_gear {
  my ($char, $money, $class, @property) = @_;
  push(@property, "- $money gp -") if $char->{debug};
  my @preferences = shuffle(
    T('rope'),
    T('iron spikes and hammer'),
    T('wooden pole'));
  return buy($char, \@preferences, $money, @property);
}

sub buy_protection {
  my ($char, $money, $class, @property) = @_;
  push(@property, "- $money gp -") if $char->{debug};
  my @preferences = shuffle(
    T('holy water'),
    T('wolfsbane'),
    T('mirror'));
  return buy($char, \@preferences, $money, @property);
}

sub buy_armor {
  my ($char, $money, $class, @property) = @_;
  push(@property, "- $money gp -") if $char->{debug};
  my $budget = $money / 2;
  $money -= $budget;

  if (member($class, T('magic-user'), T('mage'))) {
    # no armor
  } elsif (member($class, T('thief'), T('assassin'), T('bard'),
		  T('bladedancer'), T('elven nightblade'))) {
    # leather, no shield, no helmet
    ($budget, @property) = buy($char, T('leather armor'), $budget, @property);
  } elsif (member($class, T('explorer'))) {
    # chain and shield
    ($budget, @property) = buy($char, [T('chain mail'),
			       T('leather armor')], $budget, @property);
    ($budget, @property) = buy($char, T('shield'), $budget, @property);
    ($budget, @property) = buy($char, T('helmet'), $budget, @property);
  } else {
    # any armor
    ($budget, @property) = buy($char, [T('plate mail'),
				T('chain mail'),
				T('leather armor')], $budget, @property);
    ($budget, @property) = buy($char, T('shield'), $budget, @property);
    ($budget, @property) = buy($char, T('helmet'), $budget, @property);
  }
  
  # compute AC
  my $dex = $char->{dex};
  my $ac = 9 - bonus($dex);
  
  if (member(T('plate mail'), @property)) { $ac -= 6; }
  elsif (member(T('chain mail'), @property)) { $ac -= 4; }
  elsif (member(T('leather armor'), @property)) { $ac -= 2; }

  if (member(T('shield'), @property)) { $ac -= 1; }
  if ($class eq T('bladedancer')) { $ac -= $char->{level}; }

  # ACKS is ascending
  if ($char->{rules} eq "acks") { $ac = 9 - $ac; }
  
  provide($char, "ac",  $ac);

  return ($money + $budget, @property);
}

sub buy_melee_weapon {
  my $char = shift;
  my ($money, $class, @property) = @_;
  my $str = $char->{str};
  my $hp  = $char->{hp};
  my $shield = member(T('shield'), @property);
  my @preferences;
  
  if ($class eq T('cleric')) {
    @preferences = shuffle(
      T('mace'),
      T('war hammer'),
      T('club'),
      T('staff'));
  } elsif (member($class, T('magic-user'), T('mage'))) {
    @preferences = shuffle(
      T('dagger'),
      T('staff'));
  } else {
    if ($class eq T('fighter')
	and good($str)
	and $hp > 6
	and not $shield) {
      @preferences = (T('two handed sword'), T('battle axe'), T('pole arm'));
    }
    elsif (member($class, T('dwarf'), T('dwarven vaultguard'),
		  T('dwarven craftpriest'))
	   and not $shield) {
      @preferences = (T('battle axe'));
    }
    push(@preferences, T('long sword'), T('short sword'));
  }
  return buy($char, \@preferences, $money, @property);
}

sub buy_throwing_weapon {
  my $char = shift;
  my ($money, $class, @property) = @_;
  my @preferences;
  if (member($class, T('dwarf'), T('dwarven vaultguard'), T('dwarven craftpriest'))
      or member(T('battle axe'), @property)) {
    push(@preferences, [T('hand axe'), T('hand axe')]);
    push(@preferences, T('hand axe'));
  }
  if ($class eq T('fighter')) {
    push(@preferences, T('spear'));
  }
  return buy($char, \@preferences, $money, @property);
}

sub buy_ranged_weapon {
  my $char = shift;
  my ($money, $class, @property) = @_;
  my @preferences;
  my $dex = $char->{dex};
  if (($class eq T('fighter') or $class eq T('elf'))
      and average($dex)) {
    push(@preferences,
	 [T('long bow'),
	  T('quiver with 20 arrows'),
	  T('quiver with 20 arrows')],
	 [T('long bow'),
	  T('quiver with 20 arrows')]);
  }
  if (not(member($class, T('magic-user'), T('mage'), T('bladedancer'),
		 T('dwarven craftpriest')))) {
    if (not(member($class, T('cleric')))) {
      if (average($dex)) {
	push(@preferences,
	     [T('short bow'),
	      T('quiver with 20 arrows'),
	      T('quiver with 20 arrows')],
	     [T('short bow'),
	      T('quiver with 20 arrows')]);
      }
      push(@preferences,
	   [T('crossbow'),
	    T('case with 30 bolts')]);
    }
    push(@preferences,
	 [T('sling'),
	  T('pouch with 30 stones')]);
  }
  return buy($char, \@preferences, $money, @property);
}

sub buy_weapon {
  my $char = shift;
  my ($money, $class, @property) = @_;
  push(@property, "- $money gp -") if $char->{debug};
  my $budget = $money / 2;
  $money -= $budget;
  
  ($budget, @property) = buy_melee_weapon($char, $budget, $class, @property);
  ($budget, @property) = buy_throwing_weapon($char, $budget, $class, @property);
  ($budget, @property) = buy_ranged_weapon($char, $budget, $class, @property);

  if ($class ne T('cleric')) {
    ($budget, @property) = buy($char, T('silver dagger'), $budget, @property);
  }

  if ($class ne T('cleric') and $class ne T('magic-user')) {
    ($budget, @property) = buy($char, T('dagger'), $budget, @property);
    ($budget, @property) = buy($char, T('dagger'), $budget, @property);
  }

  return ($money + $budget, @property);
}

sub spellbook {
  my $char = shift;
  if ($char->{rules} eq "labyrinth lord") {
    return T('First level spells:') . "\\\\"
    . join("\\\\",
	   two(T('charm person'),
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
	       T('ventriloquism')),
	   T('second level spells:'),
	   one(T('arcane lock'),
	       T('continual light'),
	       T('detect evil'),
	       T('detect invisible'),
	       T('ESP'),
	       T('invisibility'),
	       T('knock'),
	       T('levitate'),
	       T('locate object'),
	       T('minor image'),
	       T('phantasmal force'),
	       T('web')));
  } else {
    return T('Spells:') . " "
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
}

sub moldvay_saves {
  my $char = shift;
  my $class = $char->{class};
  my $level = $char->{level};
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

  provide($char, "breath",  $breath) unless $char->{breath};
  provide($char, "poison",  $poison) unless $char->{poison};
  provide($char, "petrify",  $petrify) unless $char->{petrify};
  provide($char, "wands",  $wands) unless $char->{wands};
  provide($char, "spells",  $spells) unless $char->{spells};
}

sub acks_saves {
  my $char = shift;
  my $class = $char->{class};
  my $level = $char->{level};
  return unless $class and $level == 1;
  my ($petrify, $poison, $breath, $wands, $spells);
  if ($class eq T('fighter')) {
    ($petrify, $poison, $breath, $wands, $spells) =
      (15, 14, 16, 16, 17);
  } elsif ($class eq T('mage')) {
    ($petrify, $poison, $breath, $wands, $spells) =
      (13, 13, 15, 11, 12);
  } elsif (member($class, T('cleric'), T('bladedancer'))) {
    ($petrify, $poison, $breath, $wands, $spells) =
      (13, 10, 16, 13, 15);
  } elsif (member($class, T('thief'),  T('bard'))) {
    ($petrify, $poison, $breath, $wands, $spells) =
      (13, 13, 16, 14, 15);
  } elsif ($class eq T('assassin')) {
    ($petrify, $poison, $breath, $wands, $spells) =
      (15, 14, 16, 16, 17);
  } elsif ($class eq T('dwarven vaultguard')) {
    ($petrify, $poison, $breath, $wands, $spells) =
      (11, 10, 13, 12, 13);
  } elsif ($class eq T('dwarven craftpriest')) {
    ($petrify, $poison, $breath, $wands, $spells) =
      (9, 6, 13, 9, 11);
  } elsif ($class eq T('elven spellsword')) {
    ($petrify, $poison, $breath, $wands, $spells) =
      (14, 14, 16, 16, 16);
  } elsif ($class eq T('elven nightblade')) {
    ($petrify, $poison, $breath, $wands, $spells) =
      (12, 13, 16, 14, 14);
  }

  provide($char, "breath",  $breath) unless $char->{breath};
  provide($char, "poison",  $poison) unless $char->{poison};
  provide($char, "petrify",  $petrify) unless $char->{petrify};
  provide($char, "wands",  $wands) unless $char->{wands};
  provide($char, "spells",  $spells) unless $char->{spells};
}

sub d3 {
  return 1 + int(rand(3));
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

sub d10 {
  return 1 + int(rand(10));
}

sub d12 {
  return 1 + int(rand(12));
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

# use prototype so that Perl knows that there are only three arguments, which
# allows wrap to use wantarray when used to wrap $value, see freebooters_appearance
sub provide ($$$) {
  my ($char, $key, $value) = @_;
  push(@{$char->{provided}}, $key) unless $char->{$key};
  $char->{$key} = $value;
}

sub one {
  my $i = int(rand(scalar @_));
  return $_[$i];
}

sub two {
  my $i = int(rand(scalar @_));
  my $j = int(rand(scalar @_));
  $j = int(rand(scalar @_)) until $i != $j;
  return ($_[$i], $_[$j]);
}

sub member {
  my $element = shift;
  foreach (@_) {
    return 1 if $element eq $_;
  }
}

sub wrap {
  my ($text, $width) = @_;
  my @result;
  while (length($text) > $width) {
    my $n = $width;
    while ($n > 0) {
      if (substr($text, $n, 1) eq " ") {
	push(@result, substr($text, 0, $n));
	$text = substr($text, $n + 1);
	last;
      } else {
	$n--;
      }
    }
  }
  push(@result, $text);
  return @result if wantarray;
  return join("\\\\", @result);
}

# http://www.stadt-zuerich.ch/content/prd/de/index/statistik/publikationsdatenbank/Vornamen-Verzeichnis/VVZ_2012.html

my %names = qw{Aadhya F Aaliyah F Aanya F Aarna F Aarusha F Abiha F
Abira F Abisana F Abishana F Abisheya F Ada F Adalia F Adelheid F
Adelia F Adina F Adira F Adisa F Adisha F Adriana F Adriane F Adrijana
F Aela F Afriela F Agata F Agatha F Aicha F Aikiko F Aiko F Aila F
Ainara F Aischa F Aisha F Aissatou F Aiyana F Aiza F Aji F Ajshe F
Akksaraa F Aksha F Akshaya F Alaa F Alaya F Alea F Aleeya F Alegria F
Aleksandra F Alena F Alessandra F Alessia F Alexa F Alexandra F Aleyda
F Aleyna F Alia F Alice F Alicia F Aliena F Alienor F Aliénor F Alija
F Alina F Aline F Alisa F Alisha F Alissa F Alissia F Alix F Aliya F
Aliyana F Aliza F Alizée F Allegra F Allizza F Alma F Almira F Alva F
Alva-Maria F Alya F Alysha F Alyssa F Amalia F Amalya F Amanda F Amara
F Amaris F Amber F Ambra F Amea F Amelia F Amelie F Amélie F Amina F
Amira F Amor F Amora F Amra F Amy F Amy-Lou ? Ana F Anaahithaa F
Anabell F Anabella F Anaëlle F Anaïs F Ananya F Anastasia F Anastasija
F Anastazia F Anaya F Andeline F Andjela F Andrea F Anduena F Anela F
Anesa F Angel ? Angela F Angelina F Angeline F Anik ? Anika F Anila F
Anisa F Anise F Anisha F Anja F Ann F Anna F Anna-Malee F Annabel F
Annabelle F Annalena F Anne F Anne-Sophie F Annica F Annicka F Annigna
F Annik F Annika F Anouk F Antonet F Antonia F Antonina F Anusha F
Aralyn F Ariane F Arianna F Ariel ? Ariela F Arina F Arisa F Arishmi F
Arlinda F Arsema F Arwana F Arwen F Arya F Ashley F Ashmi F Asmin F
Astrid F Asya F Athena F Aubrey F Audrey F Aurelia F Aurélie F Aurora
F Ava F Avery F Avy F Aya F Ayana F Ayla F Ayleen F Aylin F Ayse F
Azahel F Azra F Barfin F Batoul F Batya F Beatrice F Belén F Bella F
Bente F Beril F Betel F Betelehim F Bleona F Bracha F Briana F Bronwyn
F Bruchi F Bruna F Büsra F Caelynn F Caitlin F Caja F Callista F
Camille F Cao F Carice F Carina F Carla F Carlotta F Carolina F
Caroline F Cassandra F Castille F Cataleya F Caterina F Catherine F
Céleste F Celia F Celina F Celine F Ceylin F Chana F Chanel F Chantal
F Charielle F Charleen F Charlie ? Charlize F Charlott F Charlotte F
Charly F Chavi F Chaya F Chiara F Chiara-Maé F Chinyere F Chléa F
Chloe F Chloé F Chrisbely F Christiana F Christina F Ciara F Cilgia F
Claire F Clara F Claudia F Clea F Cleo F Cleofe F Clodagh F Cloé F
Coco F Colette F Coral F Coralie F Cyrielle F Daliah F Dalila F
Dalilah F Dalina F Damiana F Damla F Dana F Daniela F Daria F Darija F
Dean ? Deborah F Déborah-Isabel F Defne F Delaila F Delia F Delina F
Derya F Deshira F Deva F Diana F Diara F Diarra F Diesa F Dilara F
Dina F Dinora F Djurdjina F Dominique F Donatella F Dora F Dorina F
Dunja F Eda F Edessa F Edith F Edna F Eduina F Eidi F Eileen F Ela F
Elanur F Elda F Eldana F Elea F Eleanor F Elena F Eleni F Elenor F
Eleonor F Eleonora F Elhana F Eliana F Elidiana F Eliel F Elif F Elin
F Elina F Eline F Elinor F Elisa F Elisabeth F Elise F Eliska F Eliza
F Ella F Ellen F Elliana F Elly F Elma F Elodie F Elona F Elora F Elsa
F Elva F Elyssa F Emelie F Emi F Emilia F Emiliana F Emilie F Émilie F
Emilija F Emily F Emma F Enis F Enna F Enrica F Enya F Erdina F Erika
F Erin F Erina F Erisa F Erna F Erona F Erva F Esma F Esmeralda F
Estée F Esteline F Estelle F Ester F Esther F Eteri F Euphrasie F Eva
F Eve F Evelin F Eviana F Evita F Ewa F Eya F Fabia F Fabienne F
Fatima F Fatma F Fay F Faye F Fe F Fedora F Felia F Felizitas F Fiamma
F Filipa F Filippa F Filomena F Fina F Finja F Fiona F Fjolla F
Flaminia F Flavia F Flor del Carmen F Flora F Florence F Florina F
Florita F Flurina F Franca F Francesca F Francisca F Franziska F
Freija F Freya F Freyja F Frida F Gabriela F Gabrielle F Gaia F
Ganiesha F Gaon F Gavisgaa F Gemma F Georgina F Ghazia F Gia F Giada F
Gianna F Gila F Gioanna F Gioia F Giorgia F Gitty F Giulia F Greta F
Grete F Gwenaelle F Gwendolin F Gyane F Hadidscha F Hadzera F Hana F
Hanar F Hania F Hanna F Hannah F Hanni F Hédi ? Heidi F Helen F Helena
F Helene F Helia F Heloise F Héloïse F Helya F Henna F Henrietta F
Heran F Hermela F Hiba F Hinata F Hiteshree F Hodman F Honey F Iara F
Ibtihal F Ida F Idil F Ilaria F Ileenia F Ilenia F Iman F Ina F Indira
F Ines F Inés F Inez F Ira F Irene F Iria F Irina F Iris F Isabel F
Isabela F Isabell F Isabella F Isabelle F Isra F Iva F Jada F Jael F
Jaël F Jaelle F Jaelynn F Jalina F Jamaya F Jana F Jane F Jannatul F
Jara F Jasmijn F Jasmin F Jasmina F Jayda F Jeanne F Jelisaveta F
Jemina F Jenna F Jennifer F Jerishka F Jessica F Jesuela F Jil F Joan
? Joana F Joanna F Johanna F Jola F Joleen F Jolie F Jonna F Joseline
F Josepha F Josephine F Joséphine F Joudia F Jovana F Joy F Judith F
Jule F Juli F Julia F Julie F Juliette F Julija F Jully F Juna F Juno
F Justine F Kahina F Kaja F Kalina F Kalista F Kapua F Karina F Karla
F Karnika F Karolina F Kashfia F Kassiopeia F Kate F Katerina F
Katharina F Kaya F Kayla F Kayley F Kayra F Kehla F Keira F
Keren-Happuch F Keziah F Khadra F Khardiata F Kiana F Kiara F Kim F
Kinda F Kira F Klara F Klea F Kostana F Kristina F Kristrún F Ksenija
F Kugagini F Kyra F Ladina F Laetitia F Laila F Laís F Lakshmi F Lana
F Lani F Lara F Laraina F Larina F Larissa F Laura F Laurelle F Lauri
? Laurianne F Lauryn F Lavin F Lavinia F Laya F Layana F Layla F Lea F
Léa F Leah F Leana F Leandra F Leanne F Leia F Leilani-Yara F Lejla F
Lelia F Lena F Leni F Lenia F Lenie F Lenja F Lenka F Lennie ? Leona F
Leoni F Leonie F Léonie F Leonor F Leticia F Leya F Leyla F Leyre F
Lia F Liana F Liane F Liann F Lianne F Liara F Liayana F Liba F Lidia
F Lidija F Lijana F Lila-Marie F Lili F Lilia F Lilian F Liliane F
Lilijana F Lilith F Lilja F Lilla F Lilli F Lilly F Lilly-Rose F Lilo
F Lily F Lin F Lina F Linda F Line F Linn F Lioba F Liora F Lisa F
Lisandra F Liselotte F Liv F Liva F Livia F Liz F Loa F Loe F Lokwa F
Lola F Lorea F Loreen F Lorena F Loriana F Lorina F Lorisa F Lotta F
Louanne F Louisa F Louise F Lovina F Lua F Luana F Luanda F Lucia F
Luciana F Lucie F Lucy F Luisa F Luise F Lux ? Luzia F Lya F Lyna F
Lynn F Lynna F Maëlle F Maelyn F Maëlys F Maeva F Magali F Magalie F
Magdalena F Mahsa F Maira F Maisun F Maja F Maka F Malaeka F Malaika F
Malea F Maléa F Malia F Malin F Malkif F Malky F Maltina F Malu F
Manar F Manha F Manisha F Mara F Maram F Mare F Mareen F Maren F
Margarida F Margherita F Margo F Margot F Maria F Mariangely F Maribel
F Marie F Marie-Alice F Marietta F Marija F Marika F Mariko F Marina F
Marisa F Marisol F Marissa F Marla F Marlen F Marlene F Marlène F
Marlin F Marta F Martina F Martje F Mary F Maryam F Mascha F Mathilda
F Matilda F Matilde F Mauadda F Maxine F Maya F Mayas F Mayla F Maylin
F Mayra F Mayumi F Medea F Medina F Meena F Mehjabeen F Mehnaz F Meila
F Melanie F Mélanie F Melek F Melian F Melike F Melina F Melisa F
Melissa F Mélissa F Melka F Melyssa F Mena F Meret F Meri F Merry F
Meryem F Meta F Mia F Mía F Michal F Michelle F Mihaela F Mila F
Milania F Milena F Milica F Milja F Milla F Milou F Mina F Mingke F
Minna F Minu F Mira F Miray F Mirdie F Miriam F Mirjam F Mirta F Miya
F Miyu F Moa F Moena F Momo F Momoco F Mona F Morea F Mubera F Muriel
F Mylène F Myriam F N'Dea F Nabihaumama F Nadija F Nadin F Nadja F
Nael ? Naemi F Naila F Naïma F Naina F Naliya F Nandi F Naomi F Nara F
Naraya F Nardos F Nastasija F Natalia F Natalina F Natania F Natascha
F Nathalie F Nava F Navida F Navina F Nayara F Nea F Neda F Neea F
Nejla F Nela F Nepheli F Nera F Nerea F Nerine F Nesma F Nesrine F
Neva F Nevia F Nevya F Nico ? Nicole F Nika F Nikita F Nikolija F
Nikolina F Nina F Nine F Nirya F Nisa F Nisha F Nives F Noa ? Noé ?
Noë F Noée F Noelia F Noemi F Noémie F Nola F Nora F Nordon F Norea F
Norin F Norina F Norlha F Nour F Nova F Nóva F Nubia F Nura F Nurah F
Nuray F Nuria F Nuriyah F Nusayba F Oceane F Oda F Olive F Olivia F
Olsa F Oluwashayo F Ornela F Ovia F Pamela-Anna F Paola F Pattraporn F
Paula F Paulina F Pauline F Penelope F Pepa F Perla F Pia F Pina F
Rabia F Rachel F Rahel F Rahela F Raïssa F Raizel F Rajana F Rana F
Ranim F Raphaela F Raquel F Rayan ? Rejhana F Rejin F Réka F Renata F
Rhea F Rhynisha-Anna F Ria F Riga F Rijona F Rina F Rita F Rivka F
Riya F Roberta F Robin ? Robyn F Rohzerin F Róisín F Romina F Romy F
Ronja F Ronya F Rosa F Rose F Rosina F Roxane F Royelle F Rozen F
Rubaba F Rubina F Ruby F Rufina F Rukaye F Rumi ? Rym F Saanvika F
Sabrina F Sadia F Safiya F Sahira F Sahra F Sajal F Salma F Salome F
Salomé F Samantha F Samina F Samira F Samira-Aliyah F Samira-Mukaddes
F Samruddhi F Sania F Sanna F Sara F Sarah F Sarahi F Saraia F Saranda
F Saray F Sari F Sarina F Sasha F Saskia F Savka F Saya F Sayema F
Scilla F Sejla F Selene F Selihom F Selina F Selma F Semanur F Sena F
Sephora F Serafima F Serafina F Serafine F Seraina F Seraphina F
Seraphine F Serena F Serra F Setareh F Shan F Shanar F Shathviha F
Shayenna F Shayna F Sheindel F Shireen F Shirin F Shiyara F Shreshtha
F Sia F Sidona F Siena F Sienna F Siiri F Sila F Silja F
Silvanie-Alison F Silvia F Simea F Simi F Simona F Sina F Sira F
Sirajum F Siri F Sirija F Sivana F Smilla F Sofia F Sofia-Margarita F
Sofie F Sofija F Solea F Soleil F Solène F Solenn F Sonia F Sophia F
Sophie F Sora F Soraya F Sosin F Sriya F Stella F Stina F Su F Subah F
Suela F Suhaila F Suleqa F Sumire F Summer F Syria F Syrina F Tabea F
Talina F Tamara F Tamasha F Tamina F Tamiya F Tara F Tatjana F Tayla F
Tayscha F Tea F Tenzin ? Teodora F Tessa F Tharusha F Thea F Theniya F
Tiana F Tijana F Tilda F Timea F Timeja F Tina F Tomma F Tonia F
Tsiajara F Tuana F Tyra F Tzi Ying F Uendi F Uma F Urassaya F Vailea F
Valentina F Valentine F Valeria F Valerie F Vanessa F Vanja F Varshana
F Vella F Vera F Victoria F Viktoria F Vinda F Viola F Vivianne F
Vivien F Vivienne F Wanda F Wayane F Wilma F Xin F Xingchen F Yael F
Yaël F Yamina F Yara F Yasmine F Yeilin F Yen My F Yersalm F Yesenia F
Yeva F Yi Nuo F Yildiz-Kiymet F Yixin F Ylvi F Yocheved F Yoko F Yosan
F Yosmely F Yuen F Yuhan F Yuna F Yvaine F Zahraa F Zaina F Zazie F
Zeinab F Zelda F Zeliha F Zenan F Zerya F Zeta F Zeyna F Zeynep F
Ziporah F Zivia F Zoe F Zoé F Zoë F Zoë-Sanaa F Zoey F Zohar F Zoi F
Zuri F Aadil M Aaron M Abdimaalik M Abdirahman M Abdul M Abdullah M
Abi M Abraham M Abrar M Abubakar M Achmed M Adam M Adan M Adesh M
Adhrit M Adil M Adiyan M Adrian M Adriano M Adrien M Adrijan M Adthish
M Advay M Advik M Aeneas M Afonso M Agustín M Ahammed M Ahnaf M Ahron
M Aiden M Ailo M Aimo M Ajan M Ajdin M Ajish M Akil M Akilar M Akira M
Akito M Aksayan M Alan M Aldin M Aldion M Alec M Alejandro M Aleksa M
Aleksandar M Aleksander M Aleksandr M Alem M Alessandro M Alessio M
Alex M Alexander M Alexandre M Alexandru M Alexey M Alexis M Alfred M
Ali M Allison M Almir M Alois M Altin M Aly M Amael M Aman M Amar M
Amaury M Amedeo M Ami M Amil M Amin M Amir M Amirhan M Amirthesh M
Ammar M Amogh M Anaël M Anakin M Anas M Anatol M Anatole M Anay M
Anayo M Andi M Andreas M Andrej M Andrés M Andrey M Andri M Andrin M
Andriy M Andy M Aneesh M Anes M Angelo M Anoush M Anqi M Antoine M
Anton M Antonio M António M Anua M Anush M Arab M Arafat M Aramis M
Aras M Arbion M Arda M Ardit M Arham M Arian M Arianit M Arijon M Arin
M Aris M Aritra M Ariya M Arlind M Arman M Armin M Arnaud M Arne M
Arno M Aron M Arsène M Art M Artemij M Arthur M Arturo M Arvid M Arvin
M Aryan M Arye M Aswad M Atharv M Attila M Attis M Aulon M Aurel M
Aurelio M Austin M Avinash M Avrohom M Axel M Ayan M Ayano M Ayham M
Ayman M Aymar M Aymon M Azaan M Azad M Azad-Can M Bailey M Balthazar M
Barnaba M Barnabas M Basil M Basilio M Bátor M Beda M Bela M Ben M
Benart M Benjamin M Bennet M Benno M Berend M Berktan M Bertal M Besir
M Bilal M Bilgehan M Birk M Bjarne M Bleart M Blend M Blendi M Bo M
Bogdan M Bolaji M Bora M Boris M Brady M Brandon M Breyling M Brice M
Bruce M Bruno M Bryan M Butrint M Caleb M Camil M Can M Cário M Carl M
Carlo M Carlos M Carmelo M Cas M Caspar M Cedric M Cédric M Célestin M
Celestino M Cemil-Lee M César M Chaim M Chandor M Charles M Chilo M
Chris M Christian M Christopher M Christos M Ciaran M Cillian M Cla M
Claudio M Colin M Collin M Connor M Conrad M Constantin M Corey M
Cosmo M Cristian M Curdin M Custavo M Cynphael M Cyprian M Cyrill M
Daan M Dagemawi M Daha M Dalmar M Damian M Damián M Damien M Damjan M
Daniel M Daniele M Danilo M Danny M Dareios M Darel M Darian M Dario M
Daris M Darius M Darwin M Davi M David M Dávid M Davide M Davin M
Davud M Denis M Deniz M Deon M Devan M Devin M Diago M Dian M Diar M
Diego M Dilom M Dimitri M Dino M Dion M Dionix M Dior M Dishan M
Diyari M Djamal M Djamilo M Domenico M Dominic M Dominik M Donart M
Dorian M Dries M Drisar M Driton M Duart M Duarte M Durgut M Durim M
Dylan M Ebu M Ebubeker M Edgar M Edi M Edon M Édouard M Edrian M
Edward M Edwin M Efehan M Efraim M Ehimay M Einar M Ekrem M Eldi M
Eldian M Elia M Eliah M Elias M Elija M Elijah M Elio M Eliot M Elliot
M Elouan M Élouan M Eloy M Elvir M Emanuel M Emil M Emilio M Emin M
Emir M Emmanuel M Endrit M Enea M Enes M Engin M Engjëll M Ennio M
Enrico M Enrique M Ensar M Enzo M Erblin M Erd M Eren M Ergin M Eric M
Erik M Erind M Erion M Eris M Ernest-Auguste M Erol M Eron M Ersin M
Ervin M Erwin M Essey M Ethan M Etienne M Evan M Ewan M Eymen M Ezio M
Fabian M Fabiàn M Fabio M Fabrice M Fadri M Faris M Faruk M Federico M
Félicien M Felipe M Felix M Ferdinand M Fernando M Filip M Filipe M
Finlay M Finn M Fionn M Firat M Fitz-Patrick M Flavio M Flori M
Florian M Florin M Flurin M Flynn M Francesco M Frederic M Frederick M
Frederik M Frédo M Fridtjof M Fritz M Furkan M Fynn M Gabriel M
Gabriele M Gael M Galin M Gaspar M Gaspard M Gavin M Geeth M Genc M
Georg M Gerald M Geronimo M Getoar M Gian M Gian-Andri M Gianluca M
Gianno M Gibran M Gibril M Gil M Gil-Leo M Gilles M Gion M Giona M
Giovanni M Giuliano M Giuseppe M Glen M Glenn M Gonçalo M Gondini M
Gregor M Gregory M Güney M Guilien M Guillaume M Gustav M Gustavo M
Gusti M Haakon M Haci M Hadeed M Halil M Hamad M Hamid M Hamza M
Hannes M Hans M Hari M Haris M Harry M Hassan M Heath M Hektor M
Hendri M Henri M Henrik M Henry M Henus M Hugo M Hussein M Huw M Iago
M Ian M Iasu M Ibrahim M Idan M Ieremia M Ifran M Iheb M Ikechi M Ilai
M Ilarion M Ilian M Ilias M Ilja M Ilyes M Ioav M Iorek M Isaac M Isak
M Ishaan M Ishak M Isi M Isidor M Ismael M Ismaël M Itay M Ivan M Iven
M Ivo M Jack M Jacob M Jacques M Jaden M Jae-Eun M Jago M Jahongir M
Jake M Jakob M Jakov M Jakub M Jamal M Jamen M James M Jamie M Jamiro
M Jan M Janick M Janis M Jann M Jannes M Jannik M Jannis M Janos M
János M Janosch M Jari M Jaron M Jasha M Jashon M Jason M Jasper M
Javier M Jawhar M Jay M Jayden M Jayme M Jean M Jechiel M Jemuël M
Jens M Jeremias M Jeremy M Jerlen M Jeroen M Jérôme M Jerun M Jhun M
Jim M Jimmy M Jitzchak M Joah M Joaquin M Joel M Joël M Johan M Johann
M Johannes M Johansel M John M Johnny M Jon M Jona M Jonah M Jonas M
Jonathan M Joona M Jordan M Jorin M Joris M Jose M Josef M Joseph-Lion
M Josh M Joshua M Jovan M Jovin M Jules M Julian M Julien M Julius M
Jun-Tao M Junior M Junis M Juri M Jurij M Justin M Jythin M Kaan M
Kailash M Kaitos M Kajeesh M Kajetan M Kardo M Karim M Karl M
Karl-Nikolaus M Kasimir M Kaspar M Kassim M Kathiravan M Kaynaan M
Kaynan M Keanan M Keano M Kejwan M Kenai M Kennedy M Kento M Kerim M
Kevin M Khodor M Kian M Kieran M Kilian M Kimon M Kiran M Kiyan M Koji
M Konrad M Konstantin M Kosmo M Krishang M Krzysztof M Kuzey M Kyan M
Kyle M Labib M Lakishan M Lamoral M Lanyu M Laris M Lars M Larton M
Lasse M Laurent M Laurenz M Laurin M Lawand M Lawrence M Lazar M Lean
M Leander M Leandro M Leano M Leart M Leas M Leen M Leif M Len M
Lenart M Lend M Lendrit M Lenert M Lenn M Lennard M Lennart M Lenno M
Lennox M Lenny M Leno M Leo M Leon M León M Léon M Leonard M Leonardo
M Leonel M Leonidas M Leopold M Leopoldo M Leron M Levi M Leviar M
Levin M Levis M Lewis M Liam M Lian M Lias M Liél M Lieven M Linard M
Lino M Linor M Linus M Linus-Lou M Lio M Lion M Lionel M Lior M Liun M
Livio M Lizhang M Lloyd M Logan M Loïc M Lois M Long Yang M Lono M
Lorenz M Lorenzo M Lorian M Lorik M Loris M Lou M Louay M Louis M
Lovis M Lowell M Luan M Luc M Luca M Lucas M Lucian M Lucien M Lucio M
Ludwig M Luis M Luís M Luk M Luka M Lukas M Lumen M Lyan M Maaran M
Maddox M Mads M Mael M Maél M Máel M Mahad M Mahir M Mahmoud M Mailo M
Maksim M Maksut M Malik M Manfred M Máni M Manuel M Manuele M Maor M
Marc M Marcel M Marco M Marek M Marino M Marius M Mark M Marko M
Markus M Marley M Marlon M Marouane M Marti M Martim M Martin M Marvin
M Marwin M Mason M Massimo M Matay M Matej M Mateja M Mateo M Matheo M
Mathéo M Matheus M Mathias M Mathieu M Mathis M Matia M Matija M
Matisjohu M Mats M Matteo M Matthew M Matthias M Matthieu M Matti M
Mattia M Mattis M Maurice M Mauricio M Maurin M Maurizio M Mauro M
Maurus M Max M Maxence M Maxim M Maxime M Maximilian M Maximiliano M
Maximilien M Maxmilan M Maylon M Median M Mehmet M Melis M Melvin M
Memet M Memet-Deniz M Menachem M Meo M Meris M Merlin M Mert M Mete M
Methma M Mias M Micah M Michael M Michele M Miguel M Mihailo M Mihajlo
M Mika M Mike M Mikias M Mikka M Mikko M Milad M Milan M Milo M Milos
M Minyou M Mio M Miran M Miraxh M Miro M Miron M Mishkin M Mithil M
Mohamed M Mohammed M Moische M Momodou M Mordechai M Moreno M Moritz M
Morris M Moses M Mubaarak M Muhamet M Muhammad M Muhammed M Muhannad M
Muneer M Munzur M Mustafa M Nadir M Nahuel M Naïm M Nando M Nasran M
Nathan M Nathanael M Natnael M Nelio M Nelson M Nenad M Neo M Néo M
Nepomuk M Nevan M Nevin M Nevio M Nic M Nick M Nick-Nolan M Niclas M
Nicolas M Nicolás M Niilo M Nik M Nikhil M Niklas M Nikola M Nikolai M
Nikos M Nilas M Nils M Nima M Nimo M Nino M Niven M Nnamdi M Noah M
Noam M Noan M Noè M Noel M Noël M Nurhat M Nuri M Nurullmubin M
Odarian M Odin M Ognjen M Oliver M Olufemi M Omar M Omer M Ömer M
Orell M Orlando M Oscar M Oskar M Osman M Otávio M Otto M Ousmane M
Pablo M Pablo-Battista M Paolo M Paris M Pascal M Patrice M Patrick M
Patrik M Paul M Pavle M Pawat M Pax M Paxton M Pedro M Peppino M
Pessach M Peven M Phil M Philemon M Philip M Philipp M Phineas M
Phoenix-Rock M Piero M Pietro M Pio M Pjotr M Prashanth M Quentin M
Quinnlan M Quirin M Rafael M Raffael M Raffaele M Rainer M Rami M Ramí
M Ran M Raoul M Raphael M Raphaël M Rasmus M Raúl M Ray M Rayen M
Reban M Reda M Refoel M Rejan M Relja M Remo M Remy M Rémy M Rénas M
Rens M Resul M Rexhep M Rey M Riaan M Rian M Riccardo M Richard M Rico
M Ridley M Riley M Rimon M Rinaldo M Rio M Rion M Riyan M Riza M Roa M
Roald M Robert M Rodney-Jack M Rodrigo M Roman M Romeo M Ronan M Rory
M Rouven M Roy M Ruben M Rúben M Rubino M Rufus M Ryan M Saakith M
Saatvik M Sabir M Sabit M Sacha M Sahl M Salaj M Salman M Salomon M
Sami M Sami-Abdeljebar M Sammy M Samuel M Samuele M Samy M Sandro M
Santiago M Saqlain M Saranyu M Sascha M Sava M Schloime M Schmuel M
Sebastian M Sebastián M Selim M Semih M Semir M Semyon M Senthamil M
Serafin M Seraphin M Seth M Sevan M Severin M Seya M Seymen M Seymour
M Shafin M Shahin M Shaor M Sharon M Shayaan M Shayan M Sheerbaz M
Shervin M Shian M Shiraz M Shlomo M Shon M Siddhesh M Silas M Sileye M
Silvan M Silvio M Simeon M Simon M Sirak M Siro M Sivan M Soel M Sol M
Solal M Souleiman M Sriswaralayan M Sruli M Stefan M Stefano M Stephan
M Steven M Stian M Strahinja M Sumedh M Suryansh M Sven M Taavi M Taha
M Taner M Tanerau M Tao M Tarik M Tashi M Tassilo M Tayshawn M
Temuulen M Teo M Teris M Thelonious M Thenujan M Theo M Theodor M
Thiago M Thierry M Thies M Thijs M Thilo M Thom M Thomas M Thor M
Tiago M Tiemo M Til M Till M Tilo M Tim M Timo M Timon M Timothée M
Timotheos M Timothy M Tino M Titus M Tjade M Tjorben M Tobias M Tom M
Tomás M Tomeo M Tosco M Tristan M Truett M Tudor M Tugra M Turan M
Tyson M Uari M Uros M Ursin M Usuy M Uwais M Valentin M Valerian M
Valerio M Vangelis M Vasilios M Vico M Victor M Viggo M Vihaan M
Viktor M Villads M Vincent M Vinzent M Vinzenz M Vito M Vladimir M
Vleron M Vo M Vojin M Wander M Wanja M William M Wim M Xavier M Yaakov
M Yadiel M Yair M Yamin M Yanhao M Yanic M Yanik M Yanis M Yann M
Yannick M Yannik M Yannis M Yardil M Yared M Yari M Yasin M Yasir M
Yavuz M Yecheskel M Yehudo M Yeirol M Yekda M Yellyngton M Yiannis M
Yifan M Yilin M Yitzchok M Ylli M Yoan M Yohannes M Yonatan M Yonathan
M Yosias M Younes M Yousef M Yousif M Yousuf M Youwei M Ysaac M Yuma M
Yussef M Yusuf M Yves M Zaim M Zeno M Zohaq M Zuheyb M Zvi M};

sub name {
  return one(keys %names);
}

my $traits = {
  # http://charaktereigenschaften.miroso.de/
  de => [qw{
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
überreagierend überschwenglich übersensibel überspannt überwältigent}],
# http://www.roleplayingtips.com/tools/1000-npc-traits/
en => [qw{able abrasive abrupt absent minded abusive accepting
accident prone accommodating accomplished action oriented active
adaptable substance abusing adorable adventurous affable affected
affectionate afraid uncommited aggressive agnostic agreeable alert
alluring aloof altruistic always hungry always late ambiguous
ambitious amiable amused amusing angry animated annoyed annoying
anti-social anxious apathetic apologetic appreciative apprehensive
approachable argumentative aristocratic arrogant artistic ashamed
aspiring assertive astonished attentive audacious austere
authoritarian authoritative available average awful awkward babbling
babyish bad bashful beautiful belligerent bewildered biter
blames others blasé blowhard boastful boisterous bold boorish bored
boring bossy boundless brainy brash bratty brave brazen bright
brilliant brotherly brutish bubbly busy calculating callous calm
candid capable capricious carefree careful careless caring caustic
cautious changeable charismatic charming chaste cheerful cheerless
childish chivalrous civilised classy clean clever close closed clumsy
coarse cocky coherent cold cold hearted combative comfortable
committed communicative compassionate competent complacent compliant
composed compulsive conceited concerned condescending confident
confused congenial conscientious considerate consistent constricting
content contented contrarian contrite controlling conversational
cooperative coquettish courageous courteous covetous cowardly cowering
coy crabby crafty cranky crazy creative credible creepy critical cross
crude cruel cuddly cultured curious cutthroat cynical dainty dangerous
daring dark dashing dauntless dazzling debonair deceitful deceiving
decent decisive decorous deep defeated defective deferential defiant
deliberate delicate delightful demanding demonic dependable dependent
depressed deranged despicable despondent detached detailed determined
devilish devious devoted dignified diligent direct disaffected
disagreeable discerning disciplined discontented discouraged discreet
disgusting dishonest disillusioned disinterested disloyal dismayed
disorderly disorganized disparaging disrespectful dissatisfied
dissolute distant distraught distressed disturbed dogmatic domineering
dorky doubtful downtrodden draconian dramatic dreamer dreamy dreary
dubious dull dumb dutiful dynamic eager easygoing eccentric educated
effervescent efficient egocentric egotistic elated eloquent
embarrassed embittered embraces change eminent emotional empathetic
enchanting encouraging enduring energetic engaging enigmatic
entertaining enthusiastic envious equable erratic ethical evasive evil
exacting excellent excessive excitable excited exclusive expansive
expert extravagant extreme exuberant fabulous facetious faded fair
faith in self faithful faithless fake fanatical fanciful fantastic
fatalistic fearful fearless feisty ferocious fidgety fierce fiery
fighter filthy fine finicky flagging flakey flamboyant flashy fleeting
flexible flighty flippant flirty flustered focused foolish forceful
forgetful forgiving formal fortunate foul frank frantic fresh fretful
friendly frightened frigid frugal frustrated fuddy duddy fun
fun loving funny furious furtive fussy gabby garrulous gaudy generous
genial gentle giddy giggly gives up easily giving glamorous gloomy
glorious glum goal orientated good goofy graceful gracious grandiose
grateful greedy gregarious grieving grouchy growly gruesome gruff
grumpy guarded guilt ridden guilty gullible haggling handsome happy
hard hard working hardy harmonious harried harsh hateful haughty
healthy heart broken heartless heavy hearted hedonistic helpful
helpless hesitant high high self esteem hilarious homeless honest
honor bound honorable hopeful hopeless hormonal horrible hospitable
hostile hot headed huffy humble humorous hurt hysterical ignorant ill
ill-bred imaginative immaculate immature immobile immodest impartial
impatient imperial impolite impotent impractical impudent impulsive
inactive incoherent incompetent inconsiderate inconsistent indecisive
independent indifferent indiscrete indiscriminate indolent indulgent
industrious inefficient inept inflexible inimitable innocent
inquisitive insecure insensitive insightful insincere insipid
insistent insolent instinctive insulting intellectual intelligent
intense interested interrupting intimidating intolerant intrepid
introspective introverted intuitive inventive involved irresolute
irresponsible irreverent irritable irritating jackass jaded jealous
jittery joking jolly jovial joyful joyous judgmental keen kenderish
kind hearted kittenish knowledgeable lackadaisical lacking languid
lascivious late lazy leader lean lethargic level lewd liar licentious
light-hearted likeable limited lineat lingering lively logical lonely
loquacious lordly loud loudmouth lovable lovely loves challenge loving
low confidence lowly loyal lucky lunatic lying macho mad malicious
manipulative mannerly materialistic matronly matter-of-fact mature
mean meek melancholy melodramatic mentally slow merciful mercurial
messy meticulous mild mischievous miserable miserly mistrusting modern
modest moody moping moralistic motherly motivated mysterious nagging
naive narcissistic narrow-minded nasty naughty neat
needs social approval needy negative negligent nervous neurotic
never hungry nibbler nice night owl nihilistic nimble nit picker
no purpose no self confidence noble noisy nonchalant nosy
not trustworthy nuanced nuisance nurturing nut obedient obese obliging
obnoxious obscene obsequious observant obstinate odd odious open
open-minded opinionated opportunistic optimistic orcish orderly
organized ornery ossified ostentatious outgoing outrageous outspoken
overbearing overweight overwhelmed overwhelming paces pacifistic
painstaking panicky paranoid particular passionate passive
passive-aggressive pathetic patient patriotic peaceful penitent
pensive perfect perfectionist performer perserverant perseveres
persevering persistent persuasive pert perverse pessimistic petty
petulant philanthropic picky pious pitiful placid plain playful
pleasant pleasing plotting plucky polite pompous poor popular positive
possessive practical precise predictable preoccupied pretentious
pretty prim primitive productive profane professional promiscuous
proper protective proud prudent psychotic puckish punctilious punctual
purposeful pushy puzzled quarrelsome queer quick quick tempered quiet
quirky quixotic rambunctious random rash rational rawboned realistic
reasonable rebellious recalcitrant receptive reckless reclusive
refined reflective regretful rejects change relaxed relents reliable
relieved religious reluctant remorseful repugnant repulsive resentful
reserved resilient resolute resourceful respectful responsible
responsive restless retiring rhetorical rich right righteous rigid
risk-taking romantic rough rowdy rude rugged ruthless sacrificing sad
sadistic safe sagely saintly salient sanctimonious sanguine sarcastic
sassy satisfied saucy savage scared scarred scary scattered scheming
scornful scrawny scruffy secretive secure sedate seductive selective
self-centered self-confident self-conscious self-controlling
self-directed self-disciplined self-giving self-reliant self-serving
selfish selfless senile sensitive sensual sentimental serene serious
sexual sexy shallow shameless sharp sharp-tongued sharp-witted
sheepish shiftless shifty short shrewd shy silent silky silly simian
simple sincere sisterly skillful sleazy sloppy slovenly slow paced
slutty sly small-minded smart smiling smooth sneaky snob sociable
soft-hearted soft-spoken solitary sore sorry sour spendthrift spiteful
splendid spoiled spontaneous spunky squeamish stately static steadfast
sterile stern stimulating stingy stoical stolid straight laced strange
strict strident strong strong willed stubborn studious stupid suave
submissive successful succinct sulky sullen sultry supercilious
superstitious supportive surly suspicious sweet sympathetic systematic
taciturn tacky tactful tactless talented talkative tall tardy tasteful
temperamental temperate tenacious tense tentative terrible terrified
testy thankful thankless thick skinned thorough thoughtful thoughtless
threatening thrifty thrilled tight timid tired tireless tiresome
tolerant touchy tough trivial troubled truculent trusting trustworthy
truthful typical ugly unappreciative unassuming unbending unbiased
uncaring uncommitted unconcerned uncontrolled unconventional
uncooperative uncoordinated uncouth undependable understanding
undesirable undisciplined unenthusiastic unfeeling unfocused
unforgiving unfriendly ungrateful unhappy unhelpful uninhibited unkind
unmotivated unpredictable unreasonable unreceptive unreliable
unresponsive unrestrained unruly unscrupulous unselfish unsure
unsympathetic unsystematic unusual unwilling upbeat upset uptight
useful vacant vague vain valiant vengeful venomous verbose versatile
vigorous vindictive violent virtuous visual vivacious volatile
voracious vulgar vulnerable warlike warm hearted wary wasteful weak
weary weird well grounded whimsical wholesome wicked wild willing wise
wishy washy withdrawn witty worldly worried worthless wretched
xenophobic youthful zany zealous}], };

# one way to test this on the command-line:
# perl halberdsnhelmets.pl get --redirect /characters | w3m -T text/html

sub traits {
  my $lang = shift;
  my $name = name();
  my $gender = $names{$name};
  my $description = "$name, ";
  my $d;
  if ($gender eq "F") {
    $d = d3();
  } elsif ($gender eq "M") {
    $d = 3 + d3();
  } else {
    $d = d6();
  }
  if ($d == 1) {
    $description .= T('young woman');
  } elsif ($d == 2) {
    $description .= T('woman');
  } elsif ($d == 3) {
    $description .= T('elderly woman');
  } elsif ($d == 4) {
    $description .= T('young man');
  } elsif ($d == 5) {
    $description .= T('man');
  } elsif ($d == 6) {
    $description .= T('elderly man');
  };
  $description .= ", ";
  my $trait = one(@{$traits->{$lang}});
  $description .= $trait;
  my $other = one(@{$traits->{$lang}});
  if ($other ne $trait) {
    $description .= " " . T('and') . " " . $other;
  }
  return $description;
}

sub random_moldvay {
  my $char = shift;
  # keys that can be provided: name, class, charsheet
  
  provide($char, "name", name()) unless $char->{name};

  my ($str, $dex, $con, $int, $wis, $cha) =
    (roll_3d6(), roll_3d6(), roll_3d6(),
     roll_3d6(), roll_3d6(), roll_3d6());

  # if a class is provided, make sure minimum requirements are met
  my $class = $char->{class};
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

  provide($char, "str", $str);
  provide($char, "dex", $dex);
  provide($char, "con", $con);
  provide($char, "int", $int);
  provide($char, "wis", $wis);
  provide($char, "cha", $cha);

  provide($char, "level",  "1");
  provide($char, "xp",  "0");
  provide($char, "thac0",  19);

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
	     and d6() > 2
	     and $char->{rules} ne "halberds-n-helmets") {
      $class = T('cleric');
    } elsif ($best eq "dex") {
      $class = T('thief');
    } else {
      my @candidates = (T('thief'), T('magic-user'), T('fighter'));
      push(@candidates, T('cleric')) if $char->{rules} ne "halberds-n-helmets";
      $class = one(@candidates);
    }
  }

  provide($char, "class",  $class);

  my $hp = $char->{hp};
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

  provide($char, "hp",  $hp);

  equipment($char);

  my $abilities = abilities($class);
  # spellbook
  if ($class eq T('magic-user') or $class eq T('elf')) {
    $abilities .= "\\\\" . spellbook();
  }
  # code
  $abilities .= "\\\\" . encode_char($char);
  provide($char, "abilities", $abilities);
  provide($char, "charsheet", T('Charactersheet.svg')) unless $char->{charsheet};
}

sub abilities {
  my $class = shift;
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
  }
  return $abilities;
}

sub encode_char {
  my $char = shift;
  return join("", "Code: ",
	      map {
		if (member($_, "name", "xp", "level", "thac0")) {
		  "";
		} elsif ($char->{$_} =~ /^\d+$/) {
		  if ($char->{$_} >= 10) {
		    chr($char->{$_} + 55); # 65 is A
		  } else {
		    $char->{$_};
		  };
		} elsif ($_ eq "class") {
		  classes()->{$char->{$_}} or "?";
		} elsif ($_ eq "property") {
		  my $h = unique(sort keys %price_cache);
		  my %h = map { $h->{$_} => $_ } keys %$h;
		  join ("", "-",
			map {
			  my $item = $_;
			  my $n = 1;
			  $item = $1, $n = $2 if $item =~ /(.*) \((\d+)\)$/;
			  warn "$item, $n, $h{$item}" if $n > 1;
			  $h{$item} x $n;
			} split(/\\\\/, $char->{$_}));
		} else {
		  "?";
		};
	      } @{$char->{provided}});
}

sub classes {
  return {
    T('dwarf') => "D",
	T('elf') => "E",
	T('halfling') => "H",
	T('fighter') => "F",
	T('magic-user') => "M",
	T('cleric') => "C",
	T('thief') => "T",
  };
}

sub decode_char {
  my ($code, $language) = @_;
  local $lang = $language; # make sure T works as intended
  my $char = {};
  provide($char, 'name', '?');
  my @abilities = map { number($_) } split(//, substr($code, 0, 6));
  for my $ability (qw(str dex con int wis cha)) {
    provide($char, $ability, shift(@abilities));
  }
  provide($char, 'level', 1);
  provide($char, 'xp', 0);
  provide($char, 'thac0', 19);
  # provide($char, 'code', $code);
  my $h = classes();
  my %h = map { $h->{$_} => $_ } keys %$h;
  my $class = $h{substr($code, 6, 1)};
  provide($char, "class", $class);
  provide($char, "hp", number(substr($code, 7, 1)));
  my ($ac) = (substr($code, 8) =~ /^(-?[0-9A-Z]+)/);
  provide($char, "ac", number($ac));
  get_price_cache(); # sets global %price_cache
  $h = unique(sort keys %price_cache);
  my $i = index($code, "-", 9);
  my @property;
  while ($i++ < length($code) - 1) {
    my $item = $h->{substr($code, $i, 1)};
    add($item, \@property);
  }
  provide($char, "property", join("\\\\", @property));
  provide($char, "abilities", abilities($class));
  moldvay_saves($char);
  return $char;
}

# convert a single digit to a number as if it were base 36, eg. A is 10
sub number {
  my $s = shift;
  return $s =~ /[A-Z]/ ? ord($s) - 55 : $s;
}

# Given a list of items, return a list mapping them to very short but unique
# strings to encode them. Actually, we'll just assume up to 36 items in the list
# and return the index. The order is important!
sub unique {
  my @source = @_;
  my %h;
  my $i = 1;
  while (@source) {
    $h{$i++} = shift(@source);
    if ($i eq '10') {
      $i = "A"; # $i++ will still work
    } elsif ($i eq 'AA') {
      $i = "a";
    }
  }
  # warn join(', ', map { "$_: $h{$_}" } keys %h);
  return \%h;
}

sub random_acks {
  my $char = shift;
  provide($char, "name", name()) unless $char->{name};

  my ($str, $dex, $con, $int, $wis, $cha) =
    (roll_3d6(), roll_3d6(), roll_3d6(),
     roll_3d6(), roll_3d6(), roll_3d6());

  # if a class is provided, make sure minimum requirements are met
  my $class = $char->{class};
  while (member($class, T('dwarven vaultguard'), T('dwarven craftpriest'))
	 and not average($con)) {
    $con = roll_3d6();
  }

  my $title = $char->{title};

  provide($char, "str", $str);
  provide($char, "dex", $dex);
  provide($char, "con", $con);
  provide($char, "int", $int);
  provide($char, "wis", $wis);
  provide($char, "cha", $cha);

  provide($char, "level",  "1");
  provide($char, "xp",  "0");
  provide($char, "attack",  10);

  my $best = best($str, $dex, $con, $int, $wis, $cha);

  if (not $class) {
    if (average($con) and $best eq "str" and d6() > 2) {
      $class = T('dwarven vaultguard');
      $title = T('Sentry');
    } elsif (average($con) and $best eq "wis" and d6() > 2) {
      $class = T('dwarven craftpriest');
      $title = T('Dwarven Craft-Catechist');
    } elsif (average($int) and $best eq "str") {
      $class = T('elven spellsword');
      $title = T('Arcanist-Guardian');
    } elsif (average($int) and $best eq "dex" and d6() > 3) {
      $class = T('elven nightblade');
      $title = T('Arcanist-Avenger');
    } elsif (average($wis) and $best eq "dex") {
      $class = T('bladedancer');
      $title = T('Blade-Initiate');
    } elsif (average($str, $dex) >= 2 and good($str, $dex) >= 1) {
      if (d6() > 3) {
	$class = T('explorer');
	$title = T('Scout');
      } else {
	$class = T('assassin');
	$title = T('Thug');
      }
    } elsif (good($str, $con) >= 2) {
      $class = T('fighter');
      $title = T('Man-at-Arms');
    } elsif (good($wis)) {
      $class = T('cleric');
      $title = T('Catechist');
    } elsif ($best eq "int" and d6() > 2) {
      $class = T('mage');
      $title = T('Arcanist');
    } elsif (average($dex) and good($cha)) {
	$class = T('bard');
	$title = T('Reciter');
    } elsif (average($dex)) {
      $class = T('thief');
      $title = T('Footpad');
    } else {
      $class = T('fighter');
      $title = T('Man-at-Arms');
    }
  }

  provide($char, "class",  $class);
  provide($char, "title",  $title);

  my $hp = $char->{hp};
  if (not $hp) {
    if ($class eq T('fighter') or $class eq T('dwarven vaultguard')) {
      $hp = d8();
    } elsif ($class eq T('mage') or $class eq T('thief')) {
      $hp = d4();
    } else {
      $hp = d6();
    }

    $hp += bonus($con);
    $hp = 1 if $hp < 1;
  }

  provide($char, "hp",  $hp);
  
  equipment($char);

  provide($char, "abilities", proficiencies());
  # abilities
  # spells
}

sub random_freebooters {
  my $char = shift;
  # keys that can be provided: gender, name, class, charsheet
  provide($char, "class", freebooters_playbook($char)) unless $char->{class};
  freebooters_abilities($char);
  freebooters_heritage($char);
  freebooters_name($char) unless $char->{name};
  provide($char, "appearance", freebooters_appearance($char)) if $char->{portrait} eq "no";
  provide($char, "hp", freebooters_hit_points($char));
  provide($char, "alignment", freebooters_alignment($char));
  provide($char, "traits", freebooters_traits($char));
  freebooters_gear($char);
  provide($char, "level",  "1");
  provide($char, "xp",  "0");
}

sub freebooters_playbook {
  my $char = shift;
  if (not $char->{class}) {
    my $roll = d12();
    if ($roll <=  6)    { return T('fighter')    }
    elsif ($roll <=  9) { return T('thief')      }
    elsif ($roll <= 11) { return T('cleric')     }
    else                { return T('magic-user') }
  }
}

sub freebooters_abilities {
  my $char = shift;
  provide($char, "str", roll_3d6());
  provide($char, "dex", roll_3d6());
  provide($char, "con", roll_3d6());
  provide($char, "int", roll_3d6());
  provide($char, "wis", roll_3d6());
  provide($char, "cha", roll_3d6());
  provide($char, "luc", roll_3d6());
  
  # and swap one
  if ($char->{class} eq T('fighter')) {
    swap_for_highest($char, ["str", "con", "dex"], ["wis", "int", "cha"]);
  } elsif ($char->{class} eq T('thief')) {
    swap_for_highest($char, ["dex"], ["wis", "str", "int"]);
  } elsif ($char->{class} eq T('cleric')) {
    swap_for_highest($char, ["cha"], ["wis", "int", "dex"]);
  } elsif ($char->{class} eq T('magic-user')) {
    swap_for_highest($char, ["int"], ["str", "wis", "con", "dex", "cha"]);
  }
}

sub swap_for_highest {
  my ($char, $to, $from) = @_;
  my $highest_from = highest($char, @$from);
  my $lowest_to = lowest($char, @$to);
  if ($char->{$highest_from} > $char->{$lowest_to}) {
    swap($char, $highest_from, $lowest_to);
    $char->{notes} .= "Swapped $highest_from and $lowest_to.\\\\"
  }
}

sub highest {
  my ($char, @abilities) = @_;
  my $best = shift(@abilities);
  for my $ability (@abilities) {
    if ($char->{$ability} > $char->{$best}) {
      $best = $ability;
    }
  }
  return $best;
}

sub lowest {
  my ($char, @abilities) = @_;
  my $worst = shift(@abilities);
  for my $ability (@abilities) {
    if ($char->{$ability} < $char->{$worst}) {
      $worst = $ability;
    }
  }
  return $worst;
}

sub swap {
  my ($char, $a, $b) = @_;
  my $temp = $char->{$a};
  $char->{$a} = $char->{$b};
  $char->{$b} = $temp;
}

sub freebooters_heritage {
  my $char = shift;
  if ($char->{class} eq T('fighter')) {
    roll_heritage($char, 7, 8, 11, 12, qw/str dex con/);
  } elsif ($char->{class} eq T('thief')) {
    roll_heritage($char, 7, 10, 11, 12, qw/dex int cha/);
  } elsif ($char->{class} eq T('cleric')) {
    roll_heritage($char, 7, 8, 11, 12, qw/cha wis con/);
  } elsif ($char->{class} eq T('magic-user')) {
    roll_heritage($char, 8, 9, 10, 12, qw/int dex cha/);
  }
}

sub roll_heritage {
  my ($char, $human, $halfling, $dwarf, $elf, @preferred) = @_;
  my $roll = d12();
  if ($roll <= $human) {
    provide($char, "race", T('human'));
    human_bonus($char, @preferred);
  } elsif ($roll <= $halfling) {
    provide($char, "race", T('halfling'));
    $char->{luc} += 2;
  } elsif ($roll <= $dwarf) {
    provide($char, "race", T('dwarf'));
    # increase the better value
    $char->{best($char->{str}, 0, $char->{con}, 0, 0, 0)} += 2;
  } elsif ($roll <= $elf) {
    provide($char, "race", T('elf'));
    # increase the better value
    $char->{best(0, $char->{dex}, 0, 0, $char->{wis}, $char->{cha})} += 2;
  }
}

sub human_bonus {
  my ($char, @preferred) = @_;
  my $picked;
  for (1..2) {
    my $choice;
    for my $attr (@preferred) {
      if ($char->{$attr} =~ /^(3|5|8|12|15|17)$/ and $attr ne $picked) {
	$char->{$attr}++;
	$picked = $choice = $attr;
	last;
      }
      if (not $choice) {
	my $attr = one(grep { $_ ne $picked } @preferred);
	$char->{$attr}++;
	$picked = $choice = $attr;
      }
    }
  }
}

sub freebooters_name {
  my $char = shift;
  my $gender = one("M", "F");
  my $name;
  # The names in this list are from the book Freebooters on the Frontier by
  # Jason Lutes. The text of that book is released under a Creative Commons
  # Attribution-ShareAlike 3.0 Unported license.
  # https://creativecommons.org/licenses/by-sa/3.0/
  if ($char->{race} eq T('human') and $gender eq "M") {
    $name = one(qw(Athelan Aldred Alger Archard Astyrian Bowden Brogan Caden
    Cerdic Devan Druce Dugal Edlyn Ebis Esward Firman Framar Fugol Garret Gidwin
    Gord Govannon Greme Grindan Halwen Holt Iden Irbend Kendrik Leor Lufian Nyle
    Odel Ord Orleg Radan Reged Rowe Scrydan Seaver Shepard Snell Stedman Swift
    Teon Tobrec Tredan Ware Warian Wulf));
  } elsif ($char->{race} eq T('human') and $gender eq "F") {
    $name = one(qw(Acca Alodia Andessa Anlis Ara Ardith Berroc Bernia Bodica
    Brigantia Brimlid Caro Cwen Darel Dawn Diera Dotor Eda Elene Elga Elswyth
    Elva Elvina Erlina Esma Faradan Freya Garmang Gloris Harmilla Hunnar Juliana
    Kandara Laralan Lorn Maida Megdas Mercia Mora Ogethas Ossia Pallas Rathet
    Sibley Sunnivar Tate Udela Viradeca Wilona Zora));
  } elsif ($char->{race} eq T('halfling') and $gender eq "M") {
    $name = one(qw(Adaman Adelard Adred Agilward Arnest Balbas Barton Bell Banco
    Bowman Cal Emmet Erling Fastman Foda Freebern Frid Gerd Hadred Hagar Halbert
    Hamfast Hildred Huge Isen Jaco Jungo Helm Konner Lambert Leon Linus Marko
    Matti Mekel Melchior Lesser Nenko Nob Olo Ortwin Otto Paladin Pasco Quintus
    Sifro Ted Tolman Wilber Wiseman));
  } elsif ($char->{race} eq T('halfling') and $gender eq "F") {
    $name = one(qw(Adelle Agilward Alfreda Amalinde Balba Bella Beryl Bess
    Camelia Cordelia Daisy Demona Drogga Elanor Ella Elsbeth Elsina Emerly Foda
    Gilda Gilly Hanna Hilda Hildred Janna Jilly Kat Klare Lily Lobelia Lorna
    Lucie Magda Marga Mari Marigold Marka Marlyn Mina Noba Olga Ottillia Pansy
    Pervinca Poppy Rose Rowan Salina Tella Ulrica));
  } elsif ($char->{race} eq T('dwarf') and $gender eq "M") {
    $name = one(qw(Bagan Banar Belir Besil Boran Darin Dirin Doibur Doigan Fagan
    Fignus Firin Gesil Glagan Glasil Glenus Goirin Gosil Hanar Heran Hoibur
    Hoili Hoinar Holir Homli Kimli Koisin Lasin Legan Loilir Mirin Moli Nasil
    Nefur Neli Nignar Noifur Ramli Regnar Safur Sali Saran Segnar Serin Simli
    Tasil Teli Tisin Toilin Toinus));
  } elsif ($char->{race} eq T('dwarf') and $gender eq "F") {
    $name = one(qw(Berin Bibura Bisil Dagna Delinia Deris Dira Disia Dorinda
    Faran Fasina Fignis Foifur Foimli Gerda Gestis Ginus Glegna Glelia Glelis
    Glemlia Gloigas Gloigna Glonara Hegna Hignara Hoimlis Kana Kemlir Keri Keris
    Kilina Kolina Korana Lifur Loilis Loilina Mamli Milina Moibur Moli Noris
    Nosi Rana Ribura Sasilia Soirina Soran Toigna Tomlis));
  } elsif ($char->{race} eq T('elf') and $gender eq "M") {
    $name = one(qw(Amánd Amioril Analad Anin Anumir Calithrambor Calóng Calór
    Cebrin Cóldor Corfindil Delithran Elithranduil Elverion Eowóril Galithrar
    Gelith Gladriendil Glamir Glarang Glil-Gang Glundil Gorfilas Góriand Hal
    Harang Isil-Galith Isilith Isónd Isorfildur Legaraldur Lómebrildur Mil-Gan
    Náldur Nelith Niol Porfindel Ráldur Silmandil Tand Taralad Tararion Tendil
    Téril Tildur Tiniomir Unálad Unebrin Unéndil Uriong));
  } elsif ($char->{race} eq T('elf') and $gender eq "F") {
    $name = one(qw(Amidë Anadriedia Anarania Anebriwien Anilmadith Beliniel
    Calararith Cebridith Celénia Celil-Gathiel Cidien Eäróndra Eärorfindra
    Eláthien Eláviel Eleniel Elorfindra Elváwien Eoweclya Eowodia Fórith
    Gilmadith Gladrieclya Glélindë Gorfinia Hadrieviel Haniel Hebriclya
    Legithralia Lómithrania Meclya Mélith Módien Paclya Paradien Pedith
    Pil-Gandra Pirith Porficlya Sithralindë Thrédith Thrilmadith Thrithien
    Throrfindra Tilmaclya Tilmawen Tinilmania Uradriethiel Urithrarith
    Urorfiviel));
  } else {
    # If race or gender is not supported, pick a random name and assign gender
    # accordingly. This should not happen unless you introduce a new race
    # without providing names for its members.
    $name = name();
    $gender = $names{$name} if member($names{$name}, "M", "F");
  }
  provide($char, "name", $name);
  provide($char, "gender", $gender);
}

sub freebooters_appearance {
  my $char = shift;
  my @phrases;
  # The descriptions in this list are from the book Freebooters on the Frontier
  # by Jason Lutes. The text of that book is released under a Creative Commons
  # Attribution-ShareAlike 3.0 Unported license.
  # https://creativecommons.org/licenses/by-sa/3.0/
  for (1..3) {
    my $phrase;
    # Note that some of the results make you roll on the next table, so don't
    # use elsif!
    if ($char->{class} eq T('cleric')) {
      $phrase = one("big feet", "blazing eyes", "bushy eyebrows", "circlet",
      "clean-shaven", "clear-eyed", "cleft chin", "crooked teeth", "curly hair",
      "dandruff", "dark skin", "dirty", "earrings", "gaunt", "goatee",
      "gray hair", "headband", "heavyset", "high forehead", "hirsute", "hooded",
      "large hands", "long beard", "missing teeth", "miter", "notable helmet",
      "notable nose", "notable garb", "pale skin", "perfect posture",
      "perfumed", "piercing gaze", "pockmarked", "rosy cheeks", "scarred",
      "shaved head", "shining eyes", "smelly", "smiling", "square chin",
      "square-shouldered", "strange marks", "stubble", "tattoos",
      "thundering voice", "tonsure", "unwashed", "warty", "well-scrubbed",
      "roll on Fighter");
      # $char->{notes} .= "appearance, rolled on the $char->{class} table: $phrase\\\\";
    }
    if ($char->{class} eq T('fighter')
	or $phrase eq "roll on Fighter") {
      $phrase = one("big feet", "big mouth", "big mustache", "notable nose",
      "braided hair", "broken nose", "chiseled", "clear-eyed", "cleft chin",
      "crooked teeth", "curly hair", "dark skin", "deep voice", "dirty",
      "earrings", "gap-toothed", "goatee", "headband", "high cheekbones",
      "hirsute", "lantern jaw", "large ears", "large hands", "large head",
      "long-legged", "matted hair", "missing ear", "missing eye",
      "missing finger", "missing teeth", "notable boots", "notable helmet",
      "perfect posture", "pockmarked", "raspy voice", "rosy cheeks", "sandals",
      "scarred", "tattoos", "shaved head", "smelly", "smiling", "squint",
      "steely gaze", "stubble", "tattoos", "unsmiling", "well-scrubbed",
      "youthful", "roll on Thief");
      # $char->{notes} .= "appearance, rolled on the $char->{class} table: $phrase\\\\";
    }
    if ($char->{class} eq T('thief')
	or $phrase eq "roll on Thief") {
      $phrase = one("broken nose", "chin whiskers", "clean-shaven",
      "clear-eyed", "crooked teeth", "curly hair", "dark skin", "deep voice",
      "disfigured", "disheveled", "gap-toothed", "gaunt", "goatee", "hirsute",
      "hooded", "limp", "little mouth", "long fingers", "matted hair",
      "missing eye", "missing finger", "missing teeth", "narrowed eyes",
      "notable footwear", "notable gloves", "notable cap/hat", "notable nose",
      "overbite", "pale skin", "pencil", "mustache", "perfect posture",
      "pockmarked", "pointy chin", "poor posture", "raspy voice", "ratty
      clothes", "red-rimmed eyes", "scarred", "shifty eyes", "small hands",
      "smelly", "squint", "stubble", "tattoos", "unsmiling", "unwashed",
      "well-groomed", "whispery voice", "widow’s peak", "roll on Magic-User");
      # $char->{notes} .= "appearance, rolled on the $char->{class} table: $phrase\\\\";
    }
    if ($char->{class} eq T('magic-user')
	or $phrase eq "roll on Magic-User") {
      $phrase = one("acid scars", "aged", "bald", "black teeth",
      "booming voice", "burn scars", "bushy eyebrows", "chin whiskers",
      "crooked teeth", "curly hair", "dark skin", "disfigured", "forked tongue",
      "gaunt", "glowing eyes", "gnarled hands", "goatee", "gray hair", "haggard",
      "hairless", "headband", "high cheekbones", "high forehead", "hooded",
      "limp", "long beard", "long fingernails", "long hair", "mismatched eyes",
      "missing teeth", "no eyebrows", "notable nose", "notable robes",
      "oily skin", "pale skin", "pockmarked", "pointy hat", "poor posture",
      "raspy voice", "scarred", "skeletal hands", "skullcap", "smelly",
      "strange marks", "sunken eyes", "tattoos", "unwashed", "warty",
      "white hair", "widow’s peak");
      # $char->{notes} .= "appearance, rolled on the $char->{class} table: $phrase\\\\";
    }
    push(@phrases, $phrase) if $phrase and not member($phrase, @phrases);
  }
  return wrap(join(", ", @phrases), 27);
}

sub freebooters_hit_points {
  my $char = shift;
  if    ($char->{class} eq T('fighter'))    { return d10() }
  elsif ($char->{class} eq T('thief'))      { return  d6() }
  elsif ($char->{class} eq T('cleric'))     { return  d8() }
  elsif ($char->{class} eq T('magic-user')) { return  d4() };
}

sub freebooters_alignment {
  my $char = shift;
  my ($evil, $chaotic, $neutral, $lawful, $good);
  
  if ($char->{class} eq T('fighter')) {
    ($evil, $chaotic, $neutral, $lawful, $good) = (2, 4, 8, 10, 12);
  } elsif ($char->{class} eq T('thief')) {
    ($evil, $chaotic, $neutral, $lawful, $good) = (2, 6, 10, 10, 12);
  } elsif ($char->{class} eq T('cleric')) {
    ($evil, $chaotic, $neutral, $lawful, $good) = (3, 5, 7, 9, 12);
  } elsif ($char->{class} eq T('magic-user')) {
    ($evil, $chaotic, $neutral, $lawful, $good) = (3, 8, 8, 8, 12);
  }
  
  my $roll = d12();
  if    ($roll <= $evil)    { return T('evil') }
  elsif ($roll <= $chaotic) { return T('chaotic') }
  elsif ($roll <= $neutral) { return T('neutral') }
  elsif ($roll <= $lawful)  { return T('lawful') }
  elsif ($roll <= $good)    { return T('good') }
}

sub freebooters_traits {
  my $char = shift;
  my $alignment = $char->{alignment};
  my ($virtues, $vices);
  if ($alignment eq T('evil')) {
    ($virtues, $vices) = (0, 3);
  } elsif ($alignment eq T('chaotic')) {
    ($virtues, $vices) = (1, 2);
  } elsif ($alignment eq T('neutral')) {
    ($virtues, $vices) = (1, 1);
  } elsif ($alignment eq T('lawful')) {
    ($virtues, $vices) = (2, 1);
  } elsif ($alignment eq T('good')) {
    ($virtues, $vices) = (3, 0);
  }
  my (@traits, $trait);
  for (1..$virtues) {
    do {
      $trait = one(qw(ambitious benevolent bold brave charitable chaste cautious
      compassionate confident considerate cooperative courteous creative curious
      daring defiant dependable determined disciplined enthusiastic fair focused
      forgiving friendly frugal funny generous gregarious helpful honest
      honorable hopeful humble idealistic just kind loving loyal merciful
      orderly patient persistent pious resourceful respectful responsible
      selfless steadfast tactful tolerant));
    } until not member($trait, @traits);
    push(@traits, $trait);
  }
  for (1..$vices) {
    do {
      $trait = one(qw(addict aggressive alcoholic antagonistic arrogant boastful
      cheater covetous cowardly cruel decadent deceitful disloyal doubtful
      egotistical envious gluttonous greedy hasty hedonist impatient inflexible
      irritable lazy lewd liar lustful mad malicious manipulative merciless
      moody murderous obsessive petulant prejudiced reckless resentful rude
      ruthless self-pitying selfish snobbish stingy stubborn vain vengeful
      wasteful wrathful zealous));
    } until not member($trait, @traits);
    push(@traits, $trait);
  }
  return wrap(join(", ", @traits), 25);
}

sub freebooters_gear {
  my $char = shift;
  my $wt = 0;

  my %weapons = 
      (T('axe') => [T('1d8'), 'close', 2],
       T('club') => [T('1d6'), 'close', 1],
       T('dagger') => [T('1d4'), "1 " . join(", ", T('pierce'), T('precise'), T('hand')), 0],
       T('flail') => [T('1d8'), join(", ", T('close'), T('forceful')), 2],
       T('great axe') => [T('1d10'), join(", ", T('close'), T('rare'), T('2-handed')), 3],
       T('great hammer') => [T('1d10'), join(", ", T('close'), T('forceful'), T('rare'), T('2-handed')), 4],
       T('great sword') => [T('1d10'), join(", ", T('close'), T('reach'), T('rare'), T('2-handed')), 3],
       T('hatchet') => [T('1d4'), join(", ", T('throw'), T('close'), T('near')), 1],
       T('knife') => [T('1d4'), join(", ", T('precise'), T('hand')), 0],
       T('mace') => [T('1d6'), join(", ", T('close'), T('forceful')), 1],
       T('pick') => [T('1d6'), "2 " . join(", ", T('pierce'), T('close'), T('awkward')), 1],
       T('polearm') => [T('1d10'), join(", ", T('reach'), T('2-handed')), 3],
       T('shortsword') => [T('1d6'), "!" . T('close'), 1],
       T('spear') => [T('1d8'), join(", ", T('throw'), T('reach'), T('near')), 2],
       T('staff') => [T('1d4'), join(", ", T('close'), T('2-handed')), 1],
       T('sword') => [T('1d8'), "!" . T('close'), 2],
       T('war hammer') => [T('1d6'), "1 " . join(", ", T('pierce'), T('close')), 1],
       T('throwing knife') => [T('1d4'), join(", ", T('hand'), T('close'), T('reach'), T('near'), T('precise')), 0],
       T('sling') => [T('1d4'), join(", ", T('near'), T('far'), T('reload')), 0],
       T('shortbow') => [T('1d6'), join(", ", T('near'), T('far'), T('2-handed')), 1],
       T('long bow') => [T('1d8'), join(", ", T('near'), T('far'), T('2-handed')), 1],
       T('light crossbow') => [T('1d6'), "1 " . join(", ", T('pierce'), T('near'), T('far'), T('2-handed'), T('reload')), 1],
       T('heavy crossbow') => [T('1d6'), "2 " . join(", ", T('pierce'), T('near'), T('far'), T('2-handed'), T('reload')), 2]);
  
  if ($char->{class} eq T('fighter')) {

    $wt += freebooter_weapon($char, \%weapons, freebooters_fighter_weapon($char, \%weapons));

    my $roll = d6();
    if ($roll == 1) {
      provide($char, "item1", T('leather armor'));
      provide($char, "armor", 1);
      provide($char, "item1-wt", 1);
      $wt += 1;
    } elsif ($roll <= 5) {
      provide($char, "item1", T('chain mail'));
      provide($char, "armor", 2);
      provide($char, "item1-wt", 3);
      $wt += 3;
    } else {
      provide($char, "item1", T('scale armor') . " (" . T('awkward') . ")");
      provide($char, "armor", 3);
      provide($char, "item1-wt", 4);
      $wt += 4;
    }
  } elsif ($char->{class} eq T('thief')) {

    $wt += freebooter_weapon($char, \%weapons, one(T('knife'), T('dagger'), T('shortsword')));
    
    provide($char, "item1", T('leather armor'));
    provide($char, "armor", 1);
    provide($char, "item1-wt", 1);
    $wt += 1;
    
  } elsif ($char->{class} eq T('cleric')) {

    $wt += freebooter_weapon($char, \%weapons, one(T('staff'), T('mace'), T('war hammer')));
    
    my $roll = d6();
    if ($roll <= 2) {
      provide($char, "item1", T('shield'));
      provide($char, "armor", 1);
      provide($char, "item1-wt", 2);
      $wt += 3;
    } elsif ($roll <= 4) {
      provide($char, "item1", T('leather armor'));
      provide($char, "armor", 1);
      provide($char, "item1-wt", 1);
      $wt += 1;
    } else {
      provide($char, "item1", T('chain mail'));
      provide($char, "armor", 2);
      provide($char, "item1-wt", 3);
      $wt += 3;
    }
  } elsif ($char->{class} eq T('magic-user')) {

    provide($char, "item1", T('spell book'));
    provide($char, "item1-wt", 1);
    provide($char, "item2", T('rations'));
    provide($char, "item2-wt", 1);
    provide($char, "silver", d6() + d6() + $char->{luc});
    $wt += 2;

    my $wp = 1; # next weapon
    my $it = 3; # next item

    my $roll = d6();
    # $char->{notes} .= "magic-user focus: $roll\\\\";
    if ($roll <= 2) {
      provide($char, "item$it", T('magic wand') . " (+1 " . T('power') . ")");
      provide($char, "item$it-wt", 0);
      $it++;
    } elsif ($roll <= 5) {
      provide($char, "weapon$wp", T('magic staff') . "\\\\(" . join(", ", T('close'), T('2-handed'), "+1 " . T('power')) . ")");
      provide($char, "dmg$wp", T('1d4'));
      provide($char, "weapon$wp-wt", 1);
      $wt += 1;
      $wp += 2;
    } else {
      provide($char, "item$it", T('magic orb') . " (+2 " . T('power') . ")");
      provide($char, "item$it-wt", 1);
      $wt += 1;
      $it++;
    }

    for (1 .. 2) {

      $roll = d6();
      # $char->{notes} .= "magic-user equipment: $roll\\\\";
      if ($roll <= 2) {
	provide($char, "item$it", T('bag of books (○○○○○)'));
	provide($char, "item$it-wt", 2);
	$wt += 2;
	$it++;
      } elsif ($roll <= 3) {
	# check if we already rolled a dagger
	if ($char->{"weapon$wp"} =~ /^dagger/) {
	  $char->{"weapon$wp"} =~ s/^dagger/dagger (2)/;
	} else {
	  $wt += freebooter_weapon($char, \%weapons, T('dagger'), $wp);
	  # don't increase $wp because a second dagger will just add (2)
	}
      } elsif ($roll <= 4) {
	provide($char, "item$it", T('healing potion (heal 1d8 HP)'));
	provide($char, "item$it-wt", 0);
	$it++;
      } else {
	provide($char, "item$it", T('spell components (+1 power, ○○○)'));
	provide($char, "item$it-wt", 0);
	$it++;
      }
    }
  }
  provide($char, "total-wt", $wt);
}

sub freebooter_weapon {
  my ($char, $weapons, $weapon, $wp) = @_;
  $wp ||= 1; # position
  if (length($weapon . " (" . $weapons->{$weapon}->[1] . ")") > 23) {
    provide($char, "weapon$wp", $weapon . "\\\\"
	    # the second line has more space
	    . wrap("(" . $weapons->{$weapon}->[1] . ")", 33));
  } else {
    provide($char, "weapon$wp", $weapon . " (" . $weapons->{$weapon}->[1] . ")");
  }
  provide($char, "dmg$wp", $weapons->{$weapon}->[0]);
  provide($char, "weapon$wp-wt", $weapons->{$weapon}->[2]);
  return $weapons->{$weapon}->[2];
}

sub freebooters_fighter_weapon {
  my ($char, $weapons) = @_;
  if ($char->{dex} >= 13 and $char->{str} >= 13
      and $char->{race} ne T('halfling')) {
    # no great weapons for halflings
    $char->{notes} .= "High dex and str and not a halfling: pick a 2-handed weapon doing 1d10.\\\\";
    return one(freeboters_filter_weapons($weapons, ["1d10", "2-handed"]));
  } elsif ($char->{dex} >= 13
	   and $char->{race} ne T('halfling')) {
    # no longbows for halflings
    $char->{notes} .= "High dex and not a halfling: pick a ranged weapon doing 1d8.\\\\";
    return one(freeboters_filter_weapons($weapons, ["1d8", "far"]));
  } elsif ($char->{str} <= 8) {
    $char->{notes} .= "Low str: pick a light (wt 1) melee weapon doing 1d6.\\\\";
    return one(freeboters_filter_weapons($weapons, ["1d6", "close", "1"]));
  } elsif ($char->{str} >= 13) {
    $char->{notes} .= "High str: pick a melee weapon doing 1d8.\\\\";
    return one(freeboters_filter_weapons($weapons, ["1d8", "close"]));
  } else {
    $char->{notes} .= "Picked a random weapon.\\\\";
    one(keys %$weapons);
  }
}

sub freeboters_filter_weapons {
  my ($weapons, $with, $without) = @_;
  return grep {
    my @tags = ($weapons->{$_}->[0],
		split(/, /, $weapons->{$_}->[1]),
		$weapons->{$_}->[2]);
    my $ok = 1;
    foreach (@$with) {
      $ok = 0 unless member($_, @tags);
    }
    foreach (@$without) {
      $ok = 0 if member($_, @tags);
    }
    $ok;
  } keys %$weapons;
}

sub random_parameters {
  my ($char, $language) = @_;
  local $lang = $language; # make sure T works as intended
  if (not exists $char->{rules}
      or not defined $char->{rules}
      or $char->{rules} eq "moldvay"
      or $char->{rules} eq "halberds-n-helmets"
      or $char->{rules} eq "labyrinth lord") {
    random_moldvay($char);
  } elsif ($char->{rules} eq "pendragon") {
    random_pendragon($char);
  } elsif ($char->{rules} eq "crypts-n-things") {
    random_crypts_n_things($char);
  } elsif ($char->{rules} eq "acks") {
    random_acks($char);
  } elsif ($char->{rules} eq "freebooters") {
    random_freebooters($char);
  } else {
    error(T('Unknown Rules'), T('%0 is unknown.', $char->{rules}));
  }

  # choose a random portrait based on the character name or class
  if (member("portrait", @_)) {
    provide($char, "portrait", portrait($char)) unless $char->{portrait};
  }
}

sub proficiencies {
  my $char = shift;
  my %proficiencies = ();

  # start with class based preferences for proficiencies
  if ($char->{class} eq T('assassin')) {

    %proficiencies = (
      'Acrobatics' => +2,
      'Alchemy' => -1,
      'Alertness' => 0,
      'Arcane Dabbling' => 0,
      'Blind Fighting' => 0,
      'Bribery' => +1,
      'Cat Burglary' => +1,
      'Climbing' => +1,
      'Combat Refexes' => 0,
      'Combat Trickery (Disarm)' => 0,
      'Combat Trickery (Incapacitate)' => 0,
      'Contortionism' => 0,
      'Disguise' => +1,
      'Eavesdropping' => 0,
      'Fighting Style' => +1,
      'Gambling' => -2,
      'Intimidation' => 0,
      'Mimicry' => -1,
      'Precise Shooting' => +1,
      'Running' => 0,
      'Seduction' => 0,
      'Skirmishing' => 0,
      'Skulking' => +1,
      'Sniping' => +1,
      'Swashbuckling' => 0,
      'Trap Finding' => 0,
      'Weapon Finesse' => -2,
      'Weapon Focus' => -1, );
    if ($char->{cha} > 12) {
      $proficiencies{Bribery} = +2;
      $proficiencies{Intimidation} = +1;
      $proficiencies{Seduction} = +1;
    }
    if ($char->{dex} > 12) {
      $proficiencies{Sniping} = +2;
    }
    if ($char->{level} > 5) {
      $proficiencies{'Arcane Dabbling'} = +1;
    }
  }      

  # add general proficiencies
  my %general = ();
  $general{'Alchemy'} = 1 if $char->{class} eq T('mage');
  $general{'Animal Husbandry'} = 0;
  $general{'Animal Husbandry'} = 1 if $char->{class} eq T('explorer');
  $general{'Beast Friendship'} = 2;
  $general{'Animal Training (Dog)'} = 2;
  $general{'Art'} = 0;
  $general{'Bargaining'} = 0;
  $general{'Bargaining'} = 1 if member($char->{class}, T('dwarven vaultguard'), T('dwarven craftpriest'), T('explorer'));
  $general{'Caving'} = 0;
  $general{'Collegiate Wizardry'} = 0;
  $general{'Collegiate Wizardry'} = 1 if member($char->{class}, T('mage'), T('elven spellsword'), T('elven nightblade'));
  $general{'Craft'} = 0;
  $general{'Diplomacy'} = 0;
  $general{'Diplomacy'} = 1 if $char->{cha} > 12;
  $general{'Disguise'} = 0;
  $general{'Disguise'} = 1 if $char->{class} eq T('thief');
  $general{'Disguise'} = 2 if $char->{class} eq T('assassin');
  $general{'Endurance'} = 0;
  $general{'Endurance'} = 1 if $char->{ac} < 3;
  $general{'Engineering'} = 0;
  $general{'Gambling'} = 0;
  $general{'Healing'} = 0;
  # add more Healing if we already have healing?
  $general{'Healing'} = 3 if $char->{int} >= 18;
  $general{'Gambling'} = 0;
  $general{'Intimidation'} = 0;
  $general{'Intimidation'} = 1 if $char->{cha} > 13;
  $general{'Knowledge'} = 0;
  $general{'Knowledge'} = 3 if $char->{int} >= 18;
  $general{'Labor'} = 0;
  $general{'Language'} = 0;
  $general{'Leadership'} = 0;
  $general{'Lip Reading'} = 0;
  $general{'Lip Reading'} = 3 if $char->{class} eq T('thief');
  $general{'Manual of Arms'} = 0;
  $general{'Mapping'} = 0;
  $general{'Military Strategy'} = 0;
  $general{'Military Strategy'} = 1 if $char->{int} > 13 and $char->{wis} > 13;
  $general{'Mimicry'} = 0;
  $general{'Naturalism'} = 0;
  $general{'Navigation'} = 0;
  $general{'Performance'} = 0;
  $general{'Performance'} = 3 if $char->{class} eq T('bard');
  $general{'Profession'} = 0;
  $general{'Profession'} = 1 if $char->{str} < 9 or $char->{dex} < 9;
  $general{'Riding'} = 0;
  $general{'Riding'} = 1 if member($char->{class}, T('fighter'), T('explorer'), T('barbarian'));
  $general{'Seafaring'} = 0;
  $general{'Seduction'} = 0;
  $general{'Seduction'} = 1 if $char->{cha} > 13;
  $general{'Siege Engineering'} = 0;
  $general{'Seduction'} = 0;
  $general{'Signaling'} = 0 if $char->{ac} < 3;
  $general{'Survival'} = 0;
  $general{'Survival'} = 2 if member($char->{class}, T('explorer'), T('barbarian'));
  $general{'Theology'} = 0;
  $general{'Theology'} = 2 if member($char->{class}, T('cleric'), T('elven bladedancer'));
  $general{'Tracking'} = 0;
  $general{'Tracking'} = 1 if member($char->{class}, T('assassin'), T('explorer'));
  $general{'Trapping'} = 0;
  $general{'Trapping'} = 1 if $char->{class} eq T('explorer');

  # set up lists
  my @proficiencies = distribution(\%proficiencies);
  my @general = distribution(\%general);
  my @result = ('Adventuring');
  push(@result, one(@proficiencies));
  my $proficiency;
  my $m = 1;
  $m += $char->{"int-bonus"} if $char->{"int-bonus"} > 0;
  for (my $i = 0; $i < $m; $i++) {
    $proficiency = one(@general);
    # do { $proficiency = one(@general) } until not member($proficiency, @result);
    push(@result, $proficiency);
  }
  return join(", ", map { T($_) } @result);
}

sub distribution {
  # Given a hash ref mapping keys to numbers between -2 and +2, returns an array with an appropriate number of keys. Use
  # one() to pick a random key.
  my $hashref = shift;
  my @result = ();
  my $default = 3; # this means the ratio between -- and ++ is 1:5
  for my $key (keys %$hashref) {
    my $n = $default + $hashref->{$key};
    for (my $i = 0; $i < $n; $i++) {
      push(@result, $key);
    }
  }
  return @result;
}

sub portrait {
  my $char = shift;
  my $gender = $char->{gender} || $names{$char->{name}};
  if ($char->{class} eq T('elf')
      or $char->{race} eq T('elf')) {
    $gender = "elf";
  } elsif ($char->{class} eq T('dwarf')
      or $char->{race} eq T('dwarf')) {
    $gender = "dwarf";
  } elsif ($gender eq "F") {
    $gender = "woman";
  } elsif ($gender eq "M") {
    $gender = "man";
  } else {
    $gender = one("woman", "man");
  }
  my $url = Mojo::URL->new("https://campaignwiki.org/face/redirect/alex/$gender");
  my $ua = Mojo::UserAgent->new;
  my $tx = $ua->get($url);
  $url->path($tx->res->headers->location);
  return $url;
}

sub characters {
  my ($char, $lang) = @_;
  my @characters;
  for (my $i = 0; $i < 50; $i++) {
    my %one = %$char; # defaults
    random_parameters(\%one, $lang);
    $one{traits} = traits($lang);
    push(@characters, \%one);
  }
  return \@characters;
}

sub stats {
  my ($char, $language, $n) = @_;
  local $lang = $language; # make sure T works as intended
  my (%class, %property);
  for (my $i = 0; $i < $n; $i++) {
    my %one = %$char; # defaults
    random_parameters(\%one, $lang);
    $class{$one{class}}++;
    foreach (split(/\\\\/, $one{property})) {
      $property{$_}++;
    }
  }

  $n = 0;
  my $txt = T('Classes') . "\n";
  foreach (sort { $class{$b} <=> $class{$a} } keys %class) {
    $txt .= sprintf "%25s %4d\n", $_, $class{$_};
    $n += $class{$_};
  }
  $txt .= sprintf "%25s %4d\n", "total", $n;

  $txt .= T('Property') . "\n";
  foreach (sort { $property{$b} <=> $property{$a} }
	   keys %property) {
    next if /starting gold:/ or /gold$/;
    next if /Startgold:/ or /Gold$/;
    $txt .= sprintf "%25s %4d\n", $_, $property{$_};
  }
  return $txt;
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

sub init {
  my $self = shift;
  my %char = %{$self->req->params->to_hash};
  my @provided; # We want to remember the order!
  my @pairs = @{$self->req->params->pairs};
  while (@pairs) {
    my $key = shift @pairs;
    my $value = shift @pairs;
    push(@provided, $key);
  }
  $char{provided} = \@provided;
  return \%char;
}

sub lang {
  my $self = shift;
  my $p = HTTP::AcceptLanguage->new($self->req->headers->accept_language);
  return $p->match(qw(en de));
}

plugin "Config" => {default => {}};

get "/" => sub {
  my $self = shift;
  $self->redirect_to($self->url_with("main" => {lang => lang($self)}));
};

get "/:lang" => [lang => qr/(?:en|de)/] => sub {
  my $self = shift;
  my $lang = $self->param("lang");
  my $query = $self->req->query_params->to_string;
  if ($query) {
    # deprecated
    $query =~ tr/;/&/;
    my $params = Mojo::Parameters->new($query);
    return $self->redirect_to($self->url_for("char" => {lang => $lang})->query(@$params));
  }
  $self->render(template => "index.$lang");
} => "main";

get "/help" => "help";

get "/hilfe" => "hilfe";

get "/random" => sub {
  my $self = shift;
  $self->redirect_to($self->url_with("random" => {lang => lang($self)}));
};

get "/random/text/:lang" => sub {
  my $self = shift;
  my $char = init($self);
  my $lang = $self->param("lang");
  random_parameters($char, $lang, "portrait");
  compute_data($char, $lang);
  $self->render(template => "text.$lang",
		format => "txt",
		char => $char);
} => "text";

get "/random/:lang" => [lang => qr/(?:en|de)/] => sub {
  my $self = shift;
  my $char = init($self);
  my $lang = $self->param("lang");
  random_parameters($char, $lang, "portrait");
  compute_data($char, $lang);
  my $svg = svg_transform($self, svg_read($char));
  $self->render(format => "svg",
		data => $svg->toString());
} => "random";

get "/char" => sub {
  my $self = shift;
  $self->redirect_to($self->url_with("char" => {lang => lang($self)}));
};

get "/char/:lang" => [lang => qr/(?:en|de)/] => sub {
  my $self = shift;
  my $char = init($self);
  my $lang = $self->param("lang");
  # no random parameters
  compute_data($char, $lang);
  my $svg = svg_transform($self, svg_read($char));
  $self->render(format => "svg",
		data => $svg->toString());
} => "char";

# deprecated
get "/link/:lang" => [lang => qr/(?:en|de)/] => sub {
  my $self = shift;
  my $lang = $self->param("lang");
  my $query = $self->req->query_params;
  $query =~ tr/;/&/;
  my $params = Mojo::Parameters->new($query);
  $self->redirect_to($self->url_for("edit" => {lang => lang($self)})->query(@$params));
};

get "/edit" => sub {
  my $self = shift;
  $self->redirect_to(edit => {lang => lang($self)});
};

get "/edit/:lang" => [lang => qr/(?:en|de)/] => sub {
  my $self = shift;
  my $char = init($self);
  my $lang = $self->param("lang");
  $self->render(template => "edit.$lang",
		char => $char);
} => "edit";

get "/decode/:lang" => [lang => qr/(?:en|de)/] => sub {
  my $self = shift;
  my $lang = $self->param("lang");
  my $code = $self->param("code");
  my $char = decode_char($code, $lang);
  $self->render(template => "edit.$lang",
		char => $char);
} => "decode";

get "/redirect" => sub {
  my $self = shift;
  $self->redirect_to($self->url_with("redirect" => {lang => lang($self)}));
};

get "/redirect/:lang" => [lang => qr/(?:en|de)/] => sub {
  my $self = shift;
  my $lang = $self->param("lang");
  my $input = $self->param("input");
  my $params = Mojo::Parameters->new;
  my $last;
  while ($input =~ /^([-a-z0-9]*): *(.*?)\r?$/gm) {
    if ($1 eq $last or $1 eq "") {
      $params->param($1 => $params->param($1) . "\\\\$2");
    } else {
      $params->append($1 => $2);
      $last = $1;
    }
  }
  $self->redirect_to($self->url_for("char" => {lang => $lang})->query($params));
} => "redirect";


get "/show" => sub {
  my $self = shift;
  my $char = init($self);
  my $svg = svg_show_id(svg_read($char));
  $self->render(format => "svg",
		data => $svg->toString());
} => "show";

get "/characters" => sub {
  my $self = shift;
  $self->redirect_to($self->url_with("characters" => {lang => lang($self)}));
};

get "/characters/:lang" => [lang => qr/(?:en|de)/] => sub {
  my $self = shift;
  my $lang = $self->param("lang");
  my $char = init($self);
  $self->render(template => "characters.$lang",
		width => "100%",
		characters => characters($char, $lang));
} => "characters";

get "/stats" => sub {
  my $self = shift;
  $self->redirect_to($self->url_with("stats" => {lang => lang($self),
						 n => 1000}));
};

get "/stats/:n" => [n => qr/\d+/] => sub {
  my $self = shift;
  my $n = $self->param("n");
  $self->redirect_to($self->url_with("stats" => {lang => lang($self),
						 n => $n}));
};

get "/stats/:lang" => [lang => qr/(?:en|de)/] => sub {
  my $self = shift;
  my $lang = $self->param("lang");
  $self->redirect_to($self->url_with("stats" => {lang => $lang,
						 n => 1000}));
};

get "/stats/:lang/:n" => [lang => qr/(?:en|de)/, n => qr/\d+/] => sub {
  my $self = shift;
  my $lang = $self->param("lang");
  my $n = $self->param("n");
  my $char = init($self);
  $self->render(format => "txt",
		text => stats($char, $lang, $n));
} => "stats";

app->secrets([app->config("secret")]) if app->config("secret");

app->start;

__DATA__

@@ index.en.html.ep
% layout "default.en";
% title "Character Sheet Generator";
<h1>Character Sheet Generator</h1>

<p>

This is the <i>Halberds and Helmets</i> Character Sheet Generator. By default it
will generate a <%= link_to "random character" => "random" %> for <i>Basic
D&D</i> by Moldvay (the “B” in <a href="https://en.wikipedia.org/wiki/Dungeons_%26_Dragons_Basic_Set#1981_revision">B/X D&D</a>).
Reload the character sheet to generate more 1<sup>st</sup> level characters.

Feel free to provide a name for your random character!

%= form_for random => begin
%= label_for name => "Name:"
%= text_field "name"
%= submit_button
% end

<p>
The character sheet contains a link in the bottom right corner which allows you
to bookmark and edit your character. <%= link_to "Learn more…" => "help" %>

<p>
If you got a printed character sheet and want to recreate it, you can type in
the code right here:

%= form_for decode => begin
%= label_for name => "Code:"
%= text_field "code"
%= submit_button
% end

<p>
If you're looking for an alternative, check out the
<a href="http://character.totalpartykill.ca/">Random D&D Character Generator</a>
by <a href="http://save.vs.totalpartykill.ca/">Ramanan Sivaranjan</a>.

@@ index.de.html.ep
% layout "default.de";
% title "Character Sheet Generator";
<h1>Charakterblatt Generator</h1>

<p>

Dies ist der <i>Hellebarden und Helme</i> Charaktergenerator. Per Default generieren wir einen
<%= link_to url_for("random", {lang => "de"}) => begin %>zufälligen Charakter<% end %>
für die <i>Basic D&D</i> Regeln von Moldvay (das “B” in
<a href="https://en.wikipedia.org/wiki/Dungeons_%26_Dragons_Basic_Set#1981_revision">B/X D&D</a>).
Um weitere Charaktere der 1. Stufe zu generieren, kann man die Seite einfach neu laden.

Wer will, kann dem generierten Charakter hier auch einen Namen geben:

%= form_for random => begin
%= label_for name => "Name:"
%= text_field "name"
%= submit_button
% end

<p>
Auf dem generierten Charakterblatt hat es unten rechts einen Link mit dem man
sich ein Lesezeichen erstellen kann und wo der Charakter bearbeitet werden kann.
<%= link_to "Weiterlesen…" => "hilfe" %>

<p>

Hat man ein Charakterblatt ausgedruckt und vergessen, sich den Link zu speicher,
kann man dies mit dem Code zum Teil wieder herstellen.

%= form_for decode => begin
%= label_for name => "Code:"
%= text_field "code"
%= submit_button
% end

<p>
Eine englische Alternative wäre der
<a href="http://character.totalpartykill.ca/">Random D&D Character Generator</a>
von <a href="http://save.vs.totalpartykill.ca/">Ramanan Sivaranjan</a>.


@@ edit.en.html.ep
% layout "default.en";
% title "Link (Character Sheet Generator)";
<h1>A Link For Your Character</h1>

<h2>Bookmark</h2>

<p>
Bookmark the following link to your
<%= link_to url_for("char")->query($self->req->params) => begin %>Character Sheet<% end %>.

<h2>Edit</h2>

<p>
Use the following form to make changes to your character sheet. You can also
copy and paste it on to a <a href="https://campaignwiki.org/">Campaign Wiki</a>
page to generate an inline character sheet.

%= form_for url_for("redirect" => {lang => $lang}) => begin
%= hidden_field lang => $lang
%= text_area "input" => (cols => 72, rows => 20) => begin
<% for my $key (@{$char->{provided}}) { %>\
<%   for my $value (split(/\\\\/, $char->{$key})) { =%>\
<%= $key =%>: <%= $value %>
<%   } %>\
<% } %>\
% end
<p>
%= submit_button
% end


@@ edit.de.html.ep
% layout "default.de";
% title "Edit Your Character";
<h1>Ein Link für deinen Charakter</h1>

<h2>Lesezeichen</h2>

<p>
Den Charakter kann man einfach aufbewahren, in dem man sich den Link auf
<%= link_to url_for("char")->query($self->req->params) => begin %>Charakterblatt<% end %>
als Lesezeichen speichert.

<h2>Bearbeiten</h2>

<p>
Mit dem folgenden Formular lassen sich leicht Änderungen am Charakter machen.
Man kann diesen Text auch auf einer <a href="https://campaignwiki.org/">Campaign
Wiki</a> Seite verwenden, um das Charakterblatt einzufügen.

%= form_for url_for("redirect" => {lang => $lang}) => begin
%= text_area "input" => (cols => 72, rows => 20) => begin
<% for my $key (@{$char->{provided}}) { %>\
<%   for my $value (split(/\\\\/, $char->{$key})) { =%>\
<%= $key =%>: <%= $value %>
<%   } %>\
<% } %>\
% end
<p>
%= submit_button
% end

@@ text.en.txt.ep
Character:
<% for my $key (@{$char->{provided}}) { %>\
<%   for my $value (split(/\\\\/, $char->{$key})) { =%>\
<%= $key =%>: <%= $value %>
<%   } %>\
<% } %>\

@@ text.de.txt.ep
Charakter:
<% for my $key (@{$char->{provided}}) { %>\
<%   for my $value (split(/\\\\/, $char->{$key})) { =%>\
<%= $key =%>: <%= $value %>
<%   } %>\
<% } %>\

@@ characters.html.ep
<% for my $char (@{$characters}) { %>
<pre style="display: block; float: left; height: 25em; width: 30em; font-size: 6pt">
<%= $char->{traits} %>
Str Dex Con Int Wis Cha HP AC Class
<%= sprintf "%3d", $char->{str} %> \
<%= sprintf "%3d", $char->{dex} %> \
<%= sprintf "%3d", $char->{con} %> \
<%= sprintf "%3d", $char->{int} %> \
<%= sprintf "%3d", $char->{wis} %> \
<%= sprintf "%3d", $char->{cha} %> \
<%= sprintf "%2d", $char->{hp} %> \
<%= sprintf "%2d", $char->{ac} %> \
<%= $char->{class} %>
<% for my $property (split(/\\\\/, $char->{property})) { =%>
<%= $property %>
<% } %>\
</pre>
<% } %>
<div style="clear: both"></div>

@@ characters.en.html.ep
% layout "default.en";
% title "Characters";
<h1>A Bunch of Characters</h1>
%= include "characters"

@@ characters.de.html.ep
% layout "default.de";
% title "Charaktere";
<h1>Einige Charaktere</h1>
%= include "characters"


@@ hilfe.html.ep
% layout "default.de";
% title "Hilfe (Charakterblatt Generator)";
<h1>Charakterblatt Generator</h1>

<p>Das funktioniert über eine Vorlage und dem Ersetzen von Platzhaltern.

<ul>
<li><a href="#moldvay">Basic D&D</a>
<li><a href="#labyrinth_lord">Labyrinth Lord</a>
<li><a href="#hellebarden">Hellebarden und Helme</a>
<li><a href="#pendragon">Pendragon</a>
<li><a href="#crypts_n_things">Crypts & Things</a>
<li><a href="#freebooters">Freebooters on the Frontier</a>
<li><a href="#ACKS">Adventure Conqueror King System</a>
</ul>

<h2 id="moldvay">Basic D&amp;D</h2>

<p>Die
<%= link_to url_for("char" => {lang => "de"})->query(charsheet => "Charakterblatt.svg") => begin %>Defaultvorlage<% end %>
(<%= link_to url_for("char" => {lang => "de"})->query(charsheet => "Charakterblatt-quer.svg") => begin %>Alternative<% end %>)
verwendet die <a href="/Purisa.ttf">Purisa</a> Schrift. Den Platzhaltern werden
über URL Parameter Werte zugewiesen
(<%= link_to url_for("char" => {lang => "de"})->query(name => "Tehah", class => "Elf", level => "1", xp => "100", ac => "9", hp => "5", str => "15", dex => "9", con => "15", int => "10", wis => "9", cha => "7", breath => "15", poison => "12", petrify => "13", wands => "13", spells => "15", property => "Zauberbuch (Gerdana)\\\\* Einschläferndes Rauschen", abilities => "Ghinorisch\\\\Elfisch", thac0 => "19", charsheet => "Charakterblatt.svg") => begin %>Beispiel<% end %>,
<%= link_to url_for("char" => {lang => "de"})->query(name => "Tehah", class => "Elf", level => "1", xp => "100", ac => "9", hp => "5", str => "15", dex => "9", con => "15", int => "10", wis => "9", cha => "7", breath => "15", poison => "12", petrify => "13", wands => "13", spells => "15", property => "Zauberbuch (Gerdana)\\\\* Einschläferndes Rauschen", abilities => "Ghinorisch\\\\Elfisch", thac0 => "19", charsheet=>"Charakterblatt-quer.svg") => begin %>Alternative<% end %>).
Das Skript kann auch zeigen
<%= link_to url_for("show")->query(charsheet=>"Charakterblatt.svg") => begin %>welche Parameter wo erscheinen<% end %>
(<%= link_to url_for("show")->query(charsheet=>"Charakterblatt-quer.svg") => begin %>Alternative<% end %>).
Die Parameter müssen UTF-8 codiert sein. Die Vorlage kann auch mehrzeilige
Platzhalter enthalten. Der entsprechende Parameter muss die Zeilen dann durch
doppelte Backslashes trennen.

<p>
Zudem werden einige Parameter berechnet, sofern sie nicht
angegeben wurden:

<ul>
<li>str → str-bonus
<li>dex → dex-bonus
<li>con → con-bonus
<li>int → int-bonus
<li>wis → wis-bonus
<li>cha → cha-bonus
<li>cha-bonus → loyalty
<li>str-bonus → damage
<li>thac0 → melee-thac0
<li>melee-thac0 → melee0-9
<li>damage → melee-damage
<li>thac0 → range-thac0
<li>range-thac0 → range0-9
<li>damage → range-damage
</ul>

<p>
Das Skript kann auch
<%= link_to url_for("random" => {lang => "de"}) => begin %>einen zufälligen Charakter<% end %>,
<%= link_to url_for("characters" => {lang => "de"}) => begin %>einige Charaktere<% end %>,
oder <%= link_to url_for("stats" => {lang => "de"}) => begin %>Statistiken<% end %>
generieren.

<p id="labyrinth_lord">
Da die Preisliste für Labyrinth Lord sich von der Moldvay Liste
etwas unterscheidet, kann man auch
<%= link_to url_for("random" => {lang => "de"})->query(rules => "labyrinth lord") => begin %>einen zufälligen Charakter<% end %>,
<%= link_to url_for("characters" => {lang => "de"})->query(rules => "labyrinth lord") => begin %>einige Charaktere<% end %>
oder <%= link_to  url_for("stats" => {lang => "de"})->query(rules => "labyrinth lord") => begin %>Statistiken<% end %>
mit <a href="http://www.goblinoidgames.com/labyrinthlord.html">Labyrinth Lord</a>
Regeln generieren.

<p id="hellebarden">
Da es für Hellebarden und Helme keine Kleriker gibt, kann man auch
<%= link_to url_for("random" => {lang => "de"})->query(rules => "halberds-n-helmets") => begin %>einen zufälligen Charakter<% end %>,
<%= link_to url_for("characters" => {lang => "de"})->query(rules => "halberds-n-helmets") => begin %>einige Charaktere<% end %>
oder <%= link_to  url_for("stats" => {lang => "de"})->query(rules => "halberds-n-helmets") => begin %>Statistiken<% end %>
mit <a href="https://github.com/kensanata/halberdsnhelmets/tree/master/Hellebarden%20und%20Helme">Hellebarden und Helme</a>
Regeln generieren.

<h2 id="pendragon">Pendragon</h2>

<p>
Das Skript kann auch
<a href="http://www.nocturnal-media.com/games/pendragon">Pendragon</a>
Charaktere anzeigen (aber nicht zufällig erstellen):
<%= link_to url_for("edit" => {lang => "de"})->query(rules => "pendragon", charsheet => "Pendragon.svg") => begin %>Pendragon Charakter<% end %> bearbeiten.
Das Skript kann auch zeigen
<%= link_to url_for("show")->query(charsheet => "Pendragon.svg") => begin %>welche Parameter wo erscheinen<% end %>.

<p>
Zudem werden einige Parameter berechnet, sofern sie nicht
angegeben wurden:

<ul>
<li>str+siz → damage
<li>str+con → heal
<li>str+dex → move
<li>siz+con → hp
<li>hp → unconscious
<li>chaste ↔ lustful
<li>energetic ↔ lazy
<li>forgiving ↔ vengeful
<li>generous ↔ selfish
<li>honest ↔ deceitful
<li>just ↔ arbitrary
<li>merciful ↔ cruel
<li>modest ↔ proud
<li>pious ↔ worldly
<li>prudent ↔ reckless
<li>temperate ↔ indulgent
<li>trusting ↔ suspicious
<li>valorous ↔ cowardly
</ul>

<h2 id="crypts_n_things">Crypts & Things</h2>

<p>
Das Skript kann auch Charaktere für
<a href="http://d101games.com/books/crypts-and-things/">Crypts & Things</a>
anzeigen (aber nicht zufällig erstellen):
<%= link_to url_for("edit" => {lang => "de"})->query(rules => "crypts-n-things", charsheet => "Crypts-n-Things.svg") => begin %>Crypts & Things Charakter<% end %>
bearbeiten. Das Skript kann auch zeigen
<%= link_to url_for("show")->query(charsheet => "Crypts-n-Things.svg") => begin %>welche Parameter wo erscheinen<% end %>.

<p>
Zudem werden einige Parameter berechnet, sofern sie nicht
angegeben wurden:

<ul>
<li>str → to-hit
<li>str → damage-bonus
<li>dex → missile-bonus
<li>dex → ac-bonus
<li>con → con-bonus
<li>int → understand
<li>cha → charm
<li>cha → hirelings
<li>wis → sanity
</ul>

<h2 id="freebooters">Freebooters on the Frontier</h2>

<p>
Das Skript kann für
<a href="http://www.drivethrurpg.com/product/157011/Freebooters-on-the-Frontier">Freebooters on the Frontier</a>
verwendet werden. Leider wurde hierfür noch keine Übersetzungsarbeit geleistet. Wenn man weiss,
<%= link_to url_for("show")->query(charsheet => "Maezar-Freebooters.svg") => begin %>welche Parameter wo erscheinen<% end %>,
ist es einfach, einen Charakter zu <%= link_to url_for("edit" => {lang => "en"})->query(rules => "freebooters") => begin %>erstellen<% end %>.

<p>
Das Skript kann
<%= link_to url_for("random" => {lang => "en"})->query(rules => "freebooters", portrait => "no") => begin %>zufällige Charaktere erstellen<% end %>.
Statt einer Beschreibung kann das Charakterblatt auch <%= link_to url_for("random" => {lang => "en"})->query(rules => "freebooters") => begin %>mit einem Bild<% end %> erstellt werden.

<h2 id="ACKS">Adventure Conqueror King System</h2>

<p>
Das Skript kann auch Charaktere für
<a href="http://www.autarch.co/">Adventure Conqueror King System</a>
anzeigen:
<%= link_to url_for("edit" => {lang => "de"})->query(rules => "acks", charsheet => "ACKS.svg") => begin %>ACKS Charakter<% end %>
bearbeiten. Das Skript kann auch zeigen
<%= link_to url_for("show")->query(charsheet => "ACKS.svg") => begin %>welche Parameter wo erscheinen<% end %>.

<p>
Zudem werden einige Parameter berechnet, sofern sie nicht
angegeben wurden:

<ul>
<li>str → str-bonus
<li>int → int-bonus
<li>wis → wis-bonus
<li>dex → dex-bonus
<li>con → con-bonus
<li>cha → cha-bonus
<li>attack+str → melee
<li>attack+dex → missile
</ul>

<p>
<b>Im Aufbau</b>: Das Skript kann auch
<%= link_to url_for("random" => {lang => "de"})->query(rules => "acks", charsheet => "ACKS.svg") => begin %>einen zufälligen Charakter<% end %>,
<%= link_to url_for("characters" => {lang => "de"})->query(rules => "acks") => begin %>einige Charaktere<% end %>
oder <%= link_to  url_for("stats" => {lang => "de"})->query(rules => "acks") => begin %>Statistiken<% end %>
generieren.


@@ help.html.ep
% layout "default.en";
% title "Help (Character Sheet Generator)";
<h1>Character Sheet Generator</h1>

<p>The generator works by using a template and replacing some placeholders.

<ul>
<li><a href="#moldvay">Basic D&D</a>
<li><a href="#labyrinth_lord">Labyrinth Lord</a>
<li><a href="#halberds">Halberds and Helmets</a>
<li><a href="#pendragon">Pendragon</a>
<li><a href="#crypts_n_things">Crypts & Things</a>
<li><a href="#freebooters">Freebooters on the Frontier</a>
<li><a href="#ACKS">Adventure Conqueror King System</a>
</ul>

<h2 id="moldvay">Basic D&D</h2>

<p>The
<%= link_to url_for("char" => {lang => "en"}) => begin %>default template<% end %>
(<%= link_to url_for("char" => {lang => "en"})->query(charsheet=>"Charactersheet-landscape.svg") => begin %>alternative<% end %>)
uses the <a href="/Purisa.ttf">Purisa</a> font. You provide values for the
placeholders by providing URL parameters
(<%= link_to url_for("char" => {lang => "en"})->query(name => "Tehah", class => "Elf", level => "1", xp => "100", ac => "9", hp => "5", str => "15", dex => "9", con => "15", int => "10", wis => "9", cha => "7", breath => "15", poison => "12", petrify => "13", wands => "13", spells => "15", property => "Spell Book (Gerdana)\\\\* sleepy swoosh", abilities => "Ghinorian\\\\Elven", thac0 => "19", charsheet => "Charactersheet.svg") => begin %>example<% end %>,
<%= link_to url_for("char" => {lang => "en"})->query(name => "Tehah", class => "Elf", level => "1", xp => "100", ac => "9", hp => "5", str => "15", dex => "9", con => "15", int => "10", wis => "9", cha => "7", breath => "15", poison => "12", petrify => "13", wands => "13", spells => "15", property => "Spell Book (Gerdana)\\\\* sleepy swoosh", abilities => "Ghinorian\\\\Elven", thac0 => "19", charsheet=>"Charactersheet-landscape.svg") => begin %>alternative<% end %>).
The script can also show
<%= link_to url_for("show")->query(charsheet=>"Charactersheet.svg") => begin %>which parameters go where<% end %>
(<%= link_to url_for("show")->query(charsheet=>"Charactersheet-landscape.svg") => begin %>alternative<% end %>).
Also note that the parameters need to be UTF-8 encoded. If the
template contains a multiline placeholder, the parameter may also provide
multiple lines separated by two backslashes.

<p>
In addition to that, some parameters are computed unless provided:

<ul>
<li>str → str-bonus
<li>dex → dex-bonus
<li>con → con-bonus
<li>int → int-bonus
<li>wis → wis-bonus
<li>cha → cha-bonus
<li>cha-bonus → loyalty
<li>str-bonus → damage
<li>thac0 → melee-thac0
<li>melee-thac0 → melee0-9
<li>damage → melee-damage
<li>thac0 → range-thac0
<li>range-thac0 → range0-9
<li>damage → range-damage
</ul>

<p>
The script can also generate a
<%= link_to url_for("random" => {lang => "en"}) => begin %>random character<% end %>,
<%= link_to url_for("characters" => {lang => "en"}) => begin %>bunch of characters<% end %>
or <%= link_to url_for("stats" => {lang => "en"}) => begin %>some statistics<% end =%>.

<p id="labyrinth_lord">
As the price list for Labyrinth Lord differs from the Moldvay
price list, you can also generate a
<%= link_to url_for("random" => {lang => "en"})->query(rules => "labyrinth lord") => begin %>random character<% end %>,
<%= link_to url_for("characters" => {lang => "en"})->query(rules => "labyrinth lord") => begin %>bunch of characters<% end %>
or <%= link_to  url_for("stats" => {lang => "en"})->query(rules => "labyrinth lord") => begin %>some statistics<% end %>
using <a href="http://www.goblinoidgames.com/labyrinthlord.html">Labyrinth Lord</a>
rules.

<p id="labyrinth_lord">
As Halberds and Helmets has no clerics, you can also generate a
<%= link_to url_for("random" => {lang => "en"})->query(rules => "halberds-n-helmets") => begin %>random character<% end %>,
<%= link_to url_for("characters" => {lang => "en"})->query(rules => "halberds-n-helmets") => begin %>bunch of characters<% end %>
or <%= link_to  url_for("stats" => {lang => "en"})->query(rules => "halberds-n-helmets") => begin %>some statistics<% end %>
using <a href="https://github.com/kensanata/halberdsnhelmets/tree/master/Halberds%20and%20Helmets">Halberds and Helmets</a>
rules.

<h2 id="pendragon">Pendragon</h2>

<p>
The script also supports
<a href="http://www.nocturnal-media.com/games/pendragon">Pendragon</a>
characters (but cannot generate them randomly): Get started with a
<%= link_to url_for("edit" => {lang => "en"})->query(rules => "pendragon", charsheet => "Pendragon.svg") => begin %>Pendragon character<% end %>.
The script can also show
<%= link_to url_for("show")->query(charsheet => "Pendragon.svg") => begin %>which parameters go where<% end %>.

<p>
In addition to that, some parameters are computed unless provided:

<ul>
<li>str+siz → damage
<li>str+con → heal
<li>str+dex → move
<li>siz+con → hp
<li>hp → unconscious
<li>chaste ↔ lustful
<li>energetic ↔ lazy
<li>forgiving ↔ vengeful
<li>generous ↔ selfish
<li>honest ↔ deceitful
<li>just ↔ arbitrary
<li>merciful ↔ cruel
<li>modest ↔ proud
<li>pious ↔ worldly
<li>prudent ↔ reckless
<li>temperate ↔ indulgent
<li>trusting ↔ suspicious
<li>valorous ↔ cowardly
</ul>

<h2 id="crypts_n_things">Crypts &amp; Things</h2>

<p>
The script also supports
<a href="http://d101games.com/books/crypts-and-things/">Crypts & Things</a>
characters (but cannot generate them randomly):
Get started with a
<%= link_to url_for("edit" => {lang => "en"})->query(rules => "crypts-n-things", charsheet => "Crypts-n-Things.svg") => begin %>Crypts & Things character<% end %>.
The script can also show
<%= link_to url_for("show")->query(charsheet => "Crypts-n-Things.svg") => begin %>which parameters go where<% end %>.

<p>
In addition to that, some parameters are computed unless provided:

<ul>
<li>str → to-hit
<li>str → damage-bonus
<li>dex → missile-bonus
<li>dex → ac-bonus
<li>con → con-bonus
<li>int → understand
<li>cha → charm
<li>cha → hirelings
<li>wis → sanity
</ul>

<h2 id="freebooters">Freebooters on the Frontier</h2>

<p>
The script also supports
<a href="http://www.drivethrurpg.com/product/157011/Freebooters-on-the-Frontier">Freebooters on the Frontier</a>
characters: Get started with an
<%= link_to url_for("edit" => {lang => "en"})->query(rules => "freebooters") => begin %>Freebooter character<% end %>.
The script can also show
<%= link_to url_for("show")->query(charsheet => "Maezar-Freebooters.svg") => begin %>which parameters go where<% end %>.

<p>
The script can also generate a
<%= link_to url_for("random" => {lang => "en"})->query(rules => "freebooters", portrait => "no") => begin %>random character<% end %>.
If you prefer, it can <%= link_to url_for("random" => {lang => "en"})->query(rules => "freebooters") => begin %>use a portrait<% end %> instead of generating an appearance.

<h2 id="ACKS">Adventure Conqueror King System</h2>

<p>
The script also supports
<a href="http://www.autarch.co/">Adventure Conqueror King System</a>
characters: Get started with an
<%= link_to url_for("edit" => {lang => "en"})->query(rules => "acks", charsheet => "ACKS.svg") => begin %>ACKS character<% end %>.
The script can also show
<%= link_to url_for("show")->query(charsheet => "ACKS.svg") => begin %>which parameters go where<% end %>.

<p>
In addition to that, some parameters are computed unless provided:

<ul>
<li>str → str-bonus
<li>int → int-bonus
<li>wis → wis-bonus
<li>dex → dex-bonus
<li>con → con-bonus
<li>cha → cha-bonus
<li>attack+str → melee
<li>attack+dex → missile
</ul>

<p>
<b>Work in Progress</b>: The script can also generate a
<%= link_to url_for("random" => {lang => "en"})->query(rules => "acks", charsheet => "ACKS.svg") => begin %>random character<% end %>,
<%= link_to url_for("characters" => {lang => "en"})->query(rules => "acks") => begin %>bunch of characters<% end %>
or <%= link_to  url_for("stats" => {lang => "en"})->query(rules => "acks") => begin %>some statistics<% end %>.


@@ default.html.ep
<!DOCTYPE html>
<html>
<head>
<title><%= title %></title>
%= stylesheet "/halberdsnhelmets/default.css"
%= stylesheet begin
body { padding: 1em; width: <%= $self->stash("width")||"80ex" %>; font-family: "Palatino Linotype", "Book Antiqua", Palatino, serif }
textarea { width: 100% }
% end
<meta name="viewport" content="width=device-width">
</head>
<body>
<%= content %>
<%= content "footer" %>
</body>
</html>

@@ layouts/default.en.html.ep
% content_for footer => begin
<div class="footer">
<hr>
<p>
<%= link_to url_for("main" => {lang => "de"}) => begin %>Character Generator<% end %> &nbsp;
<%= link_to Help => "help" %> &nbsp;
<a href="https://alexschroeder.ch/wiki/Contact">Alex Schroeder</a> &nbsp;
<a href="https://github.com/kensanata/halberdsnhelmets/tree/master/Characters">GitHub</a> &nbsp;
<%= link_to url_for("main" => {lang => "de"}) => begin %>German<% end %>
</div>
% end
%= include "default"

@@ layouts/default.de.html.ep
% content_for footer => begin
<div class="footer">
<hr>
<p>
<%= link_to url_for("main" => {lang => "de"}) => begin %>Charakterblatt Generator<% end %> &nbsp;
<%= link_to Hilfe => "hilfe" %> &nbsp;
<a href="https://alexschroeder.ch/wiki/Contact">Alex Schroeder</a> &nbsp;
<a href="https://github.com/kensanata/halberdsnhelmets/tree/master/Characters">GitHub</a> &nbsp;
<%= link_to url_for("main" => {lang => "en"}) => begin %>English<% end %>
</div>
% end
%= include "default"
