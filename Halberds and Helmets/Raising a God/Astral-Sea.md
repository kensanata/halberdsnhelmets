# Astral Sea

This is the input to use for [Text Mapper](https://campaignwiki.org/text-mapper) in order to generate `Astral-Sea.svg`. Then use `make Astral-Sea.png` to generate a large enough PNG file.

```
0101 town gate "Asgard"
0102 empty
0103 empty
0104 town "Shark Bay"
0201 empty
0202 town gate "Lagnabadalë"
0203 town gate "Monkeytown"
0204 town "Giant Lotus"
0301 town gate "Hell Gate"
0302 town "Pirate Rock"
0303 empty
0304 town "Iceland"
0401 town gate "Muspelheim"
0402 town "Arden"
0403 town "Anafasi's Tomb"
0404 town gate "Nieflheim"
0501 town gate "Nightrealm"
0502 empty
0503 town "Brood Mother"
0504 empty
0601 empty
0602 town "Hinia Oot"
0603 town gate "Fresh Air"
0604 town "Panopticon"
0701 town gate "Stormworld"
0702 town gate "Redwood"
0703 empty
0704 town "Genova"
default attributes fill="none" stroke="black" stroke-width="3"
town xml <circle r="10"/>
gate xml <text font-size="20pt" dy="-20px" dx="40px" font-family="Optima, Helvetica, sans-serif" text-anchor="middle">☼</text>
green xml <circle r="48" fill="green" opacity="0.3"/>
orange xml <circle r="48" fill="orange" opacity="0.3"/>
red xml <circle r="48" fill="red" opacity="0.3"/>
text font-size="16pt" dy="15px" font-family="Optima, Helvetica, sans-serif" fill-opacity="1"
label font-size="16pt" font-family="Optima, Helvetica, sans-serif"
glow stroke="white" stroke-width="4pt" opacity="0.8"
other <text font-size="14pt" x="100" y="-10" font-family="Optima, Helvetica, sans-serif">Legend: • town – ☼ gate</text>
```
