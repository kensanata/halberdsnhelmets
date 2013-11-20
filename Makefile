FILES=.htaccess Charactersheet.svg Charakterblatt.svg Pendragon.svg Crypts-n-Things.svg ACKS.svg Purisa.ttf Purisa.eot halberdsnhelmets.pl

all: ${FILES}

.PHONY: ${FILES}

*: 
	scp "alex@psithyrus.epfarms.org:campaignwiki.org/$@" .
