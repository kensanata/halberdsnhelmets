# Character Generator

This is the source repository for a character generator for Basic D&D
and Labyrinth Lord. In addition to that, it can be used to take any
SVG file (such as the character sheets in this directory) and fill in
parameters, essentially making it possible to "bookmark" a character
sheet. If you do this, you'll notice a little Link in the bottom right
of these character sheets. That's where you can edit your character.
Be sure to bookmark the new URL or you'll loose your edits!

The character generator is currently
[installed on Campaign Wiki](http://campaignwiki.org/halberdsnhelmets).
Take a look at the
[Help](http://campaignwiki.org/halberdsnhelmets/help) page. It comes
with links to examples of what this web application can do.


## Purisa

You can get the Purisa font for free from the
[linux.thai.net (LTN) Thai Linux FTP archive](ftp://linux.thai.net/pub/thailinux/software/thai-ttf/).


## Posting 20 Characters to Campaign Wiki

Example (English):

```bash
for i in $(seq 20); do
	f=$(mktemp /tmp/char.XXXX)
	perl halberdsnhelmets.pl '/random/text/en?' > $f
	name=$(grep name: $f | cut -c 7-)
	class=$(grep class: $f | cut -c 8-)
	if curl --head --silent "https://campaignwiki.org/wiki/Greyheim/$name" | grep --silent "^HTTP/1.1 404"; then
		echo "|[[$name]] | | 0| 0| $class 1| ?|[[Greyheim]] | – | |"
		curl -F ns=Greyheim -F title=$name -F frodo=1 -F username=Alex -F summary="New character" -F "text=<$f" https://campaignwiki.org/wiki
		sleep 1
	fi
done
```

Generate up to 20 characters but only post thieves:

```bash
n=1
for i in $(seq 20); do
    f=$(mktemp /tmp/char.XXXX)
    perl halberdsnhelmets.pl get '/random/text/en' > $f
    name=$(grep name: $f | cut -c 7-)
    class=$(grep class: $f | cut -c 8-)
    if [[ $class == 'thief' ]]; then
	if curl --head --silent "https://campaignwiki.org/wiki/Greyheim/$name" \
		| grep --silent "^HTTP/1.1 404"; then
            echo "|[[$name]] | | 0| 0| $class 1| ?|[[Greyheim]] | – | |"
            curl -F ns=Greyheim -F title=$name -F frodo=1 -F username=Alex \
		 -F summary="New character" -F "text=<$f" https://campaignwiki.org/wiki
            sleep 1
	    n=$(( $n + 1 ))
	    if [[ $n > 5 ]]; then
		exit 0;
	    fi
	fi
    else
	echo $name is a $class
    fi
    rm $f
done
```

## Dependencies

The CGI script depends on [Mojolicious](http://mojolicio.us/) and some
other Perl modules. You can install everything via `cpan` or `cpanm`,
or if you're on a Debian system, try the following:

```
sudo apt-get install git libmojolicious-perl \
  libi18n-acceptlanguage-perl libxml-libxml-perl
```

## Installation

### Home Use

If you just want to tinker with it at home, here's what I suggest:

```
mkdir ~/src
cd ~/src
git clone https://github.com/kensanata/halberdsnhelmets.git
cd ~/src/halberdsnhelmets/Characters
MOJO_HOME=. perl halberdsnhelmets.pl daemon
```

If everything went according to plan, you should see output like the
following:

```
[Thu Dec  8 14:38:59 2016] [info] Listening at "http://*:3000"
Server available at http://127.0.0.1:3000
```

Visit the link and you should see the application. If you do not,
contact me and we'll figure it out.

### Tinkering at Home

If you want to tinker with the code, use `morbo` to run the
application because that will restart the application every time you
make a change.

```
MOJO_HOME=. morbo halberdsnhelmets.pl daemon
```

### Production

This runs the script as a server on port 8080, writing a pid file:

```
hypnotoad halberdsnhelmets.pl
```

Whenever you repeat this `hypnotoad` command, the server will be
restarted. There's no need to kill it.

You can configure `hypnotoad` to listen on a different port by adding
an additional item to the config file, `halberdsnhelmets.conf`. Here's
an example:

```
{
  hypnotoad => {listen => ['http://*:8083'],},
}
```

This is great for production. If you have multiple Mojolicious
applications, you can either run them all with their own Hypnotoad, or
you can use [Toadfarm](https://metacpan.org/pod/Toadfarm).

## Images

The character portraits are made by
the [face generator](https://campaignwiki.org/face). If you install
the character generator on a website using HTTPS, then you will have
to install the face generator in the same domain, because browsers
will refuse to download images from a different domain when using
HTTPS.
