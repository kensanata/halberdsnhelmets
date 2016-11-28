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

The CGI script depends on [Mojolicious](http://mojolicio.us/) (perhaps
this is too old: `sudo apt-get install libmojolicious-perl` – I used
`cpan Mojolicious` instead). We also require `XML::LibXML` and
`HTTP::AcceptLanguage`.


## Installation

You can simply install it as a CGI script on your web server.

As the [script](halberdsnhelmets.pl) is a
[Mojolicious](http://mojolicio.us/) app, there are many other ways to
deploy it. There is a
[Cookbook](http://mojolicio.us/perldoc/Mojolicious/Guides/Cookbook#DEPLOYMENT)
with a section on deployment. The following is a quick summary.

You probably want to set the `MOJO_HOME` environment variable.
Example:

```
MOJO_HOME=. morbo halberdsnhelmets.pl 
```

### Development

This runs the script as a server on
[localhost:3000](http://localhost:3000/) and reloads it every time you
change it:

```
morbo halberdsnhelmets.pl
```

Morbo will reload the application if you change it.

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

### Simple Daemon

This runs the script as a server on
[localhost:3000](http://localhost:3000/):

```
perl halberdsnhelmets.pl daemon
```

I have never had a use for this. Usually I want to use Hypnotoad or
Toadfarm for production. See above.

## Images

The character portraits are made by the
[face generator](https://campaignwiki.org/face).
