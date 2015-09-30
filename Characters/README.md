Character Generator
===================

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

You can get the Purisa font for free from the
[linux.thai.net (LTN) Thai Linux FTP archive](ftp://linux.thai.net/pub/thailinux/software/thai-ttf/).

The images are from public domain works:

* [Women of all nations, a record of their characteristics, habits, manners, customs and influence](https://www.flickr.com/photos/internetarchivebookimages/tags/bookidwomenofallnation01joyc)

* [Women of beauty and heroism: from Semiramis to Eugenie, a portrait gallery of female loveliness, achievement and influence](https://archive.org/details/womenofbeautyher00good)

* [Notable St. Louisans in 1900; a portrait gallery of men whose energy and ability have contributed largely towards making St. Louis the commercial and financial metropolis of the West, Southwest and South](https://www.flickr.com/photos/internetarchivebookimages/tags/bookidnotablestlouisan00coxj)

* [The people of India: a series of photographic illustrations, with descriptive letterpress, of the races and tribes of Hindustan](https://www.flickr.com/photos/internetarchivebookimages/tags/bookidpeopleofindiaser05greauoft)

* [India and the Indians](https://www.flickr.com/photos/internetarchivebookimages/tags/bookidindiaindians00elwiiala)

* [Castes and tribes of southern India. Assisted by K. Rangachari](https://www.flickr.com/photos/internetarchivebookimages/tags/bookidcastestribesofso03thuruoft)

* [Mysterious Japan](https://www.flickr.com/photos/internetarchivebookimages/tags/bookidmysteriousjapan00streuoft)

* [George Maxwell Gordon; the pilgrim missionary of the Punjab. A history of his life and work, 1839-1880](https://www.flickr.com/photos/internetarchivebookimages/tags/bookidgeorgemaxwellgor00lewi)

* [From the Congo to the Niger and the Nile : an account of The German Central African expedition of 1910-1911](https://www.flickr.com/photos/internetarchivebookimages/tags/bookidfromcongotoniger02adoluoft)

* [Annual report of the Bureau of American Ethnology to the Secretary of the Smithsonian Institution](https://www.flickr.com/photos/internetarchivebookimages/tags/bookidannualreportofbu117smit)

* [Things seen in Japan](https://www.flickr.com/photos/internetarchivebookimages/tags/bookidthingsseeninjapa00holluoft)

* [Hildreth's "Japan as it was and is": a handbook of old Japan](https://www.flickr.com/photos/internetarchivebookimages/tags/bookidhildrethsjapanas02hild)

* [Fra morgenrodens rige; fortaellinger fra Japan, samlede og indledede ved Johanne Münter](https://www.flickr.com/photos/internetarchivebookimages/tags/bookidframorgenrodensr00mn)

* [Public Domain Images – 1920′s Vintage Mugshots NSWPD Special Photographs](http://publicdomainarchive.com/public-domain-images-1920%E2%80%B2s-vintage-mugshots-nswpd-special-photographs/)

* [Pennsylvanian](https://archive.org/details/pennsylvanian191920penn)

* [Bain Collection](https://www.flickr.com/photos/library_of_congress/sets/72157603624867509/)

Stripping EXIF Data
===================

Example:

    exiftool -all= -overwrite_original *.jpg

Posting 20 Characters to Campaign Wiki
======================================

Example

    for i in $(seq 20); do
        f=$(mktemp /tmp/char.XXXX)
        perl halberdsnhelmets.pl '/random/text?' > $f
        name=$(grep name: $f | cut -c 7-)
        class=$(grep class: $f | cut -c 8-)
        if curl --head --silent "https://campaignwiki.org/wiki/Greyheim/$name" | grep --silent "^HTTP/1.1 404"; then
            echo "|[[$name]] | | 0| 0| $class 1| ?|[[Greyheim]] | – | |"
            curl -F ns=Greyheim -F title=$name -F frodo=1 -F username=Alex -F summary="New character" -F "text=<$f" https://campaignwiki.org/wiki
            sleep 1
        fi
    done