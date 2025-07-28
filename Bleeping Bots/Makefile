bleeping-bots.pdf: bleeping-bots.html hex-map.svg dungeon-map.svg
	weasyprint $< $@

bleeping-bots.html: bleeping-bots.md static.html
	oddmu html -template static.html $< > $@

dungeon-map.svg: dungeon-map.txt
	text-mapper render --square < $< > $@

hex-map.svg: hex-map.txt
	text-mapper render < $< | sed 's/viewBox=".*"/viewBox="-20 -20 640 640"/' > $@

bleeping-bots.zip: bleeping-bots.html hex-map.svg dungeon-map.svg Stego.jpg
	zip $@ $^

upload: bleeping-bots.pdf bleeping-bots.zip
	rsync --archive --itemize-changes --exclude='*~' * \
	  sibirocobombus:campaignwiki.org/1pdc/2025/bleeping-bots/
