Forsguiden
==========

Vill du bidra till att förbättra forsguiden?

Hör av dig till `forsguiden@kkss.se`!

Bakgrund
--------

Siten forsguiden.com dog 2014. R.I.P.

Den anlitade ISP:n, Surftown (www.surftown.com), hade supit bort hela siten. Databasen, alla bilder, kartor och videos var borta. Allt försvann. Av någon outgrundlig anledning fanns inte ens våra filer i deras backupsystem. Utan att överdriva skulle man kunna kalla Surftown för en oseriös ISP. Sjukt oseriös.

Som tur var hade Erik Wettergren sparat en privat kopia av själva databasen. Även om databasen var från 2012, så fanns där väldigt mycket kvar av de gamla forsbeskrivningarna. Dock utan bilder, videos etc.

Därefter har Håkan Mattsson restaurerat databasen och lagt upp detta repository. Databasen finns incheckad som `forsguiden.mdb` och m h a scriptet `mdb2csv.py` exraherades dess tabeller till motsvarande csv-filer. Från csv-filerna genererades XML-filen `forsguiden.xml` m h a scriptet `csv2xml.escript`. Nu när detta är gjorts behöver det förhoppningsvis inte göras om igen.

Editera forsguiden
------------------

Den som vill förbättra den befintliga texten, lägga till beskrivningar av nya forsar etc. gör det genom att editera filen `forspaddling.xml`.

* forspaddling.xml

När detta är gjort kan man generera en HTML-fil, `forsguiden.html`,  m h a scriptet `xml2html.escript`. Eller så kör man bara `make`.

* forspaddling.html

Installera de program som behövs
--------------------------------

 * mdb2csv.py       - är skrivet i Python och kräver `mdbtools`
 * csv2xml.escript  - är skrivet i Erlang
 * xml2html.escript - är skrivet i Erlang

Installera `mdbtools` på `Mac`:

        sudo port install mdbtools

Installera `Erlang` på `Mac`:

        sudo port install erlang

Installera `Erlang` på `Linux`:

        sudo apt-get install erlang

Installera `Erlang` på `Windows`

        http://www.erlang.org/download.html
