# Copyright 2015 Hakan Mattsson
#
# See the file "LICENSE" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

all: forsguiden.html

csv: forsguiden.mdb
	./bin/mdb2csv.py

forsguiden.xml: csv
	./bín/csv2xml.escript

forsguiden.html:: forsguiden.xml
	./bin/xml2html.escript
