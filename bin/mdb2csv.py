#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Copyright 2015 Hakan Mattsson
#
# See the file "LICENSE" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# A simple script which dumps the contents of a Microsoft Access Database.
# It uses the mdbtools suite:
#   http://sourceforge.net/projects/mdbtools/

import os
import sys, subprocess # the subprocess module is new in python v 2.4

database = 'forsguiden.mdb'

delim = "^" # Column delimeter in the generated csv files

# Get the list of table names with "mdb-tables"
print("Reading " + database + "...")
table_names = subprocess.Popen(["mdb-tables", "-1", database],
                               stdout=subprocess.PIPE).communicate()[0]
tables = table_names.split('\n')

# Dump each table as a CSV file using "mdb-export",
# converting " " in table names to "_" for the CSV filenames.

os.mkdir('csv/')
for table in tables:
    if table != '':
        filename = "csv/" + table.replace(" ","_") + ".csv"
        file = open(filename, 'w')
        print("Saving " + table + ".csv")
        contents = subprocess.Popen(["mdb-export", "-Q", "-R", "§", "-d", delim, database, table],
                                    stdout=subprocess.PIPE).communicate()[0]
        file.write(contents)
        file.close()
