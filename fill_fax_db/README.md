## fill_faxes_db.py

This script is used to fill the faxes db for testing purposes like testing migrates and the task kt_fax_cleanup.

It will generate randomized fields and attach the tiff, pdf files to the document.

To setup python to run it:

pip3 install couchdb

then run with

./fill_faxes_db.py <number of docmuments to create>



