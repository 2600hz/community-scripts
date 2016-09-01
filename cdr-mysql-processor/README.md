# README #
### What is it? ###

This script is used to monitor a directory for JSON CDRs from Kazoo.
I have written a Kazoo app, cdrtofile that will automatically save CDRs to files as they come in.
When a new CDR is saved to the filesystem it will be inserted into a MySQL database.

### Requirements ###

* MySQL
* Python2
* The Python libraries in requirements.txt (install with pip install -r requirements.txt)

### Configuration ###

Configuration is set in config.py, with details of what means what.
FIELDS can be modified if a different schema is to be used.

### Running ###

* Ensure all directories mentioned in config.py exist and have the correct permissions set.
* Ensure MySQL is running, and already contains the table in mysql_schema.
* Ensure the MySQL credentials in config.py are correct.

Run the script with `python2 run.py`