###PHP-based CouchDB Dump/Restore Utility

This tool was originally authored by [Anton Bondar](https://github.com/zebooka). Additional work to support inline base64 attachements was sponsored by [CloudPBX Inc.](http://cloudpbx.ca) and authored by [Miralem Mehic](https://github.com/mickeyze).

The original dump tool authored by zebooka included supported incremental backups. This made it much more feasible to add support to dump inline base64 attachements. 

Although CouchDB's  `/_all_docs` function is more popular with DB backups that don't include attachements, this function doesn't support attachments. To download attachments, individual documents must be accessed.

Rerence on CouchDB's  `/_all_docs` function [here ](http://docs.couchdb.org/en/latest/api/database/bulk-api.html) 

###Usage for BACKUP with `couchdb-dump.php`

#####Basic Example: 

`couchdb-dump.php -H localhost -p 5984 -d test > dump.json`

#####Attachment Example: 

`couchdb-dump.php -X -a -H localhost -p 5984 -d test > dump.json`

OPTIONS:

* `-h`                 Display this help message.
* `-e`                 Turn php error reporting ON.
* `-H <HOSTNAME>`      Hostname or IP of CouchDB server (default: 'localhost').
* `-p <PORT>`          Port of CouchDB server (default: 5984).
* `-d <DATABASE>`      Database to dump.
* `-a`                 Fetch attachments inline (capture them in base64 encoded format).
* `-X`                 No revisions history in dump.
* `-A`                Fetch attachments binary (Download them to current folder).
* `-y <PHP_FILE>`      Include this PHP script that returns callback/function to check if document/revision needs to be dumped.

###Usage for RESTORE with `couchdb-restore.php`

#####Basic Example: 

`couchdb-restore.php -H localhost -p 5984 -d test -f dump.json`

OPTIONS:

* `-h` Display this help message.
* `-e`                 Turn php error reporting ON.
* `-H <HOSTNAME>`      Hostname or IP of CouchDB server (default: 'localhost').
* `-p <PORT>`          Port of CouchDB server (default: 5984).
* `-d <DATABASE>`      Database to restore.
* `-f <FILENAME>`      JSON file to restore.
* `-D`                 Drop and create database, if needed 
(default: create db, only if it does not exist).
* `-F`                 Force restore on existing DB with documents.
* `-a`                 Restore inline attachments (from base64 encoded format).

