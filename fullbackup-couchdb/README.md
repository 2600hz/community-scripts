# Full Backup CouchDB

This script takes a backup of all CouchDB databases, compresses the backup directory 
<br />
and moves it to a SSH-enabled remote server.
<br />
If an error occurs with any of the steps, an alert email is sent with details about the error.
<br />
  
## \-\-\-\-\- Installation \-\-\-\-\-

#### Get Daniele Bailo's couchdb-dump
```bash
cd fullbackup-couchdb
wget https://raw.githubusercontent.com/danielebailo/couchdb-dump/master/couchdb-backup.sh
chmod +x *.sh
```

#### Check required software
```bash
./preflight-check.sh
```

#### Edit configs
```bash
cp fullbackup-couchdb.conf.example fullbackup-couchdb.conf
nano fullbackup-couchdb.conf

```

#### Test
```bash
./fullbackup-couchdb.sh
```

#### Crontab
```bash
cp crontab-fullbackup-couchdb /etc/cron.d/fullbackup-couchdb
systemctl restart cron.service
```
The crontab file assumes you have installed the community-scripts under /opt/.
<br />
If the scripts reside elsewhere, fix the path in the crontab file.
<br />


## \-\-\-\-\- Notes \-\-\-\-\-

There is nothing Kazoo specific about this script. It will work on any system that uses CouchDB.
<br />
<br />
Only use the SFTP option if you are unable to safely use SCP.
<br />
This script is unable to detect if the SFTP client has actually transferred the backup file to the remote server. 
<br />
It only knows if the connection has been successfully opened and closed.
<br />

  
## \-\-\-\-\- Security \-\-\-\-\-

Always chroot/chjail the remote server’s user. 
<br />
The users should only be able to run the scp-server or sftp-server programs.
<br />
The user should never be able to get SSH-shell access, even if the shell and user are in a chjail.
<br />
<br />
The remote server’s other backup directories should not be accessible to the user.
<br />
In case of a break-in, the bad-guy will only hose these backups not all your backups.
<br />
An additional protection scheme includes either periodically moving these backups to another directory 
<br />
or removing write permission to the existing backups.
<br />
  
<br />
  
Sponsored by GBC Networks Oy (http://gbc.fi)
