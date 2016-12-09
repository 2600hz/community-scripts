# Clone Tool
=================

Erlang tool to copy all databases from one Bigcouch cluster to another, with options for CDRs and voicemails

## Usage

### compile `db_clone`
```bash
make clean; make
```

### run `db_clone`

```bash
./db_clone [-s source] [-t target] [-e exclude] [-max_cr_age max-cdrs-age] [-max_vm_age max-vm-age] [-dead_accounts list-of-dead-accounts] [databases]

source: source database url, default http://127.0.0.1:5984/
target: target database url, default http://127.0.0.1:15984/
exclude:
    modb: exclude databases matching regexp "$account.*-\d{6}$"
    regexp: exclude databases matching provided regexp
max-cdrs-age: maximum age, in days, for CDRs. `0` means `all`. Default `none`
max-vm-age: maximum age, in days, for voicemail messages. `0` means `all`. Default `0`.
list-of-dead-accounts: This can be set to a list of account ids that may still be in the hierarchy after removing the accounts.  This will blindly remove any id in the list from any pvt_tree. Should be quoted by `'` or `"`.

NOTE: source/target URLs should ends at `/`
```

### examples

This command clones all databases from default source to default target.
```bash
./db_clone
```

Clone `accounts` and `system_config` databases from `http://source.example.com/` to `http://target.example.com:5984/`
```bash
./db_clone -s http://source.example.com/ -t http://target.example.com:5984/ accounts system_config
```

```bash
./db_clone -e '^(accounts|system_config)$'
```

```bash
./db_clone -e modb
```

```bash
./db_clone -max_cdr_age 0 -max_vm_age none
```

```bash
./db_clone -dead_accounts "2e0bcf74f7a1b2ce7e408ce2731796a3 544060f3f8af919ad79764ca8a961241 72fabca989b3102c28482c60070aac5b"
```

NOTE: Cloning databases will take a very long time (possibly days) and to ensure it is not disrupted we recommend running it in screen.

## Few Notes
* This is a one way copy, changes made during the copy will not be cloned on subsequent runs (expect newly created documents and voicemails)
* For the lowest possibility of lost changes the new cluster should be put in service as soon as the clone is complete
* No CDRs are cloned, this could be done later while the new cluster is in service, however we do not currently have a tool available to do so
* It IS safe to stop and restart the script, however remember the longer a cloned database is unused the greater a chance changes to the original will be lost

## How it works
* It reads all databases on the SOURCE system and starts cloning the list
* If the current db being cloned IS NOT an account/XX/XX/XXXXX db
  * It will get a list of all document IDs in both the SOURCE and TARGET.  It will then clone any IDs that only exist on the SOURCE
* If the current db being cloned IS an account/XX/XX/XXXX db then it will (in detail):
  * create the database on the TARGET
  * add a view to the SOURCE db
  * copy all views to the TARGET (including the newly added "clone" view)
  * query the clone view for a list of all document IDs that are NOT cdrs, acdc_stats, credit, debit, or vmbox on both SOURCE and TARGET.  It will then clone any IDs that only exist on the SOURCE
  * query the clone view for a list of all documents with attachments on both SOURCE and TARGET.  The results of the view also includes the total size of all attachements on each document.  If this differs then the attachments are copied from the SOURCE to the TARGET
  * find all vmboxes in SOURCE and OVERWRITE them on the TARGET.  This ensures if a voicemail was left while the clone ran, you can re-run it just prior to the cut over to ensure it is present.  However, this also means if you run this script after a voicemail is left via the TARGET it will be lost.  The proper use case is to run the script, then re-run it immediately BEFORE switching Kazoo to use the TARGET, but NEVER after :)
  * query the current available credit in the db and create one transaction on the TARGET to "roll-up" (represent) all the transaction history on the SOURCE.  Note, this is a single document representing the available credit at the time of the clone so the history is lost but since we dont expose it currently this is not an issue unless the client has written a tool to use it themselfs.

Sponsored by CloudPBX Inc. (http://cloudpbx.ca)
