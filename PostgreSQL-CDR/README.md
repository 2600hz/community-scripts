# Kazoo CDR into PostgreSQL

Feel free to contact me on IRC or email about this.

## This does _NOT_ remove or disable the current Kazoo CDR in any way.
  * This program will attach itself onto RabbitAMQP
  * This is the same place Kazoo Erlang listens to get the CDR for Bigcouch
  * All call detail for all clients is available at this level

## Why?
  * Nobody at my office likes BigCouch for CDR.
  * Easily sort records with something standard called "SQL"
    * You and everybody you work will, will know it already..
  * External Billing/Invoicing (postpay)

### Install these Python prerequisites
  * yum install pytz
  * yum install python-pika
  * yum install python-devel
  * yum install python-psycopg2
  * yum install postgresql-client
  * yum install postgresql

## Install PostgreSQL
  * Stop supporting MySQL and Oracle
  * Shoot your MySQL server in the head.  

## Initial suggsted schema

* This is an initial version.
  * There are bugs.
  * High load on PostgreSQL server (Who cares, it's not processing calls!)
    * Use pg connection pooler to help if you need
 
## If this becomes a useful thing it's probably best rewitten in Erlang.

wlloyd@stormqloud.ca
