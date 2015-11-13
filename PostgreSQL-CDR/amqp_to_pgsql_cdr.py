#!/usr/bin/env python
"""
Prodosec.com / Stormqloud.ca 2015

Version 20151111

Ideas taken from Radius CDR Ruby module floating around the interwebs.

wlloyd@stormqloud.ca

Bugs exist..

"""

import pika
import sys
import datetime
import csv
import json
import psycopg2
import logging

from pytz import timezone
from dateutil import tz

DEBUG = 1
#DEBUG = False

RABBITAMQP = 'RABBIT IP ADDRESS' # standard Rabbit port

DB_HOST = 'POSTGRES HOST or IIP' 
DB_NAME = 'kazoocdr' 
DB_USER = 'root' # cdr insert only user..
DB_PASSWORD = 'kazooisreallyreallybest'

def connect_db():
    try:

        conn_string = "host='%s' dbname='%s' user='%s' password='%s'" % (DB_HOST, DB_NAME, DB_USER, DB_PASSWORD)
        
        if DEBUG>1: print conn_string 

        con = psycopg2.connect(conn_string)
        cur_prod = con.cursor()
        # cur_prod.execute(sql)
        # con.commit()

    except psycopg2.DatabaseError, e:
        print 'Error %s' % e.pgcode
        sys.exit()

    return cur_prod

def insert_sql(sql):
    try:

        conn_string = "host='%s' dbname='%s' user='%s' password='%s'" % (DB_HOST, DB_NAME, DB_USER, DB_PASSWORD)
        
        if DEBUG: print conn_string 

        con = psycopg2.connect(conn_string)
        cur_prod = con.cursor()
        cur_prod.execute(sql)
        con.commit()

    except psycopg2.DatabaseError, e:
        if e.pgcode in ('23505'):
            print 'Dup Error %s' % e.pgcode
        else:
            print 'Error %s' % e.pgcode
            sys.exit()






def is_string(a):
    return  type(a) == str

def utc_est(utc_time):

    from datetime import datetime
    from dateutil import tz

    from_zone = tz.gettz('UTC')
    to_zone = tz.gettz('America/New_York')

    utc = datetime.strptime(utc_time, '%Y-%m-%d %H:%M:%S')

    utc = utc.replace(tzinfo=from_zone)
    est = utc.astimezone(to_zone)
    return est


def callback(ch, method, properties, body):

    if DEBUG>1 : print " [x] %r:%r" % (method.routing_key, body,)

    obj = json.loads(body)
    #print method.routing_key.split('call.CHANNEL_DESTROY.', 1)[1]

    epochSeconds_at_gregorian = 62167219200
    utc_calldate = datetime.datetime.fromtimestamp(int(obj['Timestamp'])-epochSeconds_at_gregorian).strftime('%Y-%m-%d %H:%M:%S')
    calldate = utc_est(utc_calldate)

    Switch_URI = obj['Switch-URI']
    Switch_URL = obj['Switch-URL'].strip()
    To_Tag = ''
    if obj.has_key("To-Tag"):
        To_Tag = obj['To-Tag'].strip() # if there is X-AUTH-IP, no this field

    From_Tag = ''
    if obj.has_key("From-Tag"):
        From_Tag = obj['From-Tag'].strip() # if there is X-AUTH-IP, no this field

    Custom_SIP_Headers = ''
    # check dictionary Custom-SIP-Headers length
    if len(obj['Custom-SIP-Headers']) > 0:
        Custom_SIP_Headers = "X-AUTH-IP:% s" % (obj['Custom-SIP-Headers']['X-AUTH-IP'])

    Callee_ID_Name = ''
    if obj.has_key("Callee-ID-Name"):
        Callee_ID_Name = obj['Callee-ID-Name'].strip()

    Callee_ID_Number = ''
    if obj.has_key("Callee-ID-Number"):
        Callee_ID_Number = obj['Callee-ID-Number'].strip()

    Caller_ID_Name = obj['Caller-ID-Name'].strip()
    Caller_ID_Number = obj['Caller-ID-Number'].strip()
    Media_Server = obj['Media-Server'].strip()

    Presence_ID = ''
    if obj.has_key("Presence-ID"):
        Presence_ID = obj['Presence-ID'].strip()

    From = ''
    if obj.has_key("From"):
        From = obj['From'].strip()

    To = ''
    to_user = ''
    if obj.has_key("To"):
        To = obj['To'].strip()
        if len(To.split('@')[0]) >= 10:
            to_user = To.split('@')[0]

    From_Uri = ''
    if obj.has_key("From-Uri"):
        From_Uri = obj['From-Uri'].strip()

    To_Uri = ''
    if obj.has_key("To-Uri"):
        To_Uri = obj['To-Uri'].strip()


    Ringing_Seconds = obj['Ringing-Seconds'].strip()

    Billing_Seconds = obj['Billing-Seconds'].strip()
    Duration_Seconds = obj['Duration-Seconds'].strip()

    duration = obj['Billing-Seconds'].strip()
    billsec = obj['Duration-Seconds'].strip()

    Remote_SDP = ''
    if obj.has_key("Remote-SDP"):
        Remote_SDP = obj['Remote-SDP'].strip()

    User_Agent = ''
    if obj.has_key("User-Agent"):
        User_Agent = obj['User-Agent'].strip()

    Request = ''
    if obj.has_key("Request"):
        Request = obj['Request'].strip()

    Hangup_Code = '' 
    if obj.has_key("Hangup-Code"):
        Hangup_Code = obj['Hangup-Code'].strip()

    Hangup_Cause = '' 
    if obj.has_key("Hangup-Cause"):
        Hangup_Cause = obj['Hangup-Cause'].strip()

    Disposition = '' 
    if obj.has_key("Disposition"):
        Disposition = obj['Disposition'].strip()

    Other_Leg_Call_ID = '' 
    if obj.has_key("Other-Leg-Call-ID"):
        Other_Leg_Call_ID = obj['Other-Leg-Call-ID']

    Other_Leg_Destination_Number = '' 
    if obj.has_key("Other-Leg-Destination-Number"):
        Other_Leg_Destination_Number = obj['Other-Leg-Destination-Number'].strip()

    Other_Leg_Caller_ID_Number = '' 
    if obj.has_key("Other-Leg-Caller-ID-Number"):
        Other_Leg_Caller_ID_Number = obj['Other-Leg-Caller-ID-Number'].strip()

    Other_Leg_Caller_ID_Name = '' 
    if obj.has_key("Other-Leg-Caller-ID-Name"):
        Other_Leg_Caller_ID_Name = obj['Other-Leg-Caller-ID-Name'].strip()


    Other_Leg_Direction = '' 
    if obj.has_key("Other-Leg-Direction"):
        Other_Leg_Direction = obj['Other-Leg-Direction'].strip()

    Call_Direction = '' 
    if obj.has_key("Call-Direction"):
        Call_Direction = obj['Call-Direction'].strip()


    Timestamp = obj['Timestamp']

    Account_ID = '' 
    Realm = '' 
    Custom_Channel_Vars = '' 
    if obj.has_key("Custom-Channel-Vars"):

        # check if Account-ID exists
        if obj['Custom-Channel-Vars'].has_key("Account-ID"):
            Account_ID = obj['Custom-Channel-Vars']['Account-ID'].strip()

        # check if Realm exists
        if obj['Custom-Channel-Vars'].has_key("Realm"):
            Realm = obj['Custom-Channel-Vars']['Realm'].strip()

        temp = ''
        for key, value in obj['Custom-Channel-Vars'].items():
            temp += key.strip() + ":"
            temp += value.strip() 
            temp += ","
        Custom_Channel_Vars = temp
        #print "****************%s****************************" % (Custom_Channel_Vars)
        #exit


    Call_ID = '' 
    if obj.has_key("Call-ID"):
        Call_ID = obj['Call-ID'].strip()

    Node = '' 
    if obj.has_key("Node"):
        Node = obj['Node'].strip()

    Msg_ID = '' 
    if obj.has_key("Msg-ID"):
        Msg_ID = obj['Msg-ID'].strip()

    App_Version = '' 
    if obj.has_key("App-Version"):
        App_Version = obj['App-Version'].strip()


    App_Name = '' 
    if obj.has_key("App-Name"):
        App_Name = obj['App-Name'].strip()

    Event_Name = '' 
    if obj.has_key("Event-Name"):
        Event_Name = obj['Event-Name'].strip()

    Event_Category = '' 
    if obj.has_key("Event-Category"):
        Event_Category = obj['Event-Category'].strip()


    #print is_string(Switch_URI)

    sql  = "INSERT INTO call "
    sql += "("
    sql += "\"switch_uri\", "
    sql += "\"switch_url\", "
    sql += "\"to_tag\", "
    sql += "\"from_tag\", "
    sql += "\"custom_sip_headers\", "
    sql += "\"callee_id_name\", "
    sql += "\"callee_id_number\", "
    sql += "\"caller_id_name\", "
    sql += "\"caller_id_number\", "
    sql += "\"media_server\", "
    sql += "\"presence_id\", "
    sql += "\"request\", "
    sql += "\"from\", "
    sql += "\"to\", "
    sql += "\"from_uri\", "
    sql += "\"to_uri\", "
    sql += "\"ringing_seconds\", "
    sql += "\"remote_sdp\", "
    sql += "\"user_agent\", "
    sql += "\"hangup_code\", "
    sql += "\"hangup_cause\", "
    sql += "\"disposition\", "
    sql += "\"other_leg_call_id\", "
    sql += "\"other_leg_destination_number\", "
    sql += "\"other_leg_caller_id_number\", "
    sql += "\"other_leg_caller_id_name\", "
    sql += "\"other_leg_direction\", "
    sql += "\"call_direction\", "
    sql += "\"timestamp\", "
    sql += "\"custom_channel_vars\", "
    sql += "\"call_id\", "
    sql += "\"node\", "
    sql += "\"msg_id\", "
    sql += "\"app_version\", "
    sql += "\"app_name\", "
    sql += "\"event_name\", "
    sql += "\"account_id\", "
    sql += "\"realm\", "
    sql += "\"calldate\", "
    sql += "\"duration\", "
    sql += "\"billsec\", "
    sql += "\"to_user\", "
    sql += "\"event_category\" "
    sql += ") "
    sql += "VALUES "
    sql += "("
    sql += "'%s', " % ( Switch_URI ) 
    sql += "'%s', " % ( Switch_URL ) 
    sql += "'%s', " % ( To_Tag ) 
    sql += "'%s', " % ( From_Tag ) 
    sql += "'%s', " % ( Custom_SIP_Headers ) 
    sql += "'%s', " % ( Callee_ID_Name ) 
    sql += "'%s', " % ( Callee_ID_Number ) 
    sql += "'%s', " % ( Caller_ID_Name ) 
    sql += "'%s', " % ( Caller_ID_Number ) 
    sql += "'%s', " % ( Media_Server ) 
    sql += "'%s', " % ( Presence_ID ) 
    sql += "'%s', " % ( Request ) 
    sql += "'%s', " % ( From ) 
    sql += "'%s', " % ( To ) 
    sql += "'%s', " % ( From_Uri ) 
    sql += "'%s', " % ( To_Uri ) 
    sql += "'%s', " % ( Ringing_Seconds ) 
    sql += "'%s', " % ( Remote_SDP ) 
    sql += "'%s', " % ( User_Agent ) 
    sql += "'%s', " % ( Hangup_Code ) 
    sql += "'%s', " % ( Hangup_Cause ) 
    sql += "'%s', " % ( Disposition ) 
    sql += "'%s', " % ( Other_Leg_Call_ID ) 
    sql += "'%s', " % ( Other_Leg_Destination_Number ) 
    sql += "'%s', " % ( Other_Leg_Caller_ID_Number ) 
    sql += "'%s', " % ( Other_Leg_Caller_ID_Name ) 
    sql += "'%s', " % ( Other_Leg_Direction ) 
    sql += "'%s', " % ( Call_Direction ) 
    sql += "'%s', " % ( Timestamp ) 
    sql += "'%s', " % ( Custom_Channel_Vars ) 
    sql += "'%s', " % ( Call_ID ) 
    sql += "'%s', " % ( Node ) 
    sql += "'%s', " % ( Msg_ID ) 
    sql += "'%s', " % ( App_Version ) 
    sql += "'%s', " % ( App_Name ) 
    sql += "'%s', " % ( Event_Name ) 
    sql += "'%s', " % ( Account_ID ) 
    sql += "'%s', " % ( Realm ) 
    sql += "'%s', " % ( calldate ) 
    sql += "'%s', " % ( billsec ) 
    sql += "'%s', " % ( duration ) 
    sql += "'%s', " % ( to_user ) 
    sql += "'%s' " % ( Event_Category ) 
    sql += ")"
#    sql += " WHERE NOT EXISTS (SELECT * from call WHERE call.call_id = '%s' )" % (Call_ID,)

    if DEBUG: 
        print " ============================================="
        if DEBUG >1:
            print "\n->%s" % (sql)


    insert_sql(sql)


def main():
    logging.basicConfig()
    if DEBUG:
        print 'Connect to PGSQL'

    db_conn = connect_db()

    connection = pika.BlockingConnection(pika.ConnectionParameters(
            host=RABBITAMQP))

    channel = connection.channel()

    channel.exchange_declare(exchange='callevt',
                             type='topic')

    #result = channel.queue_declare(exclusive=True, queue='cdr_csv')
    result = channel.queue_declare(exclusive=False, queue='pgcdr')
    queue_name = result.method.queue

    channel.queue_bind(exchange='callevt',
                       queue='pgcdr',
                       routing_key="call.CHANNEL_DESTROY.*")

    if DEBUG:
        print ' [*] Waiting for CDR records. To exit press CTRL+C'

    channel.basic_consume(callback, queue=queue_name, no_ack=True)
    channel.start_consuming()
    sys.exit()


if __name__ == '__main__':
    main()
