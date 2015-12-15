import field_processors
import logging

##############################################################################
#### Directory config                                                   ######
##############################################################################
#The directory to monitor for new CDRs
MONITOR_DIR = "/tmp/cdrs"
#The directory to move successfully processed CDRs to
#Can be None if files shouldn't be moved
SUCCESS_DIR = "/tmp/success_cdrs"
#The directory to move CDRs that failed to be processed to
#Can be None if files shouldn't be moved
FAILED_DIR = "/tmp/failed_cdrs"
#Should any files already in the directory before the script
#is run be processed?
PROCESS_EXISTING = True

##############################################################################
#### Database config                                                    ######
##############################################################################
DB_HOST = '127.0.0.1'
DB_PORT = 3306
DB_NAME = 'cdr_test'
DB_USER = 'testuser' # cdr insert only user..
DB_PASSWORD = 'password'

##############################################################################
#### Logging config                                                     ######
##############################################################################
LOG_LEVEL = logging.INFO
LOG_FILE = None

##############################################################################
#### JSON fields, the corrosponding database column, and a functions to ######
#### convert from JSON value to the database value                      ######
##############################################################################
FIELDS = [
    ('Billing-Seconds', 'billing_seconds', int),
    ('Call-Direction', 'call_direction', None),
    ('Call-ID', 'call_id', None),
    ('Callee-ID-Name', 'callee_id_name', None),
    ('Callee-ID-Number', 'callee_id_number', None),
    ('Caller-ID-Name', 'caller_id_name', None),
    ('Caller-ID-Number', 'caller_id_number', None),
    ('Custom-Channel-Vars.Account-ID', 'account_id', None),
    ('Custom-Channel-Vars.Account-Name', 'account_name', None),
    ('Custom-Channel-Vars.Account-Realm', 'account_realm', None),
    ('Custom-Channel-Vars.Application-Name', 'application_name', None),
    ('Custom-Channel-Vars.Application-Node', 'application_node', None),
    ('Custom-Channel-Vars.Authorizing-ID', 'authorising_id', None),
    ('Custom-Channel-Vars.Authorizing-Type', 'authorising_type', None),
    ('Custom-Channel-Vars.Bridge-ID', 'bridge_id', None),
    ('Custom-Channel-Vars.Channel-Authorized', 'channel_authorized', None),
    ('Custom-Channel-Vars.E164-Destination', 'e164_destination', None),
    ('Custom-Channel-Vars.Global-Resource', 'global_resource', None),
    ('Custom-Channel-Vars.Inception', 'inception', None),
    ('Custom-Channel-Vars.Original-Number', 'original_number', None),
    ('Custom-Channel-Vars.Owner-ID', 'owner_id', None),
    ('Custom-Channel-Vars.Reseller-ID', 'reseller_id', None),
    ('Custom-Channel-Vars.Resource-ID', 'resource_id', None),
    ('Custom-Channel-Vars.Realm', 'realm', None),
    ('Custom-Channel-Vars.Register-Overwrite-Notify', 'register_overwrite_notify', None),
    ('Custom-Channel-Vars.Surpress-Unregister-Notifications', 'surpress_unregister_notifications', None),
    ('Custom-Channel-Vars.Username', 'username', None),
    ('Custom-Channel-Vars', 'custom_channel_vars', field_processors.custom_channel_vars),
    ('Custom-SIP-Headers.X-AUTH-IP', 'x_auth_ip', None),
    ('Disposition', 'disposition', None),
    ('Duration-Seconds', 'duration_seconds', int),
    ('Event-Category', 'event_category', None),
    ('Event-Name', 'event_name', None),
    ('From', 'from', None),
    ('From-Tag', 'from_tag', None),
    ('From-Uri', 'from_uri', None),
    ('Hangup-Cause', 'hangup_cause', None),
    ('Hangup-Code', 'hangup_code', None),
    ('Other-Leg-Call-ID', 'other_leg_call_id', None),
    ('Other-Leg-Caller-ID-Name', 'other_leg_caller_id_name', None),
    ('Other-Leg-Caller-ID-Number', 'other_leg_caller_id_number', None),
    ('Other-Leg-Destination-Number', 'other_leg_destination_number', None),
    ('Other-Leg-Direction', 'other_leg_direction', None),
    ('Presence-ID', 'presence_id', None),
    ('Request', 'request', None),
    ('Ringing-Seconds', 'ringing_seconds', None),
    ('Timestamp', 'timestamp', str),
    ('Timestamp', 'call_time', field_processors.to_datetime),
    ('To', 'to', None),
    ('To-Tag', 'to_tag', None),
    ('To-Uri', 'to_uri', None),
    ('User-Agent', 'user_agent', None)
    ]
