import datetime
##############################################################################
#### Functions to process fields #############################################
##############################################################################

def to_datetime(timestamp):
    epochSeconds_at_gregorian = 62167219200
    return datetime.datetime.fromtimestamp(int(timestamp)-epochSeconds_at_gregorian)

def custom_channel_vars(custom):
    return ",".join(["%s:%s" % (key.strip(), value.strip()) for key, value in custom.items()])
