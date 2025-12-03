import json
import sys

#import kazoo
#from kazoo.request_objects import KazooRequest
import modded_kazoo_sdk as kazoo
from modded_kazoo_sdk.request_objects import KazooRequest
from datetime import datetime, timedelta
import pytz
import time
import helperfunctions


def findLongRunningCalls(KazSess, acctId, acctName, kwargsNotUsed):
    '''
    Gets all calls that have been running for longer than X hours

    Whats the worst that could happen: Pretty safe! Might overwhelm API server, but extremely unlikely.
    '''
    global howLong_sec
    try:
        howLong_sec
    except:
        howLong_sec = helperfunctions.getInt("What is the maximum call length before a call is considered long running? (hours)") * 3600
    longCalls = []
    request = KazooRequest(f'/accounts/{acctId}/channels?page_size=0', method='get')
    resp = KazSess._execute_request(request)
    for call in resp.get("data", []):
        call_duration_sec = call.get("timestamp", (time.time() + 62167219200)) - time.time()
        if call_duration_sec > howLong_sec:
            call_info = {
                "account_id": acctId, "account_name": acctName,
                "interaction_id": call.get("interaction_id", "No interaction_id found"),
                "call_id": call.get("uuid", "No call_id found"),
                "other_leg": call.get("other_leg", "No other_leg found")
            }
            longCalls.append(call_info)
    return longCalls

def getCDRsForMonth(KazSess, acctId, acctName, kwargsNotUsed):
    '''
    Gets all call records for a given month and year

    Whats the worst that could happen: Pretty safe! Might overwhelm API server, but extremely unlikely.
    This is very new functionality. Please double and triple check any output especially if you intend to use it for billing
    '''
    def get_month_year():
        today = datetime.today()
        while True:
            user_input = input("Enter month and year (MM-YYYY): ")
            try:
                date_obj = datetime.strptime(user_input, "%m-%Y")
                if date_obj.year < today.year or (date_obj.year == today.year and date_obj.month < today.month):
                    return {"month": date_obj.month, "year": date_obj.year}
                else:
                    print("Date must be in the past and not the current month.")
            except ValueError:
                print("Invalid format. Please use MM-YYYY (e.g., 01-1983).")

    def get_month_start_end_timestamps(month, year):
        while True:
            timezone_str = input("Enter the time zone (e.g., 'UTC', 'America/New_York', 'Pacific/Auckland'): ")
            try:
                tz = pytz.timezone(timezone_str)
                break
            except pytz.UnknownTimeZoneError:
                print("Invalid time zone. Please try again.")

        start_dt = tz.localize(datetime(year, month, 1, 0, 0, 0))

        if month == 12:
            next_month_dt = tz.localize(datetime(year + 1, 1, 1, 0, 0, 0))
        else:
            next_month_dt = tz.localize(datetime(year, month + 1, 1, 0, 0, 0))

        end_dt = next_month_dt - timedelta(seconds=1)
        return {
            "start_ts": int(start_dt.timestamp()) + 62167219200,
            "end_ts": int(end_dt.timestamp()) + 62167219200
        }
    global cdr_time_range
    try:
        cdr_time_range
    except:
        month_year = get_month_year()
        cdr_time_range = get_month_start_end_timestamps(month_year['month'], month_year['year'])
    collected_cdrs = []
    next_start_key = None
    last_ts = cdr_time_range['end_ts']
    kaz_exceptions_since = 0
    while last_ts >= cdr_time_range['start_ts'] or next_start_key is not None:
        # if the last_ts minus a day is less than cdr_time_range['end_ts'] then use cdr_time_range['end_ts']
        # else, use last_ts minus a day
        if (last_ts - 86400) > cdr_time_range['end_ts']:
            next_ts = cdr_time_range['end_ts']
        else:
            next_ts = last_ts - 86400
        try:
            # use the start_key from the last loop if there is one...
            if next_start_key is None:
                request = KazooRequest(f'/accounts/{acctId}/cdrs/interaction?created_from={next_ts}&created_to={last_ts}&page_size=1000', method='get')
            else:
                request = KazooRequest(f'/accounts/{acctId}/cdrs/interaction?created_from={next_ts}&created_to={last_ts}&start_key={next_start_key}&page_size=1000',method='get')
            # execute the request...
            resp = KazSess._execute_request(request)
        except kazoo.exceptions.KazooApiError as e:
            # handle retries on db faults. This is expected sometimes...
            if kaz_exceptions_since == 0:
                kaz_exceptions_since = time.time()
            if time.time() - kaz_exceptions_since > 600:
                print("We encountered kazoo exceptions for over 10 minutes. Please check the health of the cluster and try again later.")
                sys.exit()
            print("Getting DB error waiting 30 seconds for indices to generate and then will try again")
            time.sleep(30)
            continue
        # Add the CDRs to the collected data
        collected_cdrs += resp['data'].copy()
        # Reset kazoo exception tracker
        kaz_exceptions_since = 0
        # Pull the next_start_key if there is one
        next_start_key = resp.get("next_start_key", None)
        print(f"Successful fetch of {len(resp['data'])} records from account {acctName} ({acctId}) from {last_ts} to {next_ts}. next_start_key is {next_start_key}")
        # If there is no next_start_key, then we go everything and lets set last_ts to reflect this
        if next_start_key is None:
            last_ts = next_ts

    return collected_cdrs

def getUserData(KazSess, acctId, acctName, kwargsNotUsed):
    '''
    Gets all Users first name, last name, email and if they are an admin

    Whats the worst that could happen: Pretty safe! Might overwhelm API server, but extremely unlikely.
    Careful about what you do with this data, it's your customers PII!
    '''
    thisAccountsUsers = []
    users = helperfunctions.pagedApiCallToEnd(KazSess, 'get', '/accounts/%s/users' % (acctId,))
    for user in users:
        thisAccountsUsers.append(
            {
                "account_name": acctName,
                "first_name": user.get("first_name", ""),
                "last_name": user.get("last_name", ""),
                "email": user.get("email", ""),
                "presence_id": user.get("presence_id", ""),
                "priv_level": user.get("priv_level", "")
            }
        )

    return thisAccountsUsers

def enableVmTrans(KazSess, acctId, acctName, kwargsNotUsed):
    '''
    Enables transcription on all devices on all sub accounts

    Whats the worst that could happen: Pretty safe! Might overwhelm API server, but unlikely.
    Could potentially cost reseller more in transcription fees
    '''
    return helperfunctions.objectNormalize(KazSess, acctId, acctName, 'vmboxes', {"transcribe": True})


def disableVmTrans(KazSess, acctId, acctName, kwargsNotUsed):
    '''
    Disables transcription on all devices on all sub accounts

    Whats the worst that could happen: Pretty safe! Might overwhelm API server, but unlikely.
    '''
    return helperfunctions.objectNormalize(KazSess, acctId, acctName, 'vmboxes', {"transcribe": False})



def billingReport(KazSess, acctId, acctName, kwargsNotUsed):
    '''
    Collects a report of normally billable items

    Whats the worst that could happen: Pretty safe! Might overwhelm API server, but unlikely
    '''
    billableItems = {}
    billableItems['acctName'] = acctName

    # VM Transcription needs  special handling!
    vmboxTranscriptionData = helperfunctions.pagedApiCallToEnd(KazSess, 'get', '/accounts/%s/vmboxes?has_key=transcribe&filter_transcribe=true' % (acctId,), None, False)
    for vmbox in vmboxTranscriptionData:
        if "vm_transcription" in billableItems:
            billableItems["vm_transcription"] += 1
        else:
            billableItems["vm_transcription"] = 1

    # App store needs special handling!
    appStoreData = helperfunctions.pagedApiCallToEnd(KazSess, 'get', '/accounts/%s/apps_store' % (acctId,))
    for appStoreDatum in appStoreData:
        if "name" in appStoreDatum:
            billableItems["app_store_" + appStoreDatum['name']] = 1

    # Numbers need special handling!
    phoneNumbers = helperfunctions.pagedApiCallToEnd(KazSess, 'get', '/accounts/%s/phone_numbers' % (acctId,))

    agNumberData = {}
    for phoneNumber, numberData in phoneNumbers.items():
        numberPrefix = phoneNumber[0:5]
        if numberPrefix in ["+1800", "+1833", "+1844", "+1855", "+1866", "+1877", "+1888", "+1822"]:
            if "did_toll_free" not in agNumberData:
                agNumberData["did_toll_free"] = 1
            else:
                agNumberData["did_toll_free"] += 1
        else:
            if "did_local" not in agNumberData:
                agNumberData["did_local"] = 1
            else:
                agNumberData["did_local"] += 1
        for feature in numberData.get("features", []):
            featureKey = "did_feature_" + feature
            if featureKey not in agNumberData:
                agNumberData[featureKey] = 1
            else:
                agNumberData[featureKey] += 1

    billableItems = {**billableItems, **agNumberData}

    # Generic counts now!
    def countObjects(acctId, objectType, segregateOn=[]):
        objectData = helperfunctions.pagedApiCallToEnd(KazSess, 'get', '/accounts/%s/%s' % (acctId, objectType))
        if segregateOn == []:
            return {objectType: len(objectData)}
        else:
            objectCountsDict = {}
            for objectDatum in objectData:
                # Dang this is nasty sorry future me...
                # Basically I need to do all this crap because some dev who shall remain nameless is putting an
                # object type in nested dicts instead of the object itself. I'm... not a fan
                # anyhow, this resolves that weirdness
                for segregateSubItem in segregateOn:
                    if isinstance(objectDatum, dict):
                        objectDatum = objectDatum.get(segregateSubItem, 'unknownType')

                if not isinstance(objectDatum, str):
                    objectDatum = 'unknownType'
                # Done with weirdness "objectDatum" is now basically the "type" of object

                objectFullName = objectType + "_" + objectDatum
                if objectFullName not in objectCountsDict:
                    objectCountsDict[objectFullName] = 1
                else:
                    objectCountsDict[objectFullName] = objectCountsDict[objectFullName] + 1
            objectCounts = {}
            for key, value in objectCountsDict.items():
                objectCounts[key] = value
            return objectCounts

    billableItems = {**billableItems, **countObjects(acctId, 'users')}
    billableItems = {**billableItems, **countObjects(acctId, 'devices', ['device_type'])}
    try:
        billableItems = {**billableItems, **countObjects(acctId, 'qubicle_queues', ['offering'])}
    except:
        pass
    try:
        billableItems = {**billableItems, **countObjects(acctId, 'qubicle_recipients', ['recipient','offering'])}
    except:
        pass

    return billableItems


def numbersFix(KazSess, acctId, acctName, kwargsNotUsed):
    '''
    Runs the "fix" number function for all descendant accounts

    Whats the worst that could happen: Pretty safe! Might overwhelm API server, but unlikely
    '''
    request = KazooRequest('/accounts/%s/phone_numbers/fix' % (acctId,), method='post')
    KazSess._execute_request(request)
    return {"result": "Number fix requested for account %s (%s)" % (acctName, acctId)}


def rebootAllPhones(KazSess, acctId, acctName, kargsNotUsed):
    '''
    Reboots all phones!

    Whats the worst that could happen: Use caution, users who connect their computers through
    their VOIP phones may not appreciate this. Lord forbid someone has a server connected through
    their VOIP phone. Before you say "no one would do THAT"... I've SEEN IT! So, be careful
    '''
    phonesRebooted = []

    for dev in helperfunctions.pagedApiCallToEnd(KazSess, 'get', '/accounts/%s/%s' % (acctId, 'devices')):
        if dev.get('device_type', 'unknown device type') == 'sip_device':
            print("Rebooting \"%s\" device in account %s (%s)" % (dev.get("name", ""), acctName, acctId))
            request = KazooRequest("/accounts/%s/devices/%s/sync" % (acctId, dev['id']), method="post")
            KazSess._execute_request(request)
            phonesRebooted.append({"id": dev.get("id", ""), "name": dev.get("name", ""), "device_type": dev.get("device_type", ""), "reboot_requested": True})
    return phonesRebooted

def disableAccount(KazSess, acctId, acctName, kwargsNotUsed):
    '''
    Sets enabled = False on all accounts where its not already set

    Whats the worst that could happen: VERY DANGEROUS! Will have massive repercussions
    if run on the cluster's top level account! USE EXTREME CAUTION!!
    '''
    global dangerAcknowledgment
    try:
        dangerAcknowledgment
    except:
        dangerAcknowledgment  = input(
            "This is a very dangerous operation!\nPlease type \"I accept personal responsibility for the result of running this script\" (without quotes) to acknowledge and continue: "
        )

    if dangerAcknowledgment != "I accept personal responsibility for the result of running this script":
        print("Acknowledgment text incorrect aborting!")
        quit()
        return False

    accountData = KazSess.get_account(acctId)['data']
    if accountData['enabled']:
        accountData['enabled'] = False
        KazSess.update_account(acctId, accountData)
        updatedAccountData = KazSess.get_account(acctId)['data']
        return {"account_name": acctName, "enabledBeforeRun": True, "updated": True, "enabledAfterRun": updatedAccountData['enabled']}
    else:
        return {"account_name": acctName, "enabledBeforeRun": False, "updated": False, "enabledAfterRun": False}

