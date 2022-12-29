import json

import kazoo
from kazoo.request_objects import KazooRequest
import helperfunctions

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
    billableItems = {**billableItems, **countObjects(acctId, 'qubicle_queues', ['offering'])}
    billableItems = {**billableItems, **countObjects(acctId, 'qubicle_recipients', ['recipient','offering'])}

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

