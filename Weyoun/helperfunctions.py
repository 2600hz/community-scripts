import kazoo
from kazoo.request_objects import KazooRequest
import requests
import json
import base64

def pagedApiCallToEnd(KazSess, method, basepath, start_key=None, AppendQuestionMarkToUrl=True):
    global pagedApiCallToEndPageSize
    try:
        pagedApiCallToEndPageSize
    except:
        pagedApiCallToEndPageSize = int(input(
            "What page size should we use for API calls? "
            "Depends on cluster config (use 0 to disable paging when possible, 50 is pretty safe if you have no idea): "
        ))

    if int(pagedApiCallToEndPageSize) == 0:
        if AppendQuestionMarkToUrl:
            fullpath = basepath + "?paginate=false"
        else:
            fullpath = basepath + "&paginate=false"
    else:
        if AppendQuestionMarkToUrl:
            fullpath = basepath + "?page_size=%s" % (str(pagedApiCallToEndPageSize))
        else:
            fullpath = basepath + "&page_size=%s" % (str(pagedApiCallToEndPageSize))

    if start_key is not None:
        fullpath += "&start_key=%s" % (start_key)
    request = KazooRequest(fullpath, method=method)
    resp = KazSess._execute_request(request)

    if 'next_start_key' in resp:
        # :/ numbers API's payload is substantally different... grr. compensating for that...
        if "numbers" in resp['data'] and "/phone_numbers" in basepath:
            return {**resp['data']['numbers'], **pagedApiCallToEnd(KazSess, method, basepath, resp['next_start_key'])}
        # Everything else works just fine combining them normally!
        else:
            return resp['data'] + pagedApiCallToEnd(KazSess, method, basepath, resp['next_start_key'])
    else:
        # More special handling for numbers
        if "numbers" in resp['data'] and "/phone_numbers" in basepath:
            return resp['data']['numbers']
        else:
            return resp['data']


def objectNormalize(KazSess, acctId, acctName, objectType, valuesToSet, updateOnlyIfSet=False):
    '''
    Normalizes a function across all of this device type across all accounts
    Hidden in helper functions so that it cannot be used interactively by users.
    Also changed its format from the kwargs format that was compatable, to explicitly defined variables
    that way it's not easy to make it work interactively anymore.... because you really shouldnt
    '''
    objModList = []
    for obj in pagedApiCallToEnd(KazSess, 'get', '/accounts/%s/%s' % (acctId, objectType)):
        doUpdate = False
        modListEntry = {objectType+"_id": obj['id']}
        if "name" in obj:
            modListEntry['name'] = obj['name']
        fullpath = "/accounts/%s/%s/%s" % (acctId, objectType, obj['id'])
        request = KazooRequest(fullpath, method='get')
        objData = KazSess._execute_request(request)['data']

        for key, value in valuesToSet.items():
            modListEntry["old_"+key] = objData.get(key, '')
            if key not in objData and updateOnlyIfSet:
                continue
            if objData.get(key, "") != value:
                objData[key] = value
                doUpdate = True

        if doUpdate:
            request = KazooRequest(fullpath, method='post')
            objData = KazSess._execute_request(request, data=objData)['data']
            print("Updated %s: object id %s in account %s (%s)" % (objectType, obj['id'], acctName, acctId))
        else:
            print("No update needed to %s: object id %s in account %s (%s)" % (objectType, obj['id'], acctName, acctId))

        modListEntry["update_done"] = doUpdate

        for key, value in valuesToSet.items():
            modListEntry["new_"+key] = objData.get(key, "")

        objModList.append(modListEntry)

    return objModList

def getInt(question):
    outputVal = input('%s: ' % (question, ))
    try:
        outputVal = int(outputVal)
    except:
        print("Please enter an integer value!")
        outputVal = getInt(question)
    return outputVal


def getYesNo(question):
    yn = input('%s (y/n): ' % (question, ))
    if yn == 'y':
        return True
    elif yn == 'n':
        return False
    else:
        print('Please enter JUST a y for yes or a n for no')
        return getYesNo(question)


def interactiveKazooAuth ():
    '''
    Interactively gets credentials with a user via shell
    :return: kazoo session
    '''
    KazBaseUrl = input("Please enter your API URL here (default - https://ui.zswitch.net/v2): ")
    if KazBaseUrl == "":
        print("defaulting to https://ui.zswitch.net/v2")
        KazBaseUrl = "https://ui.zswitch.net/v2"
    # trim off the last character if it's a /
    if(KazBaseUrl[-1] == "/"):
        KazBaseUrl = KazBaseUrl[:-1]
    authmethod = input("Type 'up' to auth with a username/password, 'ak' to use an API key or 'tk' to use an auth token (uses an auth token to get an api key for auth): ")

    if authmethod == 'up':
        KazUser = input("Please enter your KAZOO username: ")
        KazPass = input("Please enter your KAZOO password: ")
        KazAcct = input("Please enter your KAZOO account name: ")
        KazSess = kazoo.Client(username=KazUser, password=KazPass, account_name=KazAcct, base_url=KazBaseUrl)
    elif authmethod == 'ak':
        KazApiKey = input("Please enter your KAZOO API Key: ")
        KazSess = kazoo.Client(api_key=KazApiKey, base_url=KazBaseUrl)
    elif authmethod == 'tk':
        KazAuthTok = input("Please enter your auth token: ")
        try:
            KazAccountID = json.loads(base64.b64decode(bytes(KazAuthTok.split('.')[1], 'utf-8') + b'==', validate=False))['account_id']
        except:
            KazAccountID = input("Please enter an account ID to auth with: ")
        url = "%s/accounts/%s/api_key" % (KazBaseUrl, KazAccountID)
        headers = {'X-Auth-Token': KazAuthTok}
        response = requests.request("GET", url, headers=headers, data={})
        response = json.loads(response.text)
        KazSess = kazoo.Client(api_key=response['data']['api_key'], base_url=KazBaseUrl)
    KazSess.authenticate()

    return KazSess
