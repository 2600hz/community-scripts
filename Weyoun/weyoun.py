from helperfunctions import pagedApiCallToEnd, getYesNo, interactiveKazooAuth
from inspect import getmembers, isfunction
from datetime import datetime
import weyounFunctions
import json
import csv
import os

def pickAFunction(functions):
    print("Functions available:")
    for funcIndex, func in enumerate(functions):
        print("%d) %s" % (funcIndex+1, func.__name__))
    selectedFunc = input("What function would you like to run across all of your sub accounts?: ")
    try:
        if int(selectedFunc) < 1:
            print("Input not understood. Please try again")
            return pickAFunction(functions)

        print(functions[int(selectedFunc)-1].__doc__)
        if getYesNo("Is this the right function?"):
            return functions[int(selectedFunc)-1]
        else:
            print("Ok, lets try again")
            return pickAFunction(functions)
    except:
        print("Input not understood. Please try again")
        return pickAFunction(functions)


def runFunctionForAllDescendant (KazSess, rootAcct, includeSelf, func, **kwargs):
    '''
    Runs a function for each descendant of
    :param KazSess: A valid kazoo session object
    :param rootAcct: A string of the account ID whos descendant should have func applied
    :param includeSelf: If true, func also runs on the rootAcct as well as descendants
    :param func: A function that accepts a kazoo session, an account ID, an account name and kwargs as variables
    :param kwargs: Additional arguments to pass to the func
    '''
    # Get ALL account IDs
    path = '/accounts/' + rootAcct + '/descendants'
    allAccounts = pagedApiCallToEnd(KazSess, 'get', path)

    output = {}

    if includeSelf:
        output[rootAcct] = func(KazSess, rootAcct, "Root Account", kwargs)

    for acct in allAccounts:
        acctID = acct.get('id', '')
        if acctID == '':
            continue
        try:
            acctResult = func(KazSess, acctID, acct.get('name', ''), kwargs)
            print("Found %d results in %s for function %s. Result: %s" % (len(acctResult), acct.get('name', ''), func.__name__, json.dumps(acctResult)))
            output[acct.get('id', 'unkownAcctID')] = acctResult
        except:
            print("Error occoured while trying to run function %s for %s" % (func.__name__, acct.get('name', '')))
    return output


# Main entry point
def main():
    '''
    Weyoun, leader of the Vorta, a race that brutally imposed their masters' will across the galaxy

    As the name implies, this script applies order by force. Although this script's original intent was to
    normalize voicemail transcription settings across all descendants, it's grown to normalize other
    settings and also to just run generic functions across all sub-accounts like rebooting all phones in
    all sub-accounts.
    '''
    import os
    files = [f for f in os.listdir('.') if os.path.isfile(f)]
    for f in files:
        if f[-4:] == '.csv':
            print("Please move your old .csv files out of the working directory before continuing!")
            quit()
    functions = [x[1] for x in getmembers(weyounFunctions, isfunction)]
    functionToRun = pickAFunction(functions)
    rootAcctId = input('What account ID should we start from?: ')
    incluedRootAcct = getYesNo('Should the function also run on the selected start account itself?')
    KazSess = interactiveKazooAuth()

    results = runFunctionForAllDescendant(KazSess, rootAcctId, incluedRootAcct, functionToRun)
    print("Done! Results:")
    print(json.dumps(results))
    if getYesNo("Would you like to export these results to a CSV?"):
        listForCsvOutput = []
        csvHeaders = ["account_id"]
        filepath = "%s-%s-%s.csv" % (functionToRun.__name__, rootAcctId, datetime.now().strftime('%Y_%m_%d_%H_%M_%S'))
        for acctID, acctResults in results.items():
            if isinstance(acctResults, dict):
                listForCsvOutput.append({**{"account_id": acctID} , **acctResults})
                for resultKey, resultValue in acctResults.items():
                    if resultKey not in csvHeaders:
                        csvHeaders.append(resultKey)
            elif isinstance(acctResults, list):
                for objResult in acctResults:
                    listForCsvOutput.append({**{"account_id": acctID}, **objResult})
                    for resultKey, resultValue in objResult.items():
                        if resultKey not in csvHeaders:
                            csvHeaders.append(resultKey)
            else:
                print("Unknown output format. Sorry, CSV output may not be supported on this function")
        with open(filepath, 'w', newline='', encoding="utf-8") as output_file:
            dict_writer = csv.DictWriter(output_file, csvHeaders)
            dict_writer.writeheader()
            dict_writer.writerows(listForCsvOutput)


if __name__ == "__main__":
    print(main.__doc__)
    main()
