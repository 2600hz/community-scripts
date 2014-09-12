#!/usr/bin/env php
<?php
ini_set('memory_limit', '-1');
fwrite(STDOUT,  "COUCH DB RESTORER | version: 1.0.0" . PHP_EOL);
fwrite(STDOUT,  "(c) Copyright 2013, Anton Bondar <anton@zebooka.com> http://zebooka.com/soft/LICENSE/" . PHP_EOL . PHP_EOL);
fwrite(STDOUT,  "(c) Copyright 2014, Updated by Miralem Mehic <miralem@mehic.info>. Sponsored by CloudPBX Inc. <info@cloudpbx.ca>" . PHP_EOL . PHP_EOL);

$help = <<<HELP
   This tool restores provided JSON dump using _bulk_docs feature in CouchDB.

OPTIONS:
   -h                 Display this help message.
   -e                 Turn php error reporting ON.
   -H <HOSTNAME>      Hostname or IP of CouchDB server (default: 'localhost').
   -p <PORT>          Port of CouchDB server (default: 5984).
   -d <DATABASE>      Database to restore.
   -f <FILENAME>      JSON file to restore.
   -D                 Drop and create database, if needed (default: create db, only if it does not exist).
   -F                 Force restore on existing db with documents.
   -a                 Restore inline attachments (from base64 encoded format).

WARNING:
   Please note, that it is not a good idea to restore dump on existing database with documents.

USAGE:
   {$_SERVER['argv'][0]} -H localhost -p 5984 -d test -f dump.json
HELP;

$params = parseParameters($_SERVER['argv'], array('H', 'p', 'd', 'f'));
error_reporting(!empty($params['e']) ? -1 : 0);

if (isset($params['h'])) {
    fwrite(STDOUT,  $help . PHP_EOL);
    exit(1);
}
 
$host = isset($params['H']) ? trim($params['H']) : 'localhost';
$port = isset($params['p']) ? intval($params['p']) : 5984;
$database = isset($params['d']) ? strval($params['d']) : null;
$filename = isset($params['f']) ? strval($params['f']) : null;
$inlineAttachment = isset($params['a']) ? $params['a'] : false; 
$drop = isset($params['D']) ? strval($params['D']) : false;
$forceRestore = isset($params['F']) ? $params['F'] : false;

if ('' === $host || $port < 1 || 65535 < $port) {
    fwrite(STDOUT,  "ERROR: Please specify valid hostname and port (-H <HOSTNAME> and -p <PORT>)." . PHP_EOL);
    exit(1);
}

if (!isset($database) || '' === $database) {
    fwrite(STDOUT,  "ERROR: Please specify database name (-d <DATABASE>)." . PHP_EOL);
    exit(1);
}

if (!isset($filename) || !is_file($filename) || !is_readable($filename)) {
    fwrite(STDOUT,  "ERROR: Please specify JSON file to restore (-f <FILENAME>)." . PHP_EOL);
    exit(1);
}
//$filehandle = fopen($filename, 'rb');
//if (!$filehandle) {
//    fwrite(STDOUT,  "ERROR: Unable to open '{$filename}'." . PHP_EOL);
//    exit(2);
//}

// check db
$url = "http://{$host}:{$port}/{$database}/";
fwrite(STDOUT,  "Checking db '{$database}' at {$host}:{$port} ..." . PHP_EOL);
$curl = getCommonCurl($url);
$result = trim(curl_exec($curl));
$statusCode = curl_getinfo($curl, CURLINFO_HTTP_CODE);
curl_close($curl);
if (200 == $statusCode) {
    // database exists
    $exists = true;
    $db_info = json_decode($result, true);
    $docCount = (isset($db_info['doc_count']) ? $db_info['doc_count'] : 0);
    fwrite(STDOUT,  "Database '{$database}' has {$docCount} documents." . PHP_EOL);
} elseif (404 == $statusCode) {
    // database not found
    $exists = false;
    $docCount = 0;
} else {
    // unknown status
    fwrite(STDOUT,  "ERROR: Unsupported response when checking db '{$database}' status (http status code = {$statusCode}) " . $result . PHP_EOL);
    exit(2);
}
if ($drop && $exists) {
    // drop database
    fwrite(STDOUT,  "Deleting database '{$database}'..." . PHP_EOL);
    $curl = getCommonCurl($url);
    curl_setopt($curl, CURLOPT_CUSTOMREQUEST, 'DELETE');
    $result = trim(curl_exec($curl));
    $statusCode = curl_getinfo($curl, CURLINFO_HTTP_CODE);
    curl_close($curl);
    if (200 != $statusCode) {
        fwrite(STDOUT,  "ERROR: Unsupported response when deleting db '{$database}' (http status code = {$statusCode}) " . $result . PHP_EOL);
        exit(2);
    }
    $exists = false;
    $docCount = 0;
}
if ($docCount && !$forceRestore) {
    // has documents, but no force
    fwrite(STDOUT,  "ERROR: Database '{$database}' has {$docCount} documents. Refusing to restore without -F force flag." . PHP_EOL);
    exit(2);
}
if (!$exists) {
    // create db
    fwrite(STDOUT,  "Creating database '{$database}'..." . PHP_EOL);
    $curl = getCommonCurl($url);
    curl_setopt($curl, CURLOPT_CUSTOMREQUEST, 'PUT');
    $result = trim(curl_exec($curl));
    $statusCode = curl_getinfo($curl, CURLINFO_HTTP_CODE);
    curl_close($curl);
    if (201 != $statusCode) {
        fwrite(STDOUT,  "ERROR: Unsupported response when creating db '{$database}' (http status code = {$statusCode}) " . $result . PHP_EOL);
        exit(2);
    }
}

// post dump
$fileContent = file_get_contents($filename);
$decodedContent = json_decode($fileContent);
 
fwrite(STDOUT,  ">>>>>>>>>>>>>>>>> RESTORING STARTED <<<<<<<<<<<<<<<<<<<<<" . PHP_EOL); 
foreach($decodedContent->docs as $documentTemp){ 

    $documentTemp = (array)$documentTemp;  

    //so we need to fetch the latest revision of the document, because in order to upload a new version of document we MUST know latest rev ID
    $url = "http://{$host}:{$port}/{$database}/" . $documentTemp["_id"]; 
    $curl = getCommonCurl($url);
    $result = trim(curl_exec($curl));
    $statusCode = curl_getinfo($curl, CURLINFO_HTTP_CODE);

    if($statusCode == 200){
        $result = json_decode($result,true); 
        if(isset($result["_rev"]) && $result["_rev"])
            $documentTemp["_rev"] = $result["_rev"];
    }

    if(isset($documentTemp["_revisions"]))
        unset($documentTemp["_revisions"]);
    
    $url = "http://{$host}:{$port}/{$database}/" . $documentTemp["_id"];

    fwrite(STDOUT,  "Restoring '{$documentTemp['_id']}|rev:{$documentTemp['_rev']}' into db '{$database}' at {$host}:{$port}.." . PHP_EOL);

    //If we don't wont to upload attachments then we need to remove content from the file used for upload
    if(!$inlineAttachment && isset($documentTemp["_attachments"]) && $documentTemp["_attachments"]){
        unset($documentTemp["_attachments"]); 
        unset($documentTemp["unnamed"]);  
    }
 
    $curl = getCommonCurl($url); 
    curl_setopt($curl, CURLOPT_CUSTOMREQUEST, 'PUT'); /* or PUT */
    curl_setopt($curl, CURLOPT_POSTFIELDS, json_encode($documentTemp));
    curl_setopt($curl, CURLOPT_RETURNTRANSFER, true);
    curl_setopt($curl, CURLOPT_HTTPHEADER, array(
        'Content-type: application/json',
        'Accept: */*'
    ));

    // TODO: use next string when get ideas why it is not working and how to fix it.
    //curl_setopt($curl, CURLOPT_INFILE, $filehandle); // strange, but this does not work
    $result = trim(curl_exec($curl));
 
    //fclose($filehandle);
    $statusCode = curl_getinfo($curl, CURLINFO_HTTP_CODE);
    curl_close($curl);
    /*
    if ($statusCode < 200 || 299 < $statusCode) {
        fwrite(STDOUT,  "ERROR: Unable to post data to \"{$url}\" (http status code = {$statusCode}) " . $result . PHP_EOL); 
    }
    */
 
    $messages = json_decode($result, true); 

    $errors = 0;
    if (is_array($messages)) { 
        if (isset($messages['error'])) {
            $doc_id = isset($messages['id']) ? $messages['id'] : $documentTemp["_id"];
            $reason = isset($messages['reason']) ? $messages['reason'] : $messages['error'];
            fwrite(STDOUT,  "ERROR: [{$doc_id}] = {$reason}" . PHP_EOL);
            $errors++;
        } else if (isset($messages['ok'])) { 
            $doc_id = isset($messages['id']) ? $messages['id'] : '?';
            fwrite(STDOUT,  "SUCCESS: [{$doc_id}] restored!" . PHP_EOL);
        }
    } 
} 
fwrite(STDOUT,  ">>>>>>>>>>>>>>>>> RESTORING FINISHED! <<<<<<<<<<<<<<<<<<<<<" . PHP_EOL); 
exit(0);

////////////////////////////////////////////////////////////////////////////////

function getCommonCurl($url)
{
    $curl = curl_init();
    curl_setopt($curl, CURLOPT_RETURNTRANSFER, true);
    curl_setopt($curl, CURLOPT_TIMEOUT, 60);
    curl_setopt($curl, CURLOPT_USERAGENT, 'Mozilla/5.0 curl');
    curl_setopt($curl, CURLOPT_FOLLOWLOCATION, true);
    curl_setopt($curl, CURLOPT_MAXREDIRS, 3);
    curl_setopt($curl, CURLOPT_BINARYTRANSFER, 1);
    curl_setopt($curl, CURLOPT_URL, $url);
    return $curl;
}

////////////////////////////////////////////////////////////////////////////////

/**
 * Parse incoming parameters like from $_SERVER['argv'] array.
 * @author Anton Bondar <anton@zebooka.com>
 * @param array $params Incoming parameters
 * @param array $reqs Parameters with required value
 * @param array $multiple Parameters that may come multiple times
 * @return array
 */
function parseParameters(array $params, array $reqs = array(), array $multiple = array())
{
    $result = array();
    reset($params);
    while (list(, $p) = each($params)) {
        if ($p[0] == '-' && $p != '-' && $p != '--') {
            $pname = substr($p, 1);
            $value = true;
            if ($pname[0] == '-') {
                // long-opt (--<param>)
                $pname = substr($pname, 1);
                if (strpos($p, '=') !== false) {
                    // value specified inline (--<param>=<value>)
                    list($pname, $value) = explode('=', substr($p, 2), 2);
                }
            }
            $nextparam = current($params);
            if ($value === true && in_array($pname, $reqs)) {
                if ($nextparam !== false) {
                    list(, $value) = each($params);
                } else {
                    $value = false;
                } // required value for option not found
            }
            if (in_array($pname, $multiple) && isset($result[$pname])) {
                if (!is_array($result[$pname])) {
                    $result[$pname] = array($result[$pname]);
                }
                $result[$pname][] = $value;
            } else {
                $result[$pname] = $value;
            }
        } else {
            if ($p == '--') {
                // all next params are not parsed
                while (list(, $p) = each($params)) {
                    $result[] = $p;
                }
            } else {
                // param doesn't belong to any option
                $result[] = $p;
            }
        }
    }
    return $result;
}
