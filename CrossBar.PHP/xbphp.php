#!/usr/bin/php
<?php

//include('../include/CrossBar.php');

$HOME = getenv("HOME");
include("$HOME/src/work/CrossBar.PHP/CrossBar.php");
$XBOPTS = parse_ini_file("$HOME/.xbopts.conf");

$XBAR = new CrossBar($XBOPTS);


$TYPE = "GET";
$REALM = "";
$OBJECT = "";
$OBJECT_ID = "";
$ACCOUNT_ID = "";
$LIST = "";

foreach( $argv as $key => $val ) {

	if( $val == "-t" ) $TYPE = $argv[$key+1];
	if( $val == "-l" ) $LIST = "mwhhahahah";
	if( $val == "-r" ) $REALM = $argv[$key+1];
	if( $val == "-o" ) $OBJECT = $argv[$key+1];
	if( $val == "-oi" ) $OBJECT_ID = $argv[$key+1];
	if( $val == "-ua" ) $ACCOUNT_ID = $argv[$key+1];

}





if( $XBAR->is_authenticated ) {

	printf("Connected {$XBOPTS['username']}@{$XBOPTS['host']}:{$XBOPTS['port']}\n\n");

	$accounts = $XBAR->get_accounts();
	print_r($accounts);

	if( strlen($REALM) ) {
		$XBAR->use_account($accounts[$REALM]);
	}

	if( strlen($ACCOUNT_ID) ) {
		$XBAR->use_account($accounts[$REALM]);
	}

	//$response = $XBAR->send($TYPE,"/v1/accounts/48d4331788d2d064f13acf8f7ed19fb6/accounts");
	//print_r($response);

	if( strlen($LIST) ) foreach( $accounts as $key => $realm ) echo " * ".$key."\n";



	if( strlen($OBJECT) ) {
		echo $TYPE." /v1/accounts/{$XBAR->use_account_id}/$OBJECT/$OBJECT_ID";
		$response = $XBAR->send($TYPE,"/v1/accounts/{$XBAR->use_account_id}/$OBJECT/$OBJECT_ID");
		print_r($response);
	}



} else {
	printf("Connection failure {$XBOPTS['username']}@{$XBOPTS['host']}:{$XBOPTS['port']}\n");
}











?>
