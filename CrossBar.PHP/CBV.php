<?php 


require_once("CrossBar.php");

$bldred=chr(0x1B).'[1;31m'; $bldgrn=chr(0x1B).'[1;32m'; $bldylw=chr(0x1B).'[1;33m'; $bldblu=chr(0x1B).'[1;34m'; $bldpur=chr(0x1B).'[1;35m'; $bldcyn=chr(0x1B).'[1;36m'; $bldwht=chr(0x1B).'[1;37m'; $txtrst=chr(0x1B).'[0m'; 


$XBOPTS['host'] = "127.0.0.1";
$XBOPTS['port'] = 8000;
$XBOPTS['usermd5'] = md5("user:password");
$XBOPTS['realm'] = "";
//$XBOPTS['profile'] = true;

global $XBAR;
$XBAR = new CrossBar($XBOPTS); // See if we can make a connection
$USER_ID = '';
$ACCOUNT_ID = '';
$DEVICE_ID = '';

printf("{$bldblu}TEST: Authentication{$txtrst}\n");
if( $XBAR->is_authenticated() ) {

	printf("{$bldgrn}PASS: Authentication Success.{$txtrst}\n\n");

	$TEMPLATE['name'] = 'supermegamega';
	$TEMPLATE['realm'] = 'supermega.example.com';
	$TEMPLATE['owner_id'] = $XBAR->auth_account_id; //Which ever user authed we will use that account is attached to  


	$realm_id = $XBAR->get_account_id_by_realm( $TEMPLATE['realm'] );

	if( strlen($realm_id) ) {
		printf("{$bldylw}WARN: {$bldred}Existing Account SKIPPING CREATION!{$txtrst}\n"); 
	} else {
		printf("{$bldblu}TEST: Create Account{$txtrst}\n");
		$resp = $XBAR->put_account( $TEMPLATE );
		if( $resp['status'] == 'success' ) {
			printf("{$bldgrn}PASS: Account Creation Successful.{$txtrst}\n");
			$realm_id = $XBAR->get_account_id_by_realm( $TEMPLATE['realm'] );
		} else {
			printf("{$bldred}FAIL: Account Creation Failure.  ( {$bldpur} {$resp['message']} {$bldwht} ) {$txtrst}\n\n");
			//Without an account it's difficult to test the REST of the api. bwahaha I'm so punneh \(O.O)/
			exit(-1);
		}	
	}

	$acct = $XBAR->get_account( $realm_id );

	//Sanity check
	if( $TEMPLATE['realm'] != $acct['realm'] ) {
		printf("{$bldred}FATAL: {$bldwht}Response failed sanity check {$bldwht}( {$bldpur}possibly developers sanity too {$bldwht}){$txtrst}\n");	
		exit(-1);
	}

	$TEMPLATE = array();	
	$TEMPLATE['name'] = $acct['name'];
	$TEMPLATE['realm'] = $acct['realm'];
	$TEMPLATE['owner_id'] = $acct['owner_id'];
	$TEMPLATE['timezone'] = "America/Vancouver";

	printf("{$bldblu}TEST: Account Update{$txtrst}\n");
	$resp = $XBAR->post_account($TEMPLATE,$realm_id);

	if( $resp['status'] == 'success' ) {
		printf("{$bldgrn}PASS: Account Update Returned Success{$txtrst}\n\n");	
		$acct = $XBAR->get_account( $realm_id );

		/*
			May need to change this test to make sure 
			it's not the original/default value America/LosAngles I think
		*/

		if( $acct['timezone'] != "America/Vancouver" ) {
			printf("{$bldred}FAIL: Response failed sanity check {$bldwht}({$bldpur}possibly developers sanity too {$bldwht}){$txtrst}\n\n");	
		}  


	} else {
		printf("{$bldred}FAIL: Account Update Failed{$txtrst}\n\n");	
		

	}



	printf("{$bldblu}TEST: Delete Account{$txtrst}\n");

	$resp = $XBAR->del_account($realm_id);

	if( $resp['status'] != 'success'  ) {
		$acct = $XBAR->get_account( $realm_id );
		if( isset($acct['id']) ) {
			printf("{$bldred}FAIL: Account Delete Failed.{$txtrst}\n\n");	
		} else {
			printf("{$bldred}FAIL: Account was deleted but returned with an odd response.{$txtrst}\n\n");
		}
	} else {
		printf("{$bldgrn}PASS: Account Delete Success{$txtrst}\n\n");	
	}
		
		
	printf("{$bldblu}TEST: Recreating Test Account{$txtrst}\n");
	$resp = $XBAR->put_account( $TEMPLATE );
	if( $resp['status'] != 'success' ) {
		printf("{$bldred}FATAL:{$bldwht}Could not re create the test account, aborting test{$txtrst}\n");
		//print_r($resp);
		exit(-1);
	}

	if( isset($resp['data']['id']) ) {
		$ACCOUNT_ID = $resp['data']['id'];
		printf("{$bldylw}WARN: Using INSERT ACCOUNT ID:{$bldred}$ACCOUNT_ID.{$txtrst}\n\n");
		
	}

	$USER_TEMPLATE = array();
	$USER_TEMPLATE['username'] = "ultrahamster".date('his');
	$USER_TEMPLATE['first_name'] = "Ultra";
	$USER_TEMPLATE['last_name'] = "Hamster";
	$USER_TEMPLATE['priv_level'] = 'user';
	$USER_TEMPLATE['email'] = "ultra@hamster.com";
	$USER_TEMPLATE['verified'] = false;
	$USER_TEMPLATE['enabled'] = true;
	$USER_TEMPLATE['timezone'] = "America/Florida"; //lmfao I'm in miami *****!
	$USER_TEMPLATE['caller_id']['internal'] = array( 'number' => "1234", 'name' => 'Internal' );
	$USER_TEMPLATE['caller_id']['external'] = array( 'number' => "6041231234", 'name' => 'External' );
	$USER_TEMPLATE['call_forward'] = array( 
		'enabled' => false, 
		'number' => '',
		'require_keypress' => false,
		'keep_caller_id' => true,
		'substitute' => false,
		'direct_calls_only' => false 
	);
	
	/*
	$users = $XBAR->get_users($ACCOUNT_ID);
	print_r($users);
	exit(0);
	*/



	$USER_ID = '';

	printf("{$bldblu}TEST: Creating User{$txtrst}\n");
	$user = $XBAR->get_user_by_name( $USER_TEMPLATE['username'], $ACCOUNT_ID );
	if( $user ) {
		printf("{$bldylw}Warning: {$bldred}Existing User SKIPPING CREATION!{$txtrst}\n"); 
	} else {
		$resp = $XBAR->put_user($USER_TEMPLATE, $ACCOUNT_ID );
	
		if( $resp['status'] != 'success' ) {
			printf("{$bldred}FATAL: {$bldwht}Create User Failed, abort test. {$bldwht}({$bldpur} {$resp['message']}{$bldwht} ) {$txtrst}\n");
			exit(-1);
		} else {

			printf("{$bldgrn}PASS: Create User Success.{$txtrst}\n\n");
			/*
				Hack for v1.50 since user listing (possibly retrieval too) is broken.
				This only works occasionaly since couch only responds with the id 
				if it returns within a certain time limit I think ( delayed_commits ) setting is the issue
				for now it's random when data is returned and the list users interface throws a 500 error
				best bet to hope the data comes back
			*/
	
	
			if( isset($resp['data']) ) {
				$USER_ID = $resp['data']['id'];
				printf("{$bldylw}WARN: Using INSERT USER ID:{$bldred}$USER_ID{$txtrst}\n\n"); 

			}
		}
	}

	printf("{$bldblu}TEST: Get User by username.{$txtrst}\n");
	$user = $XBAR->get_user_by_name( $USER_TEMPLATE['username'], $ACCOUNT_ID );

	if( $user['username'] == $USER_TEMPLATE['username'] ) {
		printf("{$bldgrn}PASS: User found, User listing and filtering is successful.{$txtrst}\n\n");
		$USER_ID = $user['id'];
	} else {
		printf("{$bldred}FAIL: User listing OR filtering is broken.{$txtrst}\n\n");
	}
	
	if( strlen( $USER_ID ) == 0 ) {

		printf("{$bldred}FATAL: {$bldwht}Could not get a User ID back from (insert response/user listing).{$txtrst}\n");
		exit(-1);

	}
	
	printf("{$bldblu}TEST: Get User by ID:{$bldred}$USER_ID.{$txtrst}\n");
	$user = $XBAR->get_user( $USER_ID, $ACCOUNT_ID );	

	print_r($user);


	if( $user['status'] == 'success' ) {
		printf("{$bldgrn}PASS: User found, finding a User by ID is possible.{$txtrst}\n\n");
	} else {
		printf("{$bldred}FAIL: Failed to retrieve User by ID.{$txtrst}\n\n");
	}

	printf("{$bldblu}TEST: Delete User ID:{$bldred}$USER_ID.{$txtrst}\n");

	$resp = $XBAR->del_user($USER_ID, $ACCOUNT_ID);	

	if( $resp['status'] == 'success' ) {
		printf("{$bldgrn}PASS: Delete User.{$txtrst}\n\n");
	} else {
		printf("{$bldred}FAIL: Delete User.{$txtrst}\n\n");
	}

	

	$DEVICE_TEMPLATE = array();
	$DEVICE_TEMPLATE['name'] = $USER_TEMPLATE['usernmae']." Device";
	$DEVICE_TEMPLATE['device_type'] = 'sip_device';
	$DEVICE_TEMPLATE['enabled'] = true;


	/* TODO FIGURE OUT WHAT THIS IS */
	$DEVICE_TEMPLATE['provision'] = '';
	$DEVICE_TEMPLATE['suppress_unregister_notifications'] = false;

	$DEVICE_TEMPLATE['call_forward'] = array( 
		'enabled' => false, 
		'number' => '',
		'require_keypress' => false,
		'keep_caller_id' => true,
		'substitute' => false,
		'ignore_early_media' => true, 
		'direct_calls_only' => false 
	);
	



	$DEVICE_TEMPLATE['caller_id']['internal'] = array( 'number' => '1234', 'name' => 'test person' );
	$DEVICE_TEMPLATE['caller_id']['external'] = array( 'number' => '1234', 'name' => 'test person' );


	$DEVICE_TEMPLATE['media'] = array(
		'fax' => array( 'option' => 'auto' ),
		'bypass_media' => 'false'
	);

	$DEVICE_TEMPLATE['media']['audio']['codecs'][] = "PCMA";
	$DEVICE_TEMPLATE['media']['audio']['codecs'][] = "PCMU";

	$DEVICE_TEMPLATE['sip'] = array(
		//'custom_sip_headers' => '',
		'method' => 'password', //unsure what this is
		'invite_format' => 'username',
		'username' => $USER_TEMPLATE['username'],
		'password' => substr(sha1($USER_TEMPLATE['username']),0,31),
		'expire_seconds' => 300,
		'registration_expiration' => 360 
	);



	printf("{$bldblu}TEST: Create Device.{$txtrst}\n");

	$DEVICE_TEMPLATE['owner_id'] = $USER_ID;
	$resp = $XBAR->put_device( $DEVICE_TEMPLATE, $ACCOUNT_ID );

	if( $resp['status'] == 'success' ) {

		printf("{$bldgrn}PASS: Create Device.{$txtrst}\n\n");
		$DEVICE_ID = $resp['data']['id'];	

		printf("{$bldblu}TEST: Get Device by ID:{$bldred}$DEVICE_ID.{$txtrst}\n");

		$device = $XBAR->get_device($DEVICE_ID, $ACCOUNT_ID);
		if( isset($device['id']) ) {
			printf("{$bldgrn}PASS: Get Device.{$txtrst}\n\n");
		} else {
			printf("{$bldred}FAIL: Get Device.{$txtrst}\n\n");
		}


		printf("{$bldblu}TEST: Delete Device{$txtrst}\n");
		$resp = $XBAR->del_device($DEVICE_ID, $ACCOUNT_ID);

		if( $resp['status'] == "success" ) {
			printf("{$bldgrn}PASS: Delete Device.{$txtrst}\n\n");
		} else {
			printf("{$bldred}FAIL: Delete Device.{$txtrst}\n\n");
		}

	




	} else {
		printf("{$bldred}FAIL: Create Device.{$txtrst}\n\n");
	}






	$VMB_TEMPLATE = array();
	//$VMB_TEMPLATE['name'] = $_REQUEST['first_name']." ".$_REQUEST['last_name']." ( {$_REQUEST['extension_number']} )";
	$VMB_TEMPLATE['name'] = $USER_TEMPLATE['usernmae']." VMB";
	$VMB_TEMPLATE['mailbox'] = '1234';
	$VMB_TEMPLATE['pin'] = '1234';
	$VMB_TEMPLATE['timezone'] = 'America/Los_Angles';
	$VMB_TEMPLATE['check_if_owner'] = true;
	$VMB_TEMPLATE['require_pin'] = true;
	$VMB_TEMPLATE['skip_greeting'] = false;
	$VMB_TEMPLATE['skip_instructions'] = false;
	$VMB_TEMPLATE['is_setup'] = false;
	$VMB_TEMPLATE['owner_id'] = $USER_ID;



	printf("{$bldblu}TEST: Create VMB.{$txtrst}\n");


	$resp = $XBAR->put_vmbox( $VMB_TEMPLATE, $ACCOUNT_ID );	

	if( $resp['status'] == 'success' ) {

		printf("{$bldgrn}PASS: Create VMB.{$txtrst}\n\n");
		$VMB_ID = $resp['data']['id'];	

		printf("{$bldblu}TEST: Get VMB by ID:{$bldred}$VMB_ID.{$txtrst}\n");

		$vmb = $XBAR->get_vmbox($VMB_ID, $ACCOUNT_ID);
		if( isset($vmb['id']) ) {
			printf("{$bldgrn}PASS: Get VMB.{$txtrst}\n\n");
		} else {
			printf("{$bldred}FAIL: Get VMB.{$txtrst}\n\n");
		}


		printf("{$bldblu}TEST: Delete VMB{$txtrst}\n");
		$resp = $XBAR->del_vmbox( $VMB_ID, $ACCOUNT_ID);

		if( $resp['status'] == "success" ) {
			printf("{$bldgrn}PASS: Delete VMB.{$txtrst}\n\n");
		} else {
			printf("{$bldred}FAIL: Delete VMB.{$txtrst}\n\n");
		}

	} else {
		printf("{$bldred}FAIL: Create Device.{$txtrst}\n\n");
	}





	/*
	$VMB_TEMPLATE['owner_id'] = $USER_ID;
	$VMB_RESPONSE = $XBAR->put_vmbox($VMB_TEMPLATE, $OWNER_ID);

	sleep(2);
	$DEVICE_CREATED = $XBAR->get_device_by_name($NAME, $OWNER_ID);
	$VMB_CREATED = $XBAR->get_vmbox_by_name($NAME, $OWNER_ID);
	*/
	



	/*
		WILL NEED TO RE CREATE USER HERE

	*/




	/*
	$user = $XBAR->get_user_by_name( $USER_TEMPLATE['username'], $ACCOUNT_ID );
	print_r($user);
	echo "USERID:$USER_ID\n";	
	*/
	

	//print_r($resp);


	/*
	$accounts = $XBAR->get_children();
	$users = $XBAR->get_users();
	$resources = $XBAR->get_resources();
	$devices = $XBAR->get_devices();
	*/


	//Create sub account
	//Update sub account
	//Delete sub account

	//Intermediate step. re create teh sub account

	//Create user (attatched to new subaccount )
	//Retrieve the user
	//Update user ( on new sub ) 
	//Delete the user



	//Intermediate step. re create the user.

	//Create device attatched to user
	//Retrieve device 
	//Update device attatched to user
	
	//Delete device

	//Intermediate step. re create the device.
	
	//Create vmb attached to user
	//Retrieve vmb 
	//Update vmb to user 
	//Delete vmb 

	//Intermediate step. re create the vmb.


	//Callflow?



} else {


	printf("{$bldred}FAIL: Authentication Failure.{$txtrst}\n\n");


}


?>
