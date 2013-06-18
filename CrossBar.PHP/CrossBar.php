<?php
/**
	@mainpage CrossBar.PHP
	@brief PHP interface with Kazoo's JSON API. This will allow you to send and recieve data from kazoo.
	\n
	\n
	@b Classes:
	- @ref CrossBar

	@b Objects @b Supported:
	- Accounts
	- Users


        @author Chris Megalos
	@date 2012-09-21
	@note Class methods may change at anytime currently. 
	
	@class CrossBar
	@brief Check
**/
class CrossBar {

	var $force_no_decode = false;  /**< force_no_decode disables json_decode from the send method. */
	var $debug = false;	/**< debug enables debugging if true. */
	var $is_authenticated = false; /**< is_authenticated boolean flag that is changed upon authentication. */

	var $usermd5;	/**< usermd5 md5 of "user:password" */
	var $host;	/**< ipv4 address or hostname */
        var $logfile = "/var/log/xbar.log"; /**< Log file */
	

	/** 

	@brief Constructor, if you pass xauth it will use that as the auth token rather than attempting to get a new one.

	@param array $options 
	
	@return Newly created object.
	

	@code
	$XBOPTS['usermd5'] = md5("user:password");
        $XBOPTS['realm'] = 'example.voxter.ca';
        $XBOPTS['host'] = "127.0.0.1";
        $XBOPTS['port'] = 8000;
	$XBOPTS['debug'] = false;
	$XBOPTS['force_no_decode'] = false; 
	$XBOPTS['is_authenticated'] = false; 
        $XBAR = new CrossBar($XBOPTS);
	@endcode


	
	**/
	function __construct( $options ) {

		$this->force_no_decode = false; 
		$this->debug = false;	
		$this->is_authenticated = false;	
		$this->socket_stream = null;
		$this->logfile;

		$login_type = "";


		foreach($options as $key => $value) $this->$key = $value; 
		$auth = array();

		if( !isset($this->xauth) && !isset($this->auth_account_id) ) {
			if( isset($this->usermd5) ) {
				$login_type = "md5";
				//$auth = $this->send("PUT","/v1/user_auth",'{"data":{"realm": "'.$this->realm.'", "credentials": "'.$this->usermd5.'" }}');
				$auth = $this->send("PUT","/v1/user_auth",'{"data":{"account_name": "'.$this->realm.'", "credentials": "'.$this->usermd5.'" }}');
			} else {
				$login_type = "api_key";
				$auth = $this->send("PUT","/v1/api_auth",'{"data":{"api_key": "'.$this->api_key.'" }}');
			}
			$this->auth = $auth;
			if( $auth['status'] == 'success' ) {
				$this->is_authenticated = true;	
				$this->xauth = $auth['auth_token'];
				$this->auth_account_id = $auth['data']['account_id'];
				$this->use_account_id = $auth['data']['account_id'];
				$this->log("acct id: ".$auth['data']['account_id']);
				$this->log("auth token: ".$auth['auth_token']);
				
			} else {
				
				$this->log("$login_type: failure");
			}
		} 

	}


	function __destruct() {
		if( $this->socket_stream != null ) fclose($this->socket_stream); 
	}


	/** 
	@brief Writes a log line to /tmp/xbar.log (default) or if $debug is set then it's sent to stdout
	@param $logthis is the string to log
	@return null
	**/
	function log( $logthis ) {
		if( $this->debug ) {
			echo $logthis."\n";
		} else {
			file_put_contents($this->logfile,date("Y-m-d H:i:s")." - {$_SERVER['REMOTE_ADDR']} - ".$logthis."\n",FILE_APPEND);
			//syslog(LOG_NOTICE,"CrossBar.PHP: ".$logthis);
		}
	}


	/**
	@brief accessor for $is_authenticated
	@return bool (true|false)
	**/
	function is_authenticated() { return($this->is_authenticated); }


	/**
	@brief attempts to retrieve the version
	@return string json data currently haven't had chance to test this feature
	@todo test this feature
	**/
	function get_version() {

		$tmp = $this->force_no_decode;
		$this->force_no_decode = true;
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("GET","/v1/about" );
		$this->force_no_decode = $tmp;
		return($response);
	}


	/**
	@brief retrieves the connectivity 
	@return string json data 
	**/
	function get_connectivity( $cid = null, $account_id = null ) {
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("GET","/v1/accounts/{$account_id}/connectivity/$cid");
		return($response);
	}


	function get_pbxs( $account_id = null ) { 
		$response = $this->get_connectivity(null, $account_id); 
		return($response['data']); 
	}

	function get_pbx( $cid, $account_id = null ) { 
		$response = $this->get_connectivity($cid, $account_id); 
		return($response['data']); 
	}


	/**
	@brief deletes the connectivity 
	@return string json data 
	**/
        function del_connectivity( $cid,  $account_id = null) {
                if( $account_id == null ) $account_id = $this->use_account_id;
                $response = $this->send("DELETE","/v1/accounts/$account_id/connectivity/$cid");
                return($response);
        }
	function del_pbx( $cid, $account_id = null ) { $response = $this->del_connectivity($cid,$account_id); return($response['data']); }


	/**
	@brief updates the connectivity
	@return string json data 
	**/
	function put_connectivity($data,  $account_id = null ) {
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("PUT","/v1/accounts/{$account_id}/connectivity/{$cid}", json_encode(array('data'=>$data)));
		return($response);
	}

	function put_pbx($data,  $account_id = null ) { $response = $this->put_connectivity($data,$account_id); return($response['data']); }
	

	/**
	@brief create new connectivity (attach a pbx)
	@return string json data 
	**/
	function post_connectivity( $data, $cid, $account_id = null ) {
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("POST","/v1/accounts/{$account_id}/connectivity/{$cid}", json_encode(array('data'=>$data)));
		return($response);
	}
	function post_pbx($data,  $account_id = null ) { $response = $this->post_connectivity($data,$account_id); return($response['data']); }


	function get_realm_numbers( $realm_id = null ) {

		$child_nums = array();
		$current_nums = array();

		if( $realm_id == null ) $realm_id = $this->auth_account_id;

		$check_response = $this->send("GET","/v1/accounts/{$realm_id}/phone_numbers/");
		foreach( $check_response['data'] as $number => $data ) if( $number != 'id' ) $current_nums[$number] = $realm_id;
		$children = $this->get_children($realm_id);
		foreach( $children as $child ) $current_nums = array_merge( $current_nums, $this->get_realm_numbers($child['id']) );

		return($current_nums);


	}


	function get_account_id_by_did( $did, $realm_id = null ) {

		$child_nums = array();
		$current_nums = array();

		static $account_id = null;


		if( $realm_id == null ) $realm_id = $this->auth_account_id;

		$check_response = $this->send("GET","/v1/accounts/{$realm_id}/phone_numbers/");
		foreach( $check_response['data'] as $number => $data ) {
			if( $number == $did ) {
				$account_id = $realm_id;
				return($realm_id);
			}
		}

		$children = $this->get_children($realm_id);

		foreach( $children as $child ) { 
			$account_id = $this->get_account_id_by_did( $did, $child['id'] );
			if( $account_id != null ) return($account_id);
		}

		return( $account_id );


	}


	function create_webhook( $name, $url, $bind_event = 'authz', $retries = 2, $account_id = null ) {

		if( $account_id == null ) $account_id = $this->use_account_id;

		$webhook = array();
		$webhook['event_name'] = $name;
		$webhook['bind_event'] = $bind_event;
		$webhook['callback_uri'] = $url;
		//$webhook['callback_method'] = "POST";
		$webhook['retries'] = 2;

		

		$response = $this->send("PUT","/v1/accounts/{$account_id}/webhooks", json_encode(array('data'=>$webhook)) );
		return($response);

	}


	function delete_webhook( $hook_id, $account_id = null ) {

		if( $account_id == null ) $account_id = $this->use_account_id;

		$response = $this->send("DELETE","/v1/accounts/{$account_id}/webhooks/{$hook_id}");
		return($response);

	}


	function use_account( $account_id, $cache = false ) { 

		$this->use_account_id = $account_id; 

		//load all data?
		if( $cache ) {
			$this->cache = true;
		} else {
			$this->cache = false;
		}

	}



	function get_object_x( $types, $filters = array(), $account_id = null ) {

		if( $account_id == null ) $account_id = $this->use_account_id;
		$filter = '';

		if( count($types) ) foreach( $types as $t => $id ) $filter .= "$t/$id/";

		if( count($filters) ) {
			foreach( $filters as $key => $val ) $filter .= "filter_$key=$val&";
			if( strlen($filter) ) $filter = '?'.substr($filter,0,-1);
		} 

		//$this->log("GET /v1/accounts/{$account_id}/$type$filter");
		$response = $this->send_object("GET","/v1/accounts/{$account_id}/$filter");

		//return($response['data']);
		return($response->data);



	}






	function get_object( $type, $id = null, $filters = array(), $account_id = null ) {

		if( $account_id == null ) $account_id = $this->use_account_id;

		$filter = '';

		if( count($filters) ) {
			foreach( $filters as $key => $val ) $filter .= "filter_$key=$val&";
			if( strlen($filter) ) $filter = '?'.substr($filter,0,-1);
		} else if( strlen($id) ) {
			$filter = "/$id";
		}

		//$this->log("GET /v1/accounts/{$account_id}/$type$filter");
		$response = $this->send_object("GET","/v1/accounts/{$account_id}/$type$filter");

		//return($response['data']);
		return($response->data);



	}



	function get( $type, $id = null, $filters = array(), $account_id = null ) {
		//$this->log(__CLASS__."->".__METHOD__." is DEPRECATED, please replace with ".__CLASS__."->get_object");

		if( $account_id == null ) $account_id = $this->use_account_id;

		$filter = '';

		if( count($filters) ) {
			foreach( $filters as $key => $val ) $filter .= "filter_$key=$val&";
			if( strlen($filter) ) $filter = '?'.substr($filter,0,-1);
		} else if( strlen($id) ) {
			$filter = "/$id";
		}

		//$this->log("GET /v1/accounts/{$account_id}/$type$filter");
		$response = $this->send("GET","/v1/accounts/{$account_id}/$type$filter");

		return($response['data']);



	}


	function post( $type, $id, $data, $account_id = null ) {
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("POST","/v1/accounts/{$account_id}/$type/$id", json_encode(array('data'=>$data)));
		return($response);
	}


	function put( $type, $data, $account_id = null ) {
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("PUT","/v1/accounts/{$account_id}/$type/", json_encode(array('data'=>$data)));
		return($response);
	}


	function del( $type, $id, $account_id = null ) {
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("DELETE","/v1/accounts/{$account_id}/$type/$id");
		return($response);
	}


	//Convenience functions
	function get_accounts($account_id = null) { 
		if( $account_id == null ) $account_id = $this->use_account_id;
		$children = $this->get_children($account_id);
		$realms = array();
		foreach( $children as $child ) {
			//$realms[$child['id']] = $child['realm'];
			$realms[$child['realm']] = $child['id'];
		}
                $crealm = $this->get_account($this->auth_account_id);
                $realms[$crealm['realm']] = $this->auth_account_id;

		//$realms[$this->realm] = $this->auth_account_id;
		return($realms);
	}


	function get_callflow_id_map($account_id = null ) {
		if( $account_id == null ) $account_id = $this->use_account_id;
		$test = $this->get_callflows($account_id);
		$nums = array();
		foreach( $test as $key => $data ) {
			//$t2 = $XBAR->get_callflow($data['id']);
			foreach( $data['numbers'] as $num ) {
				$nums[$num] = $data['id'];	
			} 
		}

		return($nums);
	}


	function get_callflows_by( $id, $type = 'device' ) {

		$aout = array();
		$cfs = $this->get_callflows();

		foreach( $cfs as $cf ) { 
			$xcf = $this->get_callflow($cf['id']);
			//print_r($xcf['metadata']);
			print_r($xcf);
			//print_r($cf['numbers']); 
			foreach( $xcf['metadata'] as $key => $data ) {
				if( $data['pvt_type'] == $type ) {
					$aout[$xcf['id']] = $cf['numbers'];
				}
			}
		}

		return($aout);

	}


	function get_device_by_username( $username, $account_id = null ) {
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->get('devices',null,array('username'=>$username),$account_id);	
		return($response[0]);
	}



	function get_device_by_name( $name, $account_id = null ) {
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->get('devices',null,array('name'=>$name),$account_id);	
		return($response[0]);
	}

	function get_menu_by_name( $name, $account_id = null ) {
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->get('menus',null,array('name'=>$name),$account_id);	
		return($response[0]);
	}

	function get_conference_by_name( $name, $account_id = null ) {
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->get('conferences',null,array('name'=>$name),$account_id);	
		return($response[0]);
	}

	function get_directory_by_name( $name, $account_id = null ) {
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->get('directories',null,array('name'=>$name),$account_id);	
		return($response[0]);
	}


	function get_devices_by_owner( $owner_id, $account_id = null ) {
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->get('devices',null,array('owner_id'=>$owner_id),$account_id);	
		return($response);
	}


	function get_device_by_owner( $owner_id, $account_id = null ) {
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->get('devices',null,array('owner_id'=>$owner_id),$account_id);	
		return($response[0]);
	}


	function get_vmbox_by_ext( $extension,  $account_id = null ) { 
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->get('vmboxes',null,array('mailbox'=>$extension),$account_id);	
		return($response[0]);
	}


	function get_vmbox_by_number( $number,  $account_id = null ) { 
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->get('vmboxes',null,array('mailbox'=>$number),$account_id);	
		return($response[0]);
	}
	

	function get_vmbox_by_owner( $owner_id,  $account_id = null ) { 
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->get('vmboxes',null,array('owner_id'=>$owner_id),$account_id);	
		foreach( $response as $key => $data ) $response[$key] = array_merge($data, $this->get_vmbox($data['id'],$account_id));	
		return($response);
	}


	function get_vmbox_by_name( $name,  $account_id = null ) { 
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->get('vmboxes',null,array('name'=>$name),$account_id);	
		return($response[0]);
	}

	
	function login_vmbox( $mailbox, $pin,   $account_id = null ) { 
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->get('vmboxes',null,array('mailbox'=>$mailbox,'pin'=>$pin),$account_id);	
		return($response[0]);
	}
	

	function get_account_id_by_realm( $realm, $account_id = null ) { 
		$realms = $this->get_accounts($account_id);
		return( $realms[$realm] );
	}


	function get_user_by_name( $username, $account_id = null ) {
		$response = $this->get('users',null,array('username'=>$username),$account_id);	
		return($response[0]);
	}


	function get_user_id( $username ) {
		$user = $this->get_user($username);
		if( isset($user['id']) ) return($user['id']); return false;
	}


	function get_children($account_id = null) { 
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("GET","/v1/accounts/{$account_id}/children");
		return($response['data']);
	}


	function get_siblings() { 
		$response = $this->send("GET","/v1/accounts/{$this->use_account_id}/siblings");
		return($response['data']);
	}


	function get_descendants() { 
		$response = $this->send("GET","/v1/accounts/{$this->use_account_id}/descendants");
		return($response['data']);
	}
	

	function set_parent( $account_id, $new_parent_id ) {
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("POST","/v1/accounts/{$account_id}/parent","parent=$new_parent_id");
		return($response['data']);
	}
	

	function get_all_info( $account_id = null ) {

		if( $account_id == null ) $account_id = $this->use_account_id;
		$temp_users = $this->get_users($account_id);
		$temp_devices = $this->get_devices($account_id);
		$devices_status = $this->get_devices_status($account_id);
		$vmboxes = $this->get_vmboxes($account_id);
		ob_start();
		print_r($vmboxes);
		$this->log(ob_get_clean());
	
		$users = array();

		foreach( $temp_users as $user ) { $users[$user['id']] = $user; }
		foreach( $temp_devices as $device ) {
			if( isset($devices_status[$device['id']]) ) $device['online'] = true;
			$device['user'] = $users[$device['owner_id']];
		}
	}


	function get_devices_status( $account_id = null ) { 
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("GET","/v1/accounts/{$account_id}/devices/status");
		$devices = array();
		foreach( $response['data'] as $data ) $devices[$data['device_id']] = $data['registered'];
		return($devices);
	}


	function get_devices( $account_id = null ) { 
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("GET","/v1/accounts/{$account_id}/devices");
		return($response['data']);
	}


	function get_vmboxes( $account_id = null ) { 
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("GET","/v1/accounts/{$account_id}/vmboxes");
		foreach($response['data'] as $key => $val ) $response['data'][$key] = array_merge( $val, $this->get_vmbox($val['id'],$account_id) );
		return($response['data']);
	}


	function get_messages( $box_id, $account_id = null ) { 
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("GET","/v1/accounts/{$account_id}/vmboxes/$box_id/messages");
		return($response['data']);
	}


	function get_message( $message_id, $box_id, $account_id = null ) { 
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("GET","/v1/accounts/{$account_id}/vmboxes/$box_id/messages/$message_id");
		return($response['data']);
	}


	function del_message( $message_id, $box_id, $account_id = null ) { 
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("DELETE","/v1/accounts/{$account_id}/vmboxes/$box_id/messages/$message_id");
		return($response['data']);
	}


	function get_message_raw( $message_id, $box_id, $account_id = null ) { 

		$tmp = $this->force_no_decode;
		$this->force_no_decode = true;

		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("GET","/v1/accounts/{$account_id}/vmboxes/$box_id/messages/$message_id/raw");
		$this->force_no_decode = $tmp;
		return($response);
	}

	
	function get_media( $media_id = null, $account_id = null ) {
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("GET","/v1/accounts/{$account_id}/media/{$media_id}" );
		return($response['data']);

	}
	

	function get_media_raw( $media_id = null, $account_id = null ) {

		$tmp = $this->force_no_decode;
		$this->force_no_decode = true;

		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("GET","/v1/accounts/{$account_id}/media/{$media_id}/raw" );

		$this->force_no_decode = $tmp;
		return($response);

	}

	
	function del_media( $media_id, $account_id = null ) {
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("DELETE","/v1/accounts/{$account_id}/media/{$media_id}" );
		return($response);
	}
	

	function get_users( $account_id = null ) {
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("GET","/v1/accounts/{$account_id}/users" );
		return($response['data']);
	}


	function get_resources( $account_id = null ) { 
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("GET","/v1/accounts/{$account_id}/local_resources");
		return($response['data']);
	}


	function get_available_subscriptions( $account_id = null ) {

		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("GET","/v1/accounts/{$account_id}/events/available");
		return($response['data']);
	}


	function get_subs( $account_id = null ) {return( $this->get_available_subscriptions($account_id)); } 














	function get_conference_map() {
		$cf_nums = $this->get_callflows();
		$map = array();
		foreach( $cf_nums as $cf_num ) {
                        $cf = $this->get_callflow($cf_num['id']);
                        //if( $cf['numbers'][0] == "2222" ||  $cf['numbers'][0] == "3333" ) print_r($cf);
			//print_r($cf);
			if( $cf['flow']['module'] == 'conference' ) {
				foreach( $cf['numbers'] as $number ) {
					if( isset( $cf['flow']['data']['id'] ) ) {
						$map['direct'][$cf['flow']['data']['id']][] = $number;
						//$map[$cf['flow']['data']['id']]['direct'][] = $number;
					} else {
						//$map['service'][$cf['id']][] = $number;
						$map['service'][] = $number;
						//$map[$cf['id']]['service'][] = $number;
					}
				}

			}	
                
                }
		return($map);


	}


        function get_conferences( $account_id = null ) {
                if( $account_id == null ) $account_id = $this->use_account_id;
                $response = $this->send("GET","/v1/accounts/{$account_id}/conferences");
		return($response['data']);
			
        }


        function find_conferences( $account_id = null ) {
                if( $account_id == null ) $account_id = $this->use_account_id;
                $response = $this->send("GET","/v1/accounts/{$account_id}/conferences/{$conference_id}");
		return($response['data']);
        }

	
        function get_conference( $conference_id = null, $account_id = null ) {
                if( $account_id == null ) $account_id = $this->use_account_id;
                $response = $this->send("GET","/v1/accounts/{$account_id}/conferences/{$conference_id}");
		return($response['data']);
        }


	function put_conference( $data, $account_id = null ) { 
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("PUT","/v1/accounts/{$account_id}/conferences/",json_encode(array('data'=>$data)));
		return($response);
	}


	function post_conference( $data, $conference_id, $account_id = null ) { 
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("POST","/v1/accounts/{$account_id}/conferences/$conference_id", json_encode(array('data'=>$data)));
		return($response);
	}


        function del_conference( $conference_id = null, $account_id = null ) {
                if( $account_id == null ) $account_id = $this->use_account_id;
                $response = $this->send("DELETE","/v1/accounts/{$account_id}/conferences/{$conference_id}");
		return($response);
        }










        function get_directories( $account_id = null ) {
                if( $account_id == null ) $account_id = $this->use_account_id;
                $response = $this->send("GET","/v1/accounts/{$account_id}/directories");
		return($response['data']);
			
        }

	
        function get_directory( $directory_id = null, $account_id = null ) {
                if( $account_id == null ) $account_id = $this->use_account_id;
                $response = $this->send("GET","/v1/accounts/{$account_id}/directories/{$directory_id}");
		return($response['data']);
        }


	function put_directory( $data, $account_id = null ) { 
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("PUT","/v1/accounts/{$account_id}/directories/",json_encode(array('data'=>$data)));
		return($response);
	}


	function post_directory( $data, $directory_id, $account_id = null ) { 
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("POST","/v1/accounts/{$account_id}/directories/$directory_id", json_encode(array('data'=>$data)));
		return($response);
	}


        function del_directory( $directory_id = null, $account_id = null ) {
                if( $account_id == null ) $account_id = $this->use_account_id;
                $response = $this->send("DELETE","/v1/accounts/{$account_id}/directories/{$directory_id}");
		return($response);
        }


	











	function get_device( $device_id, $account_id = null ) { 
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("GET","/v1/accounts/{$account_id}/devices/$device_id");
		return($response['data']);
	}


	function del_device( $device_id, $account_id = null ) { 
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("DELETE","/v1/accounts/{$account_id}/devices/$device_id");
		return($response);
	}


	function put_device( $data, $account_id = null ) { 
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("PUT","/v1/accounts/{$account_id}/devices/",json_encode(array('data'=>$data)));
		return($response);
	}


	function post_device( $data, $device_id, $account_id = null ) { 
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("POST","/v1/accounts/{$account_id}/devices/$device_id", json_encode(array('data'=>$data)));
		return($response);
	}


	function get_vmbox( $box_id, $account_id = null ) { 
		$response = $this->get("vmboxes",$box_id,null,$account_id);
		return($response);
	}


	function del_vmbox( $box_id, $account_id = null ) {
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("DELETE","/v1/accounts/{$account_id}/vmboxes/$box_id");
		return($response);
	}


	function put_vmbox( $data, $account_id = null ) { 
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("PUT","/v1/accounts/{$account_id}/vmboxes/", json_encode(array('data'=>$data)));
		return($response);
	}


	function post_vmbox( $data, $box_id, $account_id = null ) { 
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("POST","/v1/accounts/{$account_id}/vmboxes/$box_id", json_encode(array('data'=>$data)));
		return($response);
	}


	function get_user( $user_id, $account_id = null ) {
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("GET","/v1/accounts/{$account_id}/users/$user_id" );
		return($response['data']);
	}


	function del_user( $user_id, $account_id = null ) {
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("DELETE","/v1/accounts/{$account_id}/users/$user_id");
		return($response);
	}


	function put_user( $data, $account_id = null ) {
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("PUT","/v1/accounts/{$account_id}/users", json_encode(array('data'=>$data)));
		return($response);
	}


	function post_user( $data, $user_id, $account_id = null ) {
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("POST","/v1/accounts/{$account_id}/users/$user_id", json_encode(array('data'=>$data),JSON_FORCE_OBJECT));
		return($response);
	}


	function get_account( $account_id = null ) {
		if( $account_id == null ) $account_id = $this->use_account_id;
		$account = $this->send("GET","/v1/accounts/$account_id");
		return($account['data']);
	}


	function del_account( $account_id ) {
		$response = $this->send("DELETE","/v1/accounts/$account_id/");
		return($response);
	}


	function put_account( $data, $account_id = null ) {
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("PUT","/v1/accounts/{$account_id}", json_encode(array('data'=>$data)));
		return($response);
	}
	

	function post_account( $data, $account_id = null ) {
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("POST","/v1/accounts/{$account_id}", json_encode(array('data'=>$data)));
		return($response);
	}







	function get_menus( $account_id = null) {
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("GET","/v1/accounts/$account_id/menus");
		return($response['data']);
	}


	function get_menu( $menus_id, $account_id = null ) {
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("GET","/v1/accounts/$account_id/menus/$menus_id");
		return($response['data']);
	}


	function put_menu( $data, $account_id = null) {
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("PUT","/v1/accounts/{$account_id}/menus", json_encode(array('data'=>$data)));
		return($response);
	}

	function post_menu( $data, $menus_id, $account_id = null) {
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("POST","/v1/accounts/{$account_id}/menus/{$menus_id}", json_encode(array('data'=>$data)));
		return($response);
	}

	function del_menu( $menu_id, $account_id = null ) {
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("DELETE","/v1/accounts/$account_id/menus/$menu_id");
		return($response);
	}









	function get_temporal_rules( $account_id = null) {
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("GET","/v1/accounts/$account_id/temporal_rules");
		return($response['data']);
	}


	function get_temporal_rule_by_name( $name, $account_id = null ) {
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("GET","/v1/accounts/$account_id/temporal_rules/");
		foreach( $response['data'] as $temporal_rule ) if( $temporal_rule['name'] == $name ) return($temporal_rule);
		return(false);
	}



	function get_temporal_rule( $temporal_rule_id, $account_id = null ) {
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("GET","/v1/accounts/$account_id/temporal_rules/$temporal_rule_id");
		return($response['data']);
	}


	function put_temporal_rules( $data, $account_id = null) {
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("PUT","/v1/accounts/{$account_id}/temporal_rules", json_encode(array('data'=>$data)));
		return($response);
	}

	function post_temporal_rules( $data, $temporal_rule_id, $account_id = null) {
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("POST","/v1/accounts/{$account_id}/temporal_rules/{$temporal_rule_id}", json_encode(array('data'=>$data)));
		return($response);
	}

	function del_temporal_rules( $temporal_rule_id, $account_id = null ) {
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("DELETE","/v1/accounts/$account_id/temporal_rules/$temporal_rule_id");
		return($response);
	}






























	function get_callflows( $account_id = null) {
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("GET","/v1/accounts/$account_id/callflows");
		return($response['data']);
	}


	function get_callflow( $call_flow_id, $account_id = null ) {
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("GET","/v1/accounts/$account_id/callflows/$call_flow_id");
		return($response['data']);
	}


	function put_callflow( $data, $account_id = null) {
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("PUT","/v1/accounts/{$account_id}/callflows", json_encode(array('data'=>$data)));
		return($response);
	}

	function post_callflow( $data, $cf_id, $account_id = null) {
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("POST","/v1/accounts/{$account_id}/callflows/{$cf_id}", json_encode(array('data'=>$data)));
		return($response);
	}


	function del_callflow( $call_flow_id, $account_id = null ) {
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("DELETE","/v1/accounts/$account_id/callflows/$call_flow_id");
		return($response);
	}


	/*
	function profile_send( $method, $url, $post_data = NULL ) {
		$mstart = microtime(true);
		$data = send($method,$url,$post_data);
		$mend = microtime(true);
		return( array( 'data' => $data, 'µTime' => ( $mend - $mstart )));	
	}
	*/

	function post_media( $data, $account_id = null ) {
		if( $account_id == null ) $account_id = $this->use_account_id;
		$response = $this->send("PUT","/v1/accounts/{$account_id}/media/",json_encode( array( 'data' => $data['data'] ) ) );


		if( $response['status'] == 'success' && isset($response['data']['id']) ) {
			$response = $this->send("POST","/v1/accounts/{$account_id}/media/{$response['data']['id']}/raw",$data['raw'], $data['type'] );
		}
		return($response);

	}


	/** 
	@brief Gets the details on all the cdrs
	@param $account_id The account that the cdr belongs too
	@return $response The response is an array that is just passed through. 
	**/
	function get_cdrs( $filters = Array(), $account_id = null ) {

		if( $account_id == null ) $account_id = $this->use_account_id;
		if( $filters == null ) $filters = Array();
		$filter = '';

                if( count($filters) ) {
                        foreach( $filters as $key => $val ) $filter .= "$key=$val&";
                        if( strlen($filter) ) $filter = '?'.substr($filter,0,-1);
                } else if( strlen($id) ) {
                        $filter = "/$id";
                }

		$response = $this->send("GET","/v1/accounts/{$account_id}/cdrs{$filter}", null, null, "*/*" );

		return($response['data']);
	}

	/** 
	@brief Accepts a URL to a PDF to then fax out
	@param $data The data as defined at https://2600hz.atlassian.net/wiki/display/docs/Faxes+API
	@param $account_id The account that is sending the fax
	@return $response The response is an array that is just passed through. 
	**/
	function put_fax( $data, $account_id = null ) {
		if( $account_id == null ) $account_id = $this->use_account_id;
			$response = $this->send("PUT","/v1/accounts/{$account_id}/faxes",json_encode( array( 'data' => $data['data'] ) ) );
		return($response);
	}

	/** 
	@brief Gets the details on a specific fax
	@param $fax_id The unique fax id as returned by the get_faxes func
	@param $account_id The account that the fax belongs too
	@return $response The response is an array that is just passed through. 
	**/
    function get_fax( $fax_id = null, $account_id = null ) {
		if( $account_id == null ) $account_id = $this->use_account_id;
			$response = $this->send("GET","/v1/accounts/{$account_id}/faxes/{$fax_id}" );
		return($response['data']);
	}
		
	/** 
	@brief Gets the details on all the faxes
	@param $account_id The account that the fax belongs too
	@return $response The response is an array that is just passed through. 
	**/
	function get_faxes( $account_id = null ) {
		if( $account_id == null ) $account_id = $this->use_account_id;
			$response = $this->send("GET","/v1/accounts/{$account_id}/faxes/outgoing" );
		return($response['data']);
	}
        
	/** 
	@brief Gets the actual fax file that was sent
	@param $fax_id The unique fax id as returned by the get_faxes func
	@param $account_id The account that the fax belongs too
	@return $response The response is an array that is just passed through. 
	**/
	function get_fax_file( $fax_id = null, $account_id = null ) {
		// Doesn't exist in the 2600 api yet but should.
		$bldred=chr(0x1B).'[1;31m';
		$this->log("{$bldred}!!!!: get_fax_file not yet implemented.");
				
		return( array('status' => 'failure', 'message' => 'del_fax not yet implemented.') );
		
		/*
		// From the get_media_raw call above and tweaked a bit.
		$tmp = $this->force_no_decode;
		$this->force_no_decode = true;

		if( $account_id == null ) $account_id = $this->use_account_id;
			$response = $this->send("GET","/v1/accounts/{$account_id}/faxes/{$fax_id}/file" );

		$this->force_no_decode = $tmp;
		return($response);
		
		*/
	}
    
	/** 
	@brief Deletes a fax from the queue?  Do we really need this?
	@param $fax_id The unique fax id as returned by the get_faxes func
	@param $account_id The account that the fax belongs too
	@return $response The response is an array that is just passed through. 
	**/
	function del_fax( $fax_id, $account_id = null ) {
		// Doesn't exist in the 2600 api yet but should.
		$bldred=chr(0x1B).'[1;31m';
		$this->log("{$bldred}!!!!: del_fax not yet implemented.");
		
		return( array('status' => 'failure', 'message' => 'del_fax not yet implemented.') );

		/*
		// From the del_media call above and tweaked a bit.
		if( $account_id == null ) $account_id = $this->use_account_id;
			$response = $this->send("DELETE","/v1/accounts/{$account_id}/faxes/{$fax_id}" );
		return($response);
		*/
	}


	function send_object( $method, $url, $post_data = NULL, $type = 'application/json' ) {
		$this->send($method,$url,$post_data,$type);
		return( json_decode($this->body) );	

	}
	
	function send( $method, $url, $post_data = NULL, $type = 'application/json', $accept_type = "application/json, application/octet-stream, audio/*, */*" ) {

		$bldred=chr(0x1B).'[1;31m'; $bldgrn=chr(0x1B).'[1;32m'; $bldylw=chr(0x1B).'[1;33m'; $bldblu=chr(0x1B).'[1;34m'; $bldpur=chr(0x1B).'[1;35m'; $bldcyn=chr(0x1B).'[1;36m'; $bldwht=chr(0x1B).'[1;37m'; $txtrst=chr(0x1B).'[0m'; 


		$mstart = microtime(true);
		if( $this->socket_stream == null ) {
			$this->socket_stream = fsockopen($this->host, $this->port, $errno, $errstr);
		}
		//$this->log(" fsockopen Errno: $errno Str:$errstr");	
		if( !$this->socket_stream ) {
			$this->socket_stream = null;
			$this->fsock_errno = $errno;
			$this->fsock_errstr = $errstr;
			return false; //if the connection fails return false
		}

		//$request = "$method $url HTTP/1.1\r\nHost: $this->host:$this->port\r\n";
		$request = "$method $url HTTP/1.0\r\nHost: $this->host\r\n";
		if (isset($this->user)) $request .= "Authorization: Basic ".base64_encode("$this->user:$this->pass")."\r\n";


		if( $type != null ) $request .= "Content-Type: $type\r\n";
		//$request .= "Accept: application/json, application/octet-stream, audio/*\r\n";
		$request .= "Accept: $accept_type\r\n";
		if( isset($this->xauth) ) $request .= "X-Auth-Token: {$this->xauth}\r\n";

		if($post_data) {
			//$request .= "Content-Type: application/json\r\n";
			$request .= "Content-Length: ".strlen($post_data)."\r\n\r\n";
			$request .= "$post_data\r\n";
		} else {
			$request .= "\r\n";
		}


		fwrite($this->socket_stream, $request);
		//fflush($this->socket_stream);


		$response = "";

		while(!feof($this->socket_stream) && $this->socket_stream != null ) { $response .= fgets($this->socket_stream); }

		fclose($this->socket_stream);
		$this->socket_stream = null;

		$mend = microtime(true);

		//if( $this->profile ) printf("{$bldblu}URL:{$bldylw}$url {$bldblu}µT:{$bldylw}".( $mend - $mstart ).$txtrst."\n");
		$this->log( "{$bldred}{$method} {$this->host}:{$this->port}$url{$txtrst} {$bldylw}µT:".( $mend - $mstart )."{$txtrst}");
		//$this->log( print_r($response,true) );

		list($this->headers, $this->body) = explode("\r\n\r\n", $response,2);

		$REQUEST_ID = '';

		if( strlen($this->headers) ) {
			$hexp = explode("\n",$this->headers);
			foreach( $hexp as $line ) {
				if( strstr($line,"X-Request-ID") ) {
					$reqxp = explode(":",$line);
					$REQUEST_ID = $reqxp[1];
				}				


			}
		}

		if( $method == "DELETE" ) {
			if( stristr($this->headers,"204 No Content") ) {
				return( array('status' => 'success'));
			}

		}




		//quick lazy check 
		if( !stristr($this->headers,"200 OK") && !stristr($this->headers,"201 Created")) { 

			$this->log("{$bldpur}>>>>: $method $url HTTP/1.0 ($type) len:".strlen($post_data)."$txtrst \n");
			if( $post_data && $type == 'application/json' ) $this->log("{$bldpur}>>>>: ".trim($post_data)."\n");
			$this->log("{$bldylw}<<<<: ".trim($hexp[0])." µT=".( $mend - $mstart )." request_id:$REQUEST_ID{$txtrst}\n");
			$this->log("{$bldylw}<<<<: ".$this->headers."{$txtrst}");
			$this->log("{$bldylw}<<<<: ".$this->body."{$txtrst}");


		}

		//same thing here
		if( stristr($this->headers,"401 Unauthorized") || stristr($this->headers,"400 Not Found") || stristr($this->headers,"500 Internal Server") || stristr($this->headers,"400 Bad Request") ) { 
				//$this->log("{$bldpur}Found 401{$txtrst}");
				$temp =  json_decode($this->body,true);
				
				//return( array("data" => array('status' => 'failure', 'message' => $temp['message'])) );
				return( array('status' => 'failure', 'message' => $temp['message']) );
				//return false;
		}


		if( $this->force_no_decode ) { return $this->body; } 
		return json_decode($this->body,true);
		

	}



}



?>
