<?php



class CouchDB {

	function CouchDB( $options ) {
		$this->force_no_decode = false;	
		$this->debug = false;	
		

		foreach($options as $key => $value) $this->$key = $value; 

		$couch_is_alive = file_get_contents("http://{$this->host}:{$this->port}");

		


		$auth = array();

	}

	function log( $logthis ) {
		if( $this->debug ) {
			echo $logthis."\n";
		} else {
			@file_put_contents("/var/log/couchdb.php.log",date("Y-m-d H:i:s")." - ".$logthis."\n",FILE_APPEND);
		}
	}

	
	function send( $method, $url, $post_data = NULL, $type = 'application/json' ) {

		$bldred=chr(0x1B).'[1;31m'; $bldgrn=chr(0x1B).'[1;32m'; $bldylw=chr(0x1B).'[1;33m'; $bldblu=chr(0x1B).'[1;34m'; $bldpur=chr(0x1B).'[1;35m'; $bldcyn=chr(0x1B).'[1;36m'; $bldwht=chr(0x1B).'[1;37m'; $txtrst=chr(0x1B).'[0m'; 


		$mstart = microtime(true);
		$s = fsockopen($this->host, $this->port, $errno, $errstr);
		if(!$s) {
			echo "$errno: $errstr\n";
			return false;
		}

		//$request = "$method $url HTTP/1.1\r\nHost: $this->host:$this->port\r\n";
		$request = "$method $url HTTP/1.0\r\nHost: $this->host\r\n";
		if (isset($this->user)) $request .= "Authorization: Basic ".base64_encode("$this->user:$this->pass")."\r\n";


		$request .= "Content-Type: $type\r\n";
		$request .= "Accept: application/json, application/octet-stream, audio/*\r\n";
		if( isset($this->xauth) ) $request .= "X-Auth-Token: {$this->xauth}\r\n";

		if($post_data) {
			//$request .= "Content-Type: application/json\r\n";
			$request .= "Content-Length: ".strlen($post_data)."\r\n\r\n";
			$request .= "$post_data\r\n";
		} else {
			$request .= "\r\n";
		}


		fwrite($s, $request);
		$response = "";

		while(!feof($s)) { $response .= fgets($s); }
		fclose($s);

		$mend = microtime(true);

		//if( $this->profile ) printf("{$bldblu}URL:{$bldylw}$url {$bldblu}µT:{$bldylw}".( $mend - $mstart ).$txtrst."\n");
		$this->log( "{$this->host}:{$this->port} $method $url µT:".( $mend - $mstart ));

		list($this->headers, $this->body) = explode("\r\n\r\n", $response);

		if( $method == "DELETE" ) {
			if( stristr($this->headers,"204 No Content") ) {
				return( array('status' => 'success'));
			}

		}

		/*
		if( !stristr($this->headers,"200 OK") && !stristr($this->headers,"201 Created")) { 
			$this->log("{$bldpur}>>>>: $method $url HTTP/1.0 ($type) POST_LENGTH:".strlen($post_data)."$txtrst \n");
			if( $post_data && $type == 'application/json' ) printf("{$bldpur}POST_DATA: ".trim($post_data)."\n");
			$this->log("{$bldylw}<<<<: µT=".( $mend - $mstart )."{$txtrst}\n");
			$this->log("{$bldylw}<<<<: H:".$this->headers."{$txtrst}");
			$this->log("{$bldylw}<<<<: B:".$this->body."{$txtrst}");
		}
		*/
		
		if( $this->force_no_decode ) {

			return $this->body;
		} 

		return json_decode($this->body,true);
		

	}



}



?>
