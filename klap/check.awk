# zcat kazoo.log.gz | awk -f check.awk | sort -h

BEGIN {
    format_str = "%-16s | %12s | %-32s | %s\n";
    printf(format_str, "ClientIP", "AuthType", "RequestId", "Request Payload")
}

# Get auth type and request id
#{MONTH} {DAY} {H:M:S} {SRV} {SRV_PID}: |{REQUEST_ID}|api_resource:113({PID}) PUT: /v2/user_auth? from {IP}
#$1      $2    $3      $4    $5:        $6                                    $7   $8             $9   $10
/^[^2].+PUT: .+_auth/ {
    split($6, request_id_arr, "|");
    request_id=request_id_arr[2];

    split($8, auth_type_arr, "/");
    auth_type=auth_type_arr[3];

    ip=$10
}

# when using precise timestamps
# {TIMESTAMP} {HOST} {SRV_PID}: |{REQUEST_ID}|api_resource:113({PID}) PUT: /v2/user_auth? from {IP}
# $1          $2     $3         $4                                    $5   $6             $7   $8
/^2.+PUT: .+_auth/ {
    split($4, request_id_arr, "|");
    request_id=request_id_arr[2];

    split($6, auth_type_arr, "/");
    auth_type=auth_type_arr[3];

    ip=$8
}

# parse json payload line
#{MONTH} {DAY} {H:M:S} {SRV} {SRV_PID}: |{REQUEST_ID}|api_util:587({PID}) request has a json payload: {"data":{"api_key":"{API_KEY}"}}
#$1      $2    $3      $4    $5:        $6                                $7      $8 $9 $10  $11      $12
/^[^2].+request has a json payload/ && request_id && match($6, request_id) > 0 && $auth_type {
    printf(format_str, ip, auth_type, request_id, $12);
    request_id=0
}

# {TIMESTAMP} {HOST} {SRV_PID}: |{REQUEST_ID}|api_util:587({PID}) request has a json payload: {REQUEST_JSON}}
# $1          $2     $3         $4                                $5      $6 $7 $8   $9       $10
/^2.+request has a json payload/ && request_id && match($4, request_id) > 0 && $auth_type {
    printf(format_str, ip, auth_type, request_id, $10);
    request_id=0
}
