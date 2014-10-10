# FreeSWITCH Utils
=================

## user_agents.sh \<path\>
Show the number of times unique user-agents occur in a FreeSWITCH debug log.
```bash
[root@fs001-dev FreeSWITCH]# ./user_agents.sh /var/log/freeswitch/debug.log.1 
      4    User-Agent: PolycomVVX-VVX_500-UA/5.0.0.6874
     16    User-Agent: 2600hz
```

## sipify.sh \<path\>
The FreeSWITCH debug log does not include the call-id on the lines with the SIP headers.  This script will add them so they are included in when grep'd or otherwise searched.  

_Note:_ This can consume large amounts of memory, and impact the running system.  Use with caution.

```bash
[root@fs001-dev FreeSWITCH]# ./sipify.sh /var/log/freeswitch/debug.log.1 | grep 46a1fe62-20f73e9-1c6e7dbc@10.26.0.199                                 
46a1fe62-20f73e9-1c6e7dbc@10.26.0.199 recv 1141 bytes from udp/[10.26.0.81]:5060 at 19:34:41.657063:
46a1fe62-20f73e9-1c6e7dbc@10.26.0.199    ------------------------------------------------------------------------
46a1fe62-20f73e9-1c6e7dbc@10.26.0.199    INVITE sip:*97@kanderson.dev.2600hz.com;user=phone SIP/2.0
46a1fe62-20f73e9-1c6e7dbc@10.26.0.199    Record-Route: <sip:10.26.0.81;lr=on;ftag=C4F24CBE-866C8875>
46a1fe62-20f73e9-1c6e7dbc@10.26.0.199    Via: SIP/2.0/UDP 10.26.0.81;branch=z9hG4bKae67.eae1c251.0
46a1fe62-20f73e9-1c6e7dbc@10.26.0.199    Via: SIP/2.0/UDP 10.26.0.199:5060;rport=5060;branch=z9hG4bKb8aa0385D3717F7
46a1fe62-20f73e9-1c6e7dbc@10.26.0.199    From: "user_ngmdk8" <sip:user_ngmdk8@kanderson.dev.2600hz.com>;tag=C4F24CBE-866C8875
46a1fe62-20f73e9-1c6e7dbc@10.26.0.199    To: <sip:*97@kanderson.dev.2600hz.com;user=phone>
46a1fe62-20f73e9-1c6e7dbc@10.26.0.199    CSeq: 1 INVITE
46a1fe62-20f73e9-1c6e7dbc@10.26.0.199    Call-ID: 46a1fe62-20f73e9-1c6e7dbc@10.26.0.199
46a1fe62-20f73e9-1c6e7dbc@10.26.0.199    Contact: <sip:user_ngmdk8@10.26.0.199:5060>
46a1fe62-20f73e9-1c6e7dbc@10.26.0.199    Allow: INVITE, ACK, BYE, CANCEL, OPTIONS, INFO, MESSAGE, SUBSCRIBE, NOTIFY, PRACK, UPDATE, REFER
46a1fe62-20f73e9-1c6e7dbc@10.26.0.199    User-Agent: PolycomVVX-VVX_500-UA/5.0.0.6874
46a1fe62-20f73e9-1c6e7dbc@10.26.0.199    Accept-Language: en
46a1fe62-20f73e9-1c6e7dbc@10.26.0.199    Supported: 100rel,replaces
46a1fe62-20f73e9-1c6e7dbc@10.26.0.199    Allow-Events: conference,talk,hold
46a1fe62-20f73e9-1c6e7dbc@10.26.0.199    Content-Type: application/sdp
46a1fe62-20f73e9-1c6e7dbc@10.26.0.199    Content-Length: 332
46a1fe62-20f73e9-1c6e7dbc@10.26.0.199    X-AUTH-IP: 10.26.0.199
```
