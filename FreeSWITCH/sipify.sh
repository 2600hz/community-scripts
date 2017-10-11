#!/bin/bash
# A useful tool for viewing FreeSWITCH SIP logs
# 2600hz - The Future of Cloud Telecom
usage() {
cat<<'EOF'
 Usage: grep -EC 100 -e 'regex of your_call_ids' /var/log/freeswitch/debug.log | ./sipify.sh | grep -EC 5 -e 'regex of your_call_ids'

 The reason for grepping first is grep is much faster than awk (sipify script):
 https://davidlyness.com/post/the-functional-and-performance-differences-of-sed-awk-and-other-unix-parsing-utilities

 Here is an example with a bunch of stuff put together.
 pv -qL 10m < /var/log/freeswitch/debug.log | \
 zgrep -EC 100 -e 'your_call_id|leg-2-call_id|leg-3-call-id' | \
 ./sipify.sh | \
 grep -EC 5 -e 'your_call_id|leg-2-call_id|leg-3-call-id' | \
 tee /tmp/ticket_num_`date +%b%d`_`hostname -f`.log

Explanation and significance of each part of the above command.

pv -qL 10m < /var/log/freeswitch/debug.log | \

 Use pv to read the log file and the rate it is read from disk. Reading from
 disk unrestricted can cause high system load blocking other critical services.
 Unzip, grep, and sipify (awk) can also use 100% CPU if reading unrescricted.

zgrep -EC 100 -e 'your_call_id|leg-2-call_id|leg-3-call-id' | \

 First grep is done with zgrep which will auto detect and unzip a compressed file
 or compressed piped stream. Grep is also way faster then awk for simple searches
 and the awk script is much more complex than a simple search. So grep
 is used before awk to efficiently filter a larg file to at most a few thousand
 log lines are passed to sipify (awk). Since the SIP packet lines don't all have
 the call id on them this grep needs to output context (lines before and after a
 match), the -C 100 option. It uses -E for extended regex so the pipe symbol will
 mean logical OR in the search string. That way you can specifiy multiple call ids.
 Finally the -e signifies what follows is the pattern match. This can be important
 because of the way it calls the normal grep internally and in somecases the
 call-id may have characters that confuse grep.

./sipify.sh | \

 finally pass it to sipify to detect sip packets find the call id for the packet
 and prefix all the lines of the packet with the call id for next grep with
 tighter context for the search filter.

grep -EC 5 -e 'your_call_id|leg-2-call_id|leg-3-call-id' | \

 The final filtering with grep. I like to still keep some context to the actual
 matched lines because there are often lines that do not have the call id on them
 that are part of work for the call. This happens often with log lines from
 mod_kazoo and errors. The things that can give the best clues when there is a
 problem.

tee /tmp/ticket_num_`date +%b%d`_`hostname -f`.log

 This part is mostly personal preference. I like to use tee so it outputs to the
 file and screen so I can see that something was actually found without checking
 afterword the output file. The filename I like to have be something meaningful
 so make it have the ticket number, the month/day like Jan03 and the hostname.

 if your /tmp dir has lots of files, recordings and inbound faxes are put there,
 and your desktop SCP client wants to list all the files before you can find and
 copy it, make a dir like /tmp/log/ and put the log in it.

 NOTE pv is pipe verbose. it has the ability to throttle data passed through it
   as well its other primary feature. Nice for preventing grep from overloading
   disk or CPU. Installed with yum install pv from epel repo.
EOF
}

sipify(){
awk '
BEGIN{
    I = 0
    MSGFLDOFFSET = 0
    ADDCALLIDAT = -1
    CALLID = ""
}
MSGFLDOFFSET == 0 && /send|recv/ && /(send|recv) [0-9]* bytes (to|from)/ {
    # find the awk field that has the send or recv to auto support both FS direct
    # logs and ones with stuff like date-time in front of packet data like syslog
    i = 0
    while ( i++ <= NF && MSGFLDOFFSET == 0 ) {
        if ( $i == "send" || $i == "recv" ) {
            MSGFLDOFFSET = i
        }
    }
}
$MSGFLDOFFSET == "send" || $MSGFLDOFFSET == "recv" {
    # another part to auto support syslog or FS direct
    # find char pos to insert the call-id
    t = $0
    sub(/(send|recv) [0-9]* bytes (to|from).*/, "", t)
    ADDCALLIDAT = length(t)
}
ADDCALLIDAT < 0 {
    # not in a sip packet dump print whole line as is
    print $0
}
ADDCALLIDAT >= 0 {
    # in a sip packet dump
    # buffer line till end of packet
    PACKET[I++] = $0
    if ($MSGFLDOFFSET == "Call-ID:" || $MSGFLDOFFSET == "i:") {
        #found packet call-id save for print insert
        CALLID = $(MSGFLDOFFSET + 1)
    }
    if (substr($MSGFLDOFFSET, 0, 5) == "-----") {
        # packet delimiter
        if(CALLID != ""){
            # end of packet, print each line
            for( i = 0; i < I ; i++ ){
                print substr(PACKET[i], 0, ADDCALLIDAT) CALLID, substr(PACKET[i], (ADDCALLIDAT + 1))
                delete PACKET[i]
            }
            I = 0
            CALLID = ""
            ADDCALLIDAT = -1
        }
    }
}
END{
    if ( CALLID != "" ){
        #end of file but not end of packet and we have the call-id
        for(i=0;i<I;i++){
            print substr(PACKET[i], 0, ADDCALLIDAT) PRESP CALLID, substr(PACKET[i], (ADDCALLIDAT + 1))
            #delete PACKET[i]
        }
    }
}
' "$@"
}
if [[ ( -z "$@" || "$1" == "-h" || "$1" == "--help" ) && -t 0 ]] ; then
    usage
else
    sipify "$@" <&0
fi
