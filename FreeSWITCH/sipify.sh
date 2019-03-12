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
    PktLnCnt = 0
    FldOfPktByte = 0
    AddCallidAt = -1
    CallID = ""
}
#{
#another way to remove file name present from grepping multiple files
#sub(/^[^ :]+:/, "")
#}
FldOfPktByte == 0 && /send|recv/ && /(send|recv) [0-9]* bytes (to|from)/ {
    # find the awk field that has the send or recv to auto support FS direct
    # logs and ones with stuff like date-time or log file name in front of packet 
    # data like syslog and grep multiple files
    i = 0
    while ( i++ <= NF && FldOfPktByte == 0 )
        if ( $i == "bytes" ) #send|recv not in own field when from grep multi file
            FldOfPktByte = i
}
$FldOfPktByte == "bytes" && AddCallidAt < 0 && /(send|recv) [0-9]* bytes (to|from)/ {
    # found start of packet
    # find char pos to insert the call-id
    OffsetStr = $0
    sub(/(send|recv) [0-9]* bytes (to|from).*/, "", OffsetStr)
    AddCallidAt = length(OffsetStr) + 1
    if ( $(FldOfPktByte - 2) ~ ".(send|recv)$" ) {
        HeaderFldOffset = FldOfPktByte - 1
    } else {
       HeaderFldOffset = FldOfPktByte - 2
    }
    PktLines[PktLnCnt++] = $0
    next
}
AddCallidAt < 0 {
    # not in a sip packet dump print whole line as is
    print $0
    next
}
AddCallidAt >= 0 {
    # in a sip packet dump
    if ( $0 == "--" || $0 ~ "[^-]--$" ) {
        #end of grep context, print the packet we have so far
        printpacket()
        print $0 # the end of grep context
        next
    }
    if ( substr($0, (AddCallidAt - 1), 3) != "   " && $HeaderFldOffset != "" ) {
        # some log line mixed in the middle of the packet dump. Print ahead of packet dump.
        print $0
        next  
    }
    # buffer line till end of packet
    PktLines[PktLnCnt++] = $0
    if ($HeaderFldOffset == "Call-ID:" || $HeaderFldOffset == "i:") {
        #found packet call-id save for print insert
        CallID = $(HeaderFldOffset + 1)
        next
    }
    if (substr($HeaderFldOffset, 0, 5) == "-----" ) {
        # packet delimiter or end of grep context
        if(PktLnCnt > 2 ){
            # end of packet (line 2 of packet also has "----")
            printpacket()
        }
    }
}
END{
    #end of file but maybe not end of packet and need to print the lines
    printpacket()
}
function printpacket(){
    for(i=0;i<PktLnCnt;i++){
        print substr(PktLines[i], 0, AddCallidAt - 1) CallID, substr(PktLines[i], (AddCallidAt))
        delete PktLines[i]
    }
    PktLnCnt = 0
    CallID = ""
    AddCallidAt = -1
}
' "$@"
}
if [[ ( -z "$@" || "$1" == "-h" || "$1" == "--help" ) && -t 0 ]] ; then
    usage
else
     #sed 's/^[^ :]\+://' "$@" <&0 | sipify # strip file path from a multi file grep that puts the file/path in front of every log line. FYI: "grep --no-filename" will surpress the file name
     sipify "$@" <&0
fi
