<?php
// page located at http://example.com/process_gather.php
header('Content-Type: text/xml');
echo "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
echo "<Response>";
echo "<Say>You entered " . $_REQUEST['Digits'] . "</Say>";
echo "<Say>I will now play a short song</Say>";
echo "<Play loop='1'>http://a.tumblr.com/tumblr_m0p8z5YLEb1r23p09o1.mp3</Play>";
echo "<Redirect method='GET'>/dial.xml</Redirect>";
echo "<Say>The redirection failed</Say>";
echo "</Response>";
?>
