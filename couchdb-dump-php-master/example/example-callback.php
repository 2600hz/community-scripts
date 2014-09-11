<?php

return function ($doc, $latestRev) {
    // save last 3 revisions
    $revParts = explode('-', $doc['_rev'], 2);
    $lastRevParts = explode('-', $latestRev, 2);
    return ($lastRevParts[0] - $revParts[0] < 3);
};
