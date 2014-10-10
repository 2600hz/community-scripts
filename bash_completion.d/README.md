# bash completion scripts
=================

This script will give you tab completion in bash for sup commands

## Installation

Step 1: Make sure you have the bash-completion package installed in your distribution (the package is probably called bash-completion)

Step 2: Place sup.sh in /etc/bash_completion.d

Step 3: chmod the script executable, "chmod +x /etc/bash_completion.d/sup.sh"

Step 4: Log out and back in (or, execute "source /etc/bash_completion.d/sup.sh" to load it

Step 5: Type sup and hit tab!  Rejoice!

## Notes / Caveats

1) When using the ecallmgr module, you must go back and modify the command to include "-n ecallmgr".  

(i.e. sup ecallmgr_maintenance channel_summary should become sup -n ecallmgr ecallmgr_maintenance channel_summary)

2) This module ONLY currently looks for modules ending in _maintenance - I realize that other functions can be called, but this was just a first version.  Feel free to modify the script! :) 

3) When tab completing function names, it will tell you the argument count the function expects, but you MUST remove that from the command before execution or it will fail.

(i.e. You will be able to tab complete to something like "sup stepswitch_maintenance process_number/1" which tells you that process_number takes one argument.  On execution, it needs to be removed (and the argument added).  Example:  "sup stepswitch_maintenance process_number +12125551212"

4) Some modules have comments or do multiple exports so they don't auto-complete properly.  If you find one fix it :) or let us know and we will correct it, thanks!

Happy tab completing!
