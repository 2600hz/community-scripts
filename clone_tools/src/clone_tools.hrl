%% ===========[ MODIFY ]====================
%% TARGET should be set to the URL of haproxy
%% for the new database cluster.
-define(TARGET, "http://127.0.0.1:15984/").

%% SOURCE should be set to a single node
%% in the old database cluster.
-define(SOURCE, "http://127.0.0.1:5984/").

%% MAX_CR_AGE is the maxium age (in days)
%% of the CDRs to copy (IE: last 30 days).
%% Set to 0 to copy all or 'none'.
-define(MAX_CR_AGE, 'none').

%% MAX_VM_AGE is the maxium age (in days)
%% of the voicemail in a box to copy.
%% Set to 0 to copy all or 'none'.
-define(MAX_VM_AGE, 0).

%% DEAD_ACCOUNT_IDS is the listing of account
%% IDs that are no longer existent but appear
%% in an account's pvt_tree. This will strip
%% the dead account IDs from existing accounts
%% -define(DEAD_ACCOUNT_IDS, [<<"abc1234...">>,...]
-define(DEAD_ACCOUNT_IDS, []).

%% =========================================

-define(WNM_DB_PREFIX, <<"numbers/">>).
-define(LOG_PATH, "/tmp/").

-define(BLACK, io:format("\e[30m", [])).
-define(RED, io:format("\e[31m", [])).
-define(GREEN, io:format("\e[32m", [])).
-define(YELLOW, io:format("\e[33m", [])).
-define(BLUE, io:format("\e[34m", [])).
-define(MAGENTA, io:format("\e[35m", [])).
-define(CYAN, io:format("\e[36m", [])).
-define(WHITE, io:format("\e[37m", [])).

-define(LOG(C, F, A), fun(Control, Format, Args) ->
                      Path = ?LOG_PATH ++ atom_to_list(?MODULE),
                      file:write_file(Path, io_lib:format(Format, Args), ['append']),
                      io:format(Control ++ Format, Args)
                      end(C, F, A)).
-define(LOG(F, A), ?LOG_WHITE(F, A)).
-define(LOG_BLACK(F, A), ?LOG("\e[30m", F, A)).
-define(LOG_RED(F, A), ?LOG("\e[31m", F, A)).
-define(LOG_GREEN(F, A), ?LOG("\e[32m", F, A)).
-define(LOG_YELLOW(F, A), ?LOG("\e[33m", F, A)).
-define(LOG_BLUE(F, A), ?LOG("\e[34m", F, A)).
-define(LOG_MAGENTA(F, A), ?LOG("\e[35m", F, A)).
-define(LOG_CYAN(F, A), ?LOG("\e[36m", F, A)).
-define(LOG_WHITE(F, A), ?LOG("\e[37m", F, A)).
