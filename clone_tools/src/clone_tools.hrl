
-define(TARGET, "http://127.0.0.1:15984/").
-define(SOURCE, "http://127.0.0.1:5984/").

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
