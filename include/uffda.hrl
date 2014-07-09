-type service_name()    :: atom().
-type service_pid()     :: pid().
-type service_fsm()     :: atom().
-type service_fsm_pid() :: pid().

-record(service_options, {
    stimeout :: integer()
    }).

-type service_options() :: #service_options{}.

-type service_event()   :: unregister
                         | {starting, service_pid()}
                         | {re_init,  service_pid()}
                         | online
                         | offline
                         | current_status.

-type service_status()  :: not_registered
                         | registered
                         | starting_up
                         | restarting
                         | slow_start
                         | slow_restart
                         | crashed
                         | down
                         | up.

-define(MAX_STARTUP_TIME, 300).
