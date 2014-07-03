-type service_name()    :: atom().
-type service_pid()     :: pid().
-type service_event()   :: unregister | starting | online | offline | reset.
-type service_status()  :: not_registered | registered | starting_up | up | down | crashed.

-type service_fsm()     :: atom().
-type service_fsm_pid() :: pid().
