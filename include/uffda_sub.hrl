-type sub_type() :: pid | sse.
-type address() :: pid() | string().
-type state() :: {address(), service_name()}.

-type event() :: {publish, service_name(), service_status()}.

-type call() :: report_subscriber.
-define(PUBLISH_MGR, uffda_publish_manager).
-type sse_address() :: string().
