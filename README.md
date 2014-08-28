uffda [![Build Status](https://travis-ci.org/tigertext/uffda.svg)](https://travis-ci.org/tigertext/uffda)
=======================================================================================================

An Erlang Service Registry used to track the availability and status of
services conforming to its interface.  From Wikipedia, "Uff da is often used in
the Pacific Northwest and Upper Midwest as a term for sensory overload. It can
be used as an expression of surprise, astonishment, exhaustion, relief and
sometimes dismay. For many, Uff da is an all-purpose expression with a variety
of nuances, and covering a variety of situations." The name was chosen to
reflect the feelings that may result from service outages, and hopefully this
project will provide insight to better track outages and presence of services.

Testing
-------
In order to run tests, clone and hit make:

    $ git clone https://github.com/tigertext/uffda
    $ cd uffda
    $ make tests

To check that the types are properly specified, make the plt and run dialyzer:

    $ make plt
    $ make dialyze

Travis CI
-----------
Builds are also enabled through travis-ci.org

Publish & Subscribe
-------------------
Uffda has support for subscribing to particular services and receiving
notifications over several mediums when state changes for those services. To
subscribe to an SSE stream, hit the endpoint `/subscribe/:service_name` with an
HTTP request.

[![Imgur](http://i.imgur.com/VR2al8I.png)](http://i.imgur.com/VR2al8I)

The erlang shell commands emulate the behavior of a service making those calls
to uffda, and the SSE stream reports on the changes in subscription or of the
subscribed to service. The current format for SSE is that the `event` value is
a member of `{subscription_starting, updating, unsubscribing}`, `id`'s value is
the service name, and `data` encodes the most recent state of the service. 

The syndication is not limited to SSE: currently there is also support for SMS,
email, sending to another process, in addition to being extensible. There is a
behaviour in place that dictates how to create a sender, which expects a `send`
callback with the following signature.

    send(atom(), #ass{address :: pid() | string(),
                      service :: atom(),
                      status :: atom()) -> ok.
    
Such a callback is expected to be defined with the behaviour `uffda_publisher`
declared. An example for sending emails with the unix `mail` command is shown below:

[![Imgur](http://i.imgur.com/JoeRXub.png)](http://imgur.com/JoeLXub)

There is also a need to edit the `find_vars` function in `src/pubsub/uffda_subscription.erl`
to accomodate further behaviours.
Further ideas for creating senders would be

+ TigerText
+ Sensu
+ Filtering on the event type, e.g. sending only on a timeout or a crash.

Additional room for improvement would be

+ reading a list of subscribers from a static file:

        {email, "tholland@tigertext.com", baz}.
        {sms, "9178576309", baz}.
        {sms, "5559991234", bar}.

+ Subscriptions added/revoked over HTTP
