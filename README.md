uffda
=====

Erlang Service Registry

Testing
-------
In order to run it locally, it should be eneough to build the Persistant Lookup Table and
then run the tests:

    $ git clone https://github.com/th0114nd/uffda
    $ cd uffda
    $ make build-plt && make tests

To check that the types are properly specified, run 

    $ make dialyze
