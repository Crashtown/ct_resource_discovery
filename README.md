ct_resource_discovery
=====

This OTP application is responsible for resource discovery inside cluster of Erlang/OTP nodes. This app starts ct_resource_discovery gen_server which provide api for definition of "Local Resources" and "Target Types". 2 and more nodes can exchange and cache remote resources by calling `trade_resources/0`

Build
-----

    $ rebar3 compile
