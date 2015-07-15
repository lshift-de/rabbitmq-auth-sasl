Development & Testing
=====================

You should be testing under [umbrella](https://github.com/rabbitmq/rabbitmq-public-umbrella) environment.
To prepare working environment execute below steps:

~~~bash
git clone git@github.com:rabbitmq/rabbitmq-public-umbrella.git;
cd rabbitmq-public-umbrella; make up;

git clone git@github.com:lshift-de/rabbitmq-auth-sasl.git;
cd rabbitmq-auth-sasl; make test;
~~~

#### Generate client / server keys and certs

Follow [official guide](https://www.rabbitmq.com/ssl.html) how to do this.

#### Configure server

Go to `rabbitmq-public-umbrella/rabbitmq-server` and define SSL config - provide proper paths to given files:

~~~bash
...
{ssl_listeners, [5671]},
{ssl_options, [{cacertfile,"testca/cacert.pem"},
               {certfile,"server/cert.pem"},
               {keyfile,"server/key.pem"},
               {depth, 2},
               {verify,verify_peer},
               {fail_if_no_peer_cert,false}
              ]
}
...
~~~

Update auth mechanisms and backends:

~~~bash
...
{auth_mechanisms, ['PLAIN', 'AMQPLAIN', 'EXTERNAL']},
{auth_backends, [rabbitmq_auth_sasl_backend]}
...
~~~

Change all these params in `ebin/rabbit_app.in`.

#### Start server with plugin:

~~~bash
cd rabbitmq-public-umbrella/rabbitmq-auth-sasl;
make run-in-broker;
~~~

#### Configure client and connect to server:

Start shell with loaded erlang rabbitmq client code:

~~~bash
cd rabbitmq-public-umbrella/rabbitmq-erlang-client; make;
erl -pa ebin -pa include -pa deps/rabbit_common-0.0.0/ebin -pa deps/rabbit_common-0.0.0/include
~~~

Initialize client records:

~~~erlang
rr("include/amqp_client.hrl").
~~~

Define custom / external auth method:

~~~erlang
External = fun(<<"Token required">>, _, State) -> {<<"Token">>, State};
              (none, _, init) -> {<<"EXTERNAL">>, []};
              (none, _, _State) -> {<<"">>, _State}
           end.
~~~

Define SSL options - provide proper paths to given files:

~~~erlang
SSLOptions = [
    {cacertfile, "testca/cacert.pem"},
    {certfile, "client/cert.pem"},
    {keyfile, "client/key.pem"},
    {verify, verify_peer},
    {fail_if_no_peer_cert, true}
].
~~~

Define connection params:

~~~erlang
ConnectionParams = #amqp_params_network{port = 5671, auth_mechanisms = [External], ssl_options=SSLOptions}.
~~~

Open connection:

~~~erlang
amqp_connection:start(ConnectionParams).
~~~