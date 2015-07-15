include ../umbrella.mk

test:
	erl -noshell -pa ebin -pa build/dep-apps/*/ebin -pa ../rabbitmq-server/ebin -eval "eunit:test([rabbitmq_auth_sasl_token, rabbitmq_auth_sasl_backend], [verbose])" -s init stop
