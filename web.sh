. /home/dmitriid/erlang_releases/r15b01/activate
nice -n 19 erl -heart -pa ebin apps/*/ebin deps/*/ebin site/ebin -name hint@127.0.0.1 -boot start_sasl -run make all load -s saloon_app -s hint_search -config app.config
