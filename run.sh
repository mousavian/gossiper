export ERL_CRASH_DUMP=/dev/null
echo "-- compiling --"
rebar3 clean && rm _build/default/lib/gossiper -rf 2>/dev/null && rebar3 compile

echo "-- running --"
# erl -env ERL_LIBS _build/default/lib -eval 'gossiper:start().' -noshell
erl -env ERL_LIBS _build/default/lib -eval 'application:ensure_all_started(gossiper).' -noshell

# production
#./bin/gossip foreground