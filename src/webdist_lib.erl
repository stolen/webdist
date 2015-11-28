-module(webdist_lib).

-export([load/0]).
-export([conf/1]).

load() ->
    application:load(webdist).

conf(Key) ->
    case application:get_env(webdist, Key) of
        undefined -> error({no_key, Key});
        {ok, Value} -> Value
    end.
