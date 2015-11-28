-module(webdist_epmd).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_info/2]).


start_link() ->
    gen_server:start_link(
        { local, erl_epmd }, ?MODULE, epmd, []).


init(epmd) ->
    erl_epmd:init(none).


handle_call(Call, From, State) ->
    error_logger:info_msg("EPMD Call ~120p~n", [Call]),
    erl_epmd:handle_call(Call, From, State).
%handle_call({register, Name, Port}, _From, State) ->
%    io:format("EPMD Register ~w at ~w~n", [Name, Port]),
%    {reply, {ok, 1}, State}.

handle_info(Info, State) ->
    erl_epmd:handle_info(Info, State).
