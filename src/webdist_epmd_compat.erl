-module(webdist_epmd_compat).

-export([start_link/0]).
-export([port_please/3]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_info/2, handle_cast/2, terminate/2, code_change/3]).


start_link() ->
    % We have to use 'erl_epmd' name because all calls go through that module
    % which relies on process with this name
    gen_server:start_link({local, erl_epmd}, ?MODULE, compat, []).

port_please(Node, HostName, Timeout) ->
    erl_epmd:port_please(Node, HostName, Timeout).

init(compat) ->
    %% Compatibility mode: proxy all calls to vanilla erl_empd module
    error_logger:info_msg("EPMD compat init~n", []),
    erl_epmd:init([]).

handle_call(Call, From, State) ->
    error_logger:info_msg("EPMD Call ~120p~n", [Call]),
    erl_epmd:handle_call(Call, From, State).

handle_info(Info, State) ->
    error_logger:info_msg("EPMD Info ~120p~n", [Info]),
    erl_epmd:handle_info(Info, State).

handle_cast(Cast, State) ->
    error_logger:info_msg("EPMD Cast ~120p~n", [Cast]),
    erl_epmd:handle_cast(Cast, State).


terminate(Reason, State) ->
    erl_epmd:terminate(Reason, State).

code_change(OldVsn, State, Extra) ->
    erl_epmd:code_change(OldVsn, State, Extra).
