-module(webdist_epmd).

-export([start_link/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_info/2, handle_cast/2, terminate/2, code_change/3]).


start_link() ->
    application:load(webdist),
    Mode = application:get_env(webdist, mode, compat),
    error_logger:info_msg("EPMD start_link mode: ~w~n", [Mode]),
    % We have to use 'erl_epmd' name because all calls go through that module
    % which relies on process with this name
    gen_server:start_link({local, erl_epmd}, ?MODULE, Mode, []).


init(compat) ->
    %% Compatibility mode: proxy all calls to vanilla erl_empd module
    error_logger:info_msg("EPMD init~n", []),
    {ok, State} = erl_epmd:init(none),
    {ok, {compat, State}}.


handle_call(Call, From, {compat, State}) ->
    error_logger:info_msg("EPMD Call ~120p~n", [Call]),
    case erl_epmd:handle_call(Call, From, State) of
        {reply, Reply, State1} ->
            {reply, Reply, {compat, State1}};
        {stop, Reason, Reply, State1} ->
            {stop, Reason, Reply, {compat, State1}}
    end.
%handle_call({register, Name, Port}, _From, State) ->
%    io:format("EPMD Register ~w at ~w~n", [Name, Port]),
%    {reply, {ok, 1}, State}.

handle_info(Info, {compat, State}) ->
    error_logger:info_msg("EPMD Info ~120p~n", [Info]),
    case erl_epmd:handle_info(Info, State) of
        {noreply, State1} ->
            {noreply, {compat, State1}};
        {stop, Reason, State1} ->
            {stop, Reason, {compat, State1}}
    end.

handle_cast(Cast, {compat, State}) ->
    error_logger:info_msg("EPMD Cast ~120p~n", [Cast]),
    case erl_epmd:handle_cast(Cast, State) of
        {noreply, State1} ->
            {noreply, {compat, State1}};
        {stop, Reason, State1} ->
            {stop, Reason, {compat, State1}}
    end.


terminate(Reason, {compat, State}) ->
    erl_epmd:terminate(Reason, State).

code_change(OldVsn, {compat, State}, Extra) ->
    erl_epmd:code_change(OldVsn, State, Extra).
