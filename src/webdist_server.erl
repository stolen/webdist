-module(webdist_server).

-export([start/0]).

% cowboy callbacks
-export([init/3, handle/2, terminate/3]).
-export([upgrade/4]).

start() ->
    webdist_lib:load(),
    application:ensure_all_started(cowboy),
    cowboy:start_http(?MODULE, 5, [{ip, {0,0,0,0,0,0,0,0}}, {port, webdist_lib:conf(port)}], [{env, [{dispatch, cowboy_routes()}]}]).


cowboy_routes() ->
    NodePath = {"/erl/:name", ?MODULE, node},
    DiscPath = {"/erl", ?MODULE, discovery},
    cowboy_router:compile([{'_', [NodePath, DiscPath]}]).




init(_Type, _Req0, node) ->
%    {Host, Req1} = cowboy_req:host(Req0),
%    {Name, Req2} = cowboy_req:binding(name, Req1),
    {upgrade, protocol, ?MODULE}.

handle(Req, State) ->
    {ok, Req2} = cowboy_req:reply(500, [], <<>>, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.



upgrade(Req0, _Env, ?MODULE, node) ->
    {HostBin, Req1} = cowboy_req:host(Req0),
    Host = binary_to_list(HostBin),
    {Name, Req2} = cowboy_req:binding(name, Req1),
    % Discover target node port
    case erl_epmd:port_please(binary_to_list(Name), Host) of
        {port, Port, Version} ->
            {ok, ErlSocket} = gen_tcp:connect(Host, Port, [{packet, raw}, binary, {active, once}, {nodelay, true}]),
            confirm_and_loop(Req2, Version, ErlSocket);
        noport ->
            {ok, Req3} = cowboy_req:reply(404, [], <<>>, Req2),
            {halt, Req3};
        {error, Error} ->
            {ok, Req3} = cowboy_req:reply(500, [], io_lib:format("Error: ~120p~n", [Error]), Req2),
            {halt, Req3}
    end.

confirm_and_loop(Req2, Version, ErlSocket) ->
    % Confirm upgrade
    {ok, Req3} = cowboy_req:upgrade_reply(101, [{<<"upgrade">>, <<"erlang-distribution">>}, {<<"version">>, integer_to_binary(Version)}], Req2),
    % Ensure upgrade is sent and do accept expected message
    receive {cowboy_req, resp_sent} -> ok after 1000 -> erlang:error(no_response_ack) end,
    % Downgrade to low-level
    [Socket, Transport] = cowboy_req:get([socket, transport], Req2),
    ok = Transport:setopts(Socket, [{packet, raw}, binary, {active, once}, {nodelay, true}]),
    proxy_loop(Transport, Socket, ErlSocket),
    {halt, Req3}.

proxy_loop(TransportA, SockA, SockB) ->
    receive
        {_, SockA, DataA} when is_binary(DataA) ->
            ok = gen_tcp:send(SockB, DataA),
            TransportA:setopts(SockA, [{active, once}]),
            proxy_loop(TransportA, SockA, SockB);
        {_, SockB, DataB} when is_binary(DataB) ->
            TransportA:send(SockA, DataB),
            inet:setopts(SockB, [{active, once}]),
            proxy_loop(TransportA, SockA, SockB);
        {tcp_closed, SockA} ->
            gen_tcp:close(SockB);
        {tcp_closed, SockB} ->
            TransportA:close(SockA);
        Other ->
            error({unexpected_message, Other})
    end.

