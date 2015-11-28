-module(webdist_dist).

-define(dist_trace, true).

-include_lib("kernel/include/net_address.hrl").
-include_lib("kernel/include/dist.hrl").
-include_lib("kernel/include/dist_util.hrl").

-export([listen/1, accept/1, close/1, select/1, setup/5]).% , accept_connection/5,
         %setup/5, is_node_name/1]).

-export([accept_loop/2, do_setup/6, tick/1]).
-export([setopts_pre_nodeup/1, setopts_post_nodeup/1]).

listen(_Name) ->
    webdist_lib:load(),
    case inet6_tcp:listen(0, [{active, false}, {packet,2}]) of
        {ok, Socket} ->
            TcpAddress = get_tcp_address(Socket),
	    Creation = 1,
	    {ok, {Socket, TcpAddress, Creation}};
        Error ->
            Error
    end.

get_tcp_address(Socket) ->
    {ok, Address} = inet:sockname(Socket),
    {ok, Host} = inet:gethostname(),
    #net_address {
                  address = Address,
                  host = Host,
                  protocol = tcp,
                  family = inet6
                 }.

accept(Listen) ->
    spawn_opt(?MODULE, accept_loop, [self(), Listen], [link, {priority, max}]).

accept_loop(Kernel, Listen) ->
    case inet6_tcp:accept(Listen) of
        {ok, Socket} ->
            error_logger:info("Dist: Accepted ~w~n", [Socket]),
            %Kernel ! {accept,self(),Socket,inet6,tcp},
            accept_loop(Kernel, Listen);
        Error ->
            exit(Error)
    end.


close(Socket) ->
    inet6_tcp:close(Socket).


select(_) -> true.


setup(Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    spawn_link(?MODULE, do_setup, [self(), Node, Type, MyNode, LongOrShortNames, SetupTime]).


do_setup(Kernel, Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    ?trace("~p~n",[{?MODULE, self(), setup, Node, Type, MyNode, LongOrShortNames,SetupTime}]),
    [Name, Address] = splitnode(Node),
    Port = webdist_lib:conf(port),
    Timer = dist_util:start_timer(SetupTime),
    case gen_tcp:connect(Address, Port, [binary, {active, false}], SetupTime) of
        {ok, Socket} ->
            http_setup(Kernel, Socket, Node, Name, Address, Type, MyNode, Timer);
        __Other ->
            ?trace("connect to ~120p failed: ~120p~n", [Node, __Other]),
            ?shutdown(Node)
    end.


http_setup(Kernel, Socket, Node, Name, Address, Type, MyNode, Timer) ->
    % Send request
    gen_tcp:send(Socket, http_req(Name, Address)),

    % Receive expected response line...
    ok = inet:setopts(Socket, [{packet, http_bin}]),
    {ok, {http_response, {1,1}, 101, _}} = gen_tcp:recv(Socket, 0, 5000),
    % ... and headers
    {ok, Version} = recv_headers(Socket, undefined),
    ok = inet:setopts(Socket, [{active, false}, {packet, 2}, {mode, list}]),
    HSData = make_hsdata(Kernel, Node, Type, MyNode, Socket, make_netaddr(Address, Socket), Version, Timer),
    dist_util:handshake_we_started(HSData).

http_req(Name, Address) ->
    [<<"CONNECT /erl/">>, Name, <<" HTTP/1.1\r\n">>,
     <<"Host: ">>, Address, <<"\r\n">>,
     <<"Connection: Upgrade\r\n">>,
     <<"Upgrade: erlang-distribution\r\n">>,
     <<"\r\n">>].

recv_headers(Socket, Version) ->
    ok = inet:setopts(Socket, [{packet, httph_bin}]),
    {ok, Resp} = gen_tcp:recv(Socket, 0, 500),
    case Resp of
        {http_header, _, <<"Version">>, _, BinVersion} ->
            ?trace("WebDist version header: ~s~n", [BinVersion]),
            recv_headers(Socket, binary_to_integer(BinVersion));
        {http_header, _, _, _, _} ->
            recv_headers(Socket, Version);
        http_eoh ->
            {ok, Version}
    end.

splitnode(Node) ->
    case string:tokens(atom_to_list(Node), "@") of
        [Name, Address] ->
            [Name, Address];
        _ ->
            error_logger:error_msg("** Nodename ~p illegal **~n", [Node]),
            ?shutdown(Node)
    end.


make_netaddr(Address, Socket) ->
    {ok, {Ip, TcpPort}} =  inet:peername(Socket),
    #net_address {
        address = {Ip,TcpPort},
        host = Address,
        protocol = tcp,
        family = family(Ip)}.

family({_,_,_,_}) -> inet;
family({_,_,_,_,_,_,_,_}) -> inet6.

make_hsdata(Kernel, Node, Type, MyNode, Socket, NetAddr, Version, Timer) ->
    #hs_data{
        kernel_pid = Kernel,
        other_node = Node,
        this_node = MyNode,
        socket = Socket,
        timer = Timer,
        this_flags = 0,
        other_version = Version,
        f_send = fun inet6_tcp:send/2,
        f_recv = fun inet6_tcp:recv/3,
        f_setopts_pre_nodeup = fun ?MODULE:setopts_pre_nodeup/1,
        f_setopts_post_nodeup = fun ?MODULE:setopts_post_nodeup/1,
        f_getll = fun inet:getll/1,
        f_address = fun(_,_) -> NetAddr end,
        mf_tick = fun ?MODULE:tick/1,
        mf_getstat = fun inet_tcp_dist:getstat/1,
        request_type = Type
        }.


tick(Socket) ->
    case prim_inet:send(Socket, [], [force]) of
        {error, closed} ->
            self() ! {tcp_closed, Socket},
            {error, closed};
        R ->
            R
    end.

%% we may not always want the nodelay behaviour
%% for performance reasons

nodelay() ->
    case application:get_env(kernel, dist_nodelay) of
        undefined ->
            {nodelay, true};
        {ok, true} ->
            {nodelay, true};
        {ok, false} ->
            {nodelay, false};
        _ ->
            {nodelay, true}
    end.

setopts_pre_nodeup(S) ->
    inet:setopts(S, [{active, false}, {packet, 4}, nodelay()]).

setopts_post_nodeup(S) ->
    inet:setopts(S, [{active, true}, {deliver, port}, {packet, 4}, nodelay()]).
