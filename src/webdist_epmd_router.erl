-module(webdist_epmd_router).

-export([start_link/0]).
-export([port_please/3]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_info/2, handle_cast/2, terminate/2, code_change/3]).


start_link() ->
    % We have to use 'erl_epmd' name because all calls go through that module
    % which relies on process with this name
    gen_server:start_link({local, erl_epmd}, ?MODULE, router, []).

port_please(Node, HostName, Timeout) ->
    gen_server:call(erl_epmd, {port_please, Node, HostName, Timeout}, Timeout).

init(router) ->
    State = #{
	    my_nodehost => undefined,
	    my_name => undefined,
	    my_port => undefined
	    },
    {ok, {router, State}}.

handle_info(_, {router, State}) ->
    {noreply, {router, State}}.

handle_cast(_, {router, State}) ->
    {noreply, {router, State}}.


handle_call({register, Name, PortNo}, _From, {router, #{my_name := undefined} = State}) ->
    NewState = State#{my_name := Name, my_port := PortNo},
    {reply, {ok, 1}, {router, NewState}};

handle_call({register, _, _}, _From, {router, State}) ->
    {reply, {error, already_registered}, {router, State}};

%% We don't know own name. Try to get it, then continue lookup
handle_call({port_please, Node, Host, Timeout}, From, {router, #{my_nodehost := undefined} = State}) ->
    case node() of
	nonode@nohost ->
	    {reply, get_port(Node, Host, Timeout, State), {router, State}};
	RealNode ->
	    NewState = State#{my_nodehost := nodehost(RealNode)},
	    handle_call({port_please, Node, Host, Timeout}, From, {router, NewState})
    end;

%% port_please for our own node. Return 'self' shortcut
handle_call({port_please, Node, Host, _}, _From, {router, #{my_nodehost := {Node, Host}} = State}) ->
    %% TODO: find a way to get our ?epmd_dist_high and ?epmd_dist_low (supported dist version range)
    {reply, {self, 5}, {router, State}};

handle_call({port_please, Node, Host, Timeout}, _From, {router, State}) ->
    {reply, get_port(Node, Host, Timeout, State), {router, State}};

handle_call(_, _From, {router, State}) ->
    {reply, {error, not_implemented}, {router, State}}.


code_change(_OldVsn, {router, State}, _Extra) ->
    {ok, {router, State}}.

terminate(_, _) ->
    ok.


nodehost(NodeName) when is_atom(NodeName) ->
    [Node, Host] = string:tokens(atom_to_list(NodeName), "@"),
    {Node, Host}.

get_port(_, _, _, _) ->
    noport.
