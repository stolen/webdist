-module(webdist_epmd).

-export([mode/0, start_link/0]).

-export([port_please/2, port_please/3]).


mode() ->
    application:get_env(webdist, mode, compat).

module() ->
    module(mode()).

module(compat) -> webdist_epmd_compat;
module(router) -> webdist_epmd_router.

start_link() ->
    application:load(webdist),
    Mode = mode(),
    error_logger:info_msg("EPMD start_link mode: ~w~n", [Mode]),
    (module()):start_link().



port_please(Node, HostName) ->
    port_please(Node, HostName, infinity).

port_please(Node, HostName, Timeout) ->
    error_logger:info_msg("EPMD port_please: ~120p @ ~120p~n", [Node, HostName]),
    (module()):port_please(Node, HostName, Timeout).
