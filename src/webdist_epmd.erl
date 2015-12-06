-module(webdist_epmd).

-export([mode/0, start_link/0]).


mode() ->
    application:get_env(webdist, mode, compat).

start_link() ->
    application:load(webdist),
    Mode = mode(),
    error_logger:info_msg("EPMD start_link mode: ~w~n", [Mode]),
    start_link(Mode).


start_link(compat) ->
    webdist_epmd_compat:start_link();

start_link(router) ->
    webdist_epmd_router:start_link().
