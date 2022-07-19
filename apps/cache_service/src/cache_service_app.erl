-module(cache_service_app).
-behaviour(application).

-ignore_xref([
    {?MODULE, start_phase, 3},
    {?MODULE, start, 0},
    {?MODULE, stop, 0}
    ]).

-export([start/0,
    start/2,
    stop/0,
    stop/1,
    start_phase/3]).

start() ->
    application:ensure_all_started(cache_service).

stop() ->
    application:stop(cache_service).

start(_StartType, _StartArgs) ->
    cache_service_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(cache_service_http).

start_phase(start_trails_http, _StartType, []) ->
    default_cache = ets:new(default_cache, [named_table, public]),
    {ok, Port} = application:get_env(cache_service, http_port),
    {ok, ListenerCount} = application:get_env(cache_service, http_listener_count),
    Handlers = [cache_service_rest_handler, cowboy_swagger_handler],
    Trails = trails:trails(Handlers),
    trails:store(Trails),
    Dispatch = trails:single_host_compile(Trails),
    CowboyOptions = #{env => #{dispatch => Dispatch}, request_timeout => 12000},
    {ok, _} =
        cowboy:start_clear(cache_http,
            #{socket_opts => [{port, Port}], num_acceptors => ListenerCount},
            CowboyOptions),
    ok.