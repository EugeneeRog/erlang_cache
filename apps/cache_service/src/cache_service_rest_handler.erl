-module(cache_service_rest_handler).

-behaviour(trails_handler).

-ignore_xref([
  {?MODULE, content_types_accepted, 2},
  {?MODULE, content_types_provided, 2},
  {?MODULE, allowed_methods, 2},
  {?MODULE, delete_resource, 2},
  {?MODULE, ets_get, 2},
  {?MODULE, ets_insert, 2},
  {?MODULE, init, 2}]).

-define(CACHE_ETS, default_cache).
-define(ALLOWED_METHODS, [<<"GET">>, <<"POST">>, <<"DELETE">>]).

-export([
  trails/0,
  init/2,
  allowed_methods/2,
  content_types_provided/2,
  content_types_accepted/2,
  delete_resource/2,
  ets_get/2,
  ets_insert/2]).

init(Req, State) ->
  {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
  {?ALLOWED_METHODS, Req, State}.

content_types_provided(Req, State) ->
  {[{<<"application/x-www-form-urlencoded">>, ets_get}], Req, State}.

ets_get(Req, State) ->
  case cowboy_req:read_urlencoded_body(Req) of
    {ok, [{<<"key">>, Key}], _} ->
      EtsRes = ets:lookup(?CACHE_ETS, Key),
      case EtsRes of
          [{Key, Value}] ->
              {<<"{", Key/binary, ":", Value/binary, "}">>, Req, State};
          [] ->
            {"", cowboy_req:stream_reply(404, Req), State}
      end;
    _ ->
      ReqWithBody = cowboy_req:set_resp_body(<<"{error: Bad Request}">>, Req),
      FinalReq = cowboy_req:reply(400, ReqWithBody),
      {true, FinalReq, State}
  end.

delete_resource(Req, State) ->
  case cowboy_req:read_urlencoded_body(Req) of
    {ok, [{<<"key">>, Key}], _} ->
      EtsRes = ets:lookup(?CACHE_ETS, Key),
      case EtsRes of
        [{Key, _}] ->
          Res = ets:delete(?CACHE_ETS, Key),
          {Res, Req, State};
        [] ->
          {true, cowboy_req:reply(404, Req), State}
      end;
      _ ->
        ReqWithBody = cowboy_req:set_resp_body(<<"{error: Bad Request}">>, Req),
        FinalReq = cowboy_req:reply(400, ReqWithBody),
        {true, FinalReq, State}
  end.

content_types_accepted(Req, State) ->
  {[{<<"application/x-www-form-urlencoded">>, ets_insert}], Req, State}.

ets_insert(Req, State) ->
  case cowboy_req:read_urlencoded_body(Req) of
    {ok, [{<<"key">>, Key}, {<<"value">>, Value}], _} ->
      EtsObject = {Key, Value},
      true = ets:insert(?CACHE_ETS, EtsObject),
      {true, Req, State};
    _ ->
      ReqWithBody = cowboy_req:set_resp_body(<<"{error: Bad Request}">>, Req),
      FinalReq = cowboy_req:reply(400, ReqWithBody),
      {true, FinalReq, State}
end.

trails() ->
  MsgTrailsMetadata =
    #{post => #{
        tags => ["cache_service_rest"],
        desc => "added and update the cache",
        parameters => [
          #{name => <<"key">>,
              description => <<"key for search">>,
              required => true,
              type => <<"string">>  },
          #{name => <<"value">>,
            description => <<"key for search">>,
            required => true,
            type => <<"string">>  }]},
      delete => #{
        tags => ["cache_service_rest"],
        desc => "delete cache",
        parameters => [
          #{name => <<"key">>,
            description => <<"key for delete">>,
            required => true,
            type => <<"string">>}]},
      get => #{
        tags => ["cache_service_rest"],
        'content-type' => "application/json",
        description => "get params from the cache",
        produces => ["application/json", "text/plain"],
        parameters => [
          #{name => <<"key">>,
            description => <<"key for search">>,
            required => true,
            type => <<"string">>  }]}
    },
  [trails:trail("/cache", ?MODULE, [], MsgTrailsMetadata)].