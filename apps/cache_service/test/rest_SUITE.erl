-module(rest_SUITE).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

% tests
-export([check_get_success/1,
  check_post_success/1,
  check_delete_success/1,
  check_get_undef/1,
  check_delete_undef/1,
  check_bad_key_post/1,
  check_bad_key_delete/1,
  check_bad_key_get/1]).

-include_lib("common_test/include/ct.hrl").

-define(RQST_URL, "/cache").
-define(POST_RQST_PARAMS, <<"key=example&value=exampleData">>).
-define(FAIL_RQST_PARAMS, <<"fail=example">>).

-define(TEST_RQST_HEADERS, [{<<"content-type">>, <<"application/x-www-form-urlencoded">>}]).

-define(POST_EX_RESULT, [{<<"example">>, <<"exampleData">>}]).
-define(EXEPTED_GET_ETS, <<"{example:exampleData}">>).
%% --------------------------------

all() -> [
  check_post_success,
  check_get_success,
  check_delete_success,
  check_get_undef,
  check_delete_undef,
  check_bad_key_post,
  check_bad_key_delete,
  check_bad_key_get
].

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(cache_service),
  {ok, _} = application:ensure_all_started(gun),
  Config.

end_per_suite(Config) ->
  ok = application:stop(gun),
  ok = application:stop(cache_service),
  Config.

init_per_testcase(_TestCase, Config) ->
  Config.

end_per_testcase(_TestCase, Config) ->
  Config.

% -----------------------------------------

check_post_success(Config) ->
  ConnPid = prep_gun(),
  StreamRef = gun:post(ConnPid, ?RQST_URL, ?TEST_RQST_HEADERS, ?POST_RQST_PARAMS),
  {response, _, 204, _} = gun:await(ConnPid, StreamRef),
  ?POST_EX_RESULT = ets:lookup(default_cache, <<"example">>),
  Config.

check_get_success(Config) ->
  ConnPid = prep_gun(),
  StreamRef = gun:request(ConnPid, <<"GET">>, ?RQST_URL, ?TEST_RQST_HEADERS, <<"key=example">>),
  {response, _, 200, _} = gun:await(ConnPid, StreamRef),
  {ok,?EXEPTED_GET_ETS} = gun:await_body(ConnPid, StreamRef),
  Config.

check_get_undef(Config) ->
  ConnPid = prep_gun(),
  StreamRef = gun:request(ConnPid, <<"GET">>, ?RQST_URL, ?TEST_RQST_HEADERS, <<"key=example">>),
  {response, _, 404, _} = gun:await(ConnPid, StreamRef),
  {ok, <<"">>} = gun:await_body(ConnPid, StreamRef),
  Config.

check_delete_success(Config) ->
  ConnPid = prep_gun(),
  StreamRef = gun:request(ConnPid, <<"DELETE">>, ?RQST_URL, ?TEST_RQST_HEADERS, <<"key=example">>),
  {response, _, 204, _} = gun:await(ConnPid, StreamRef),
  [] = ets:lookup(default_cache, <<"example">>),
  Config.

check_delete_undef(Config) ->
  ConnPid = prep_gun(),
  StreamRef = gun:request(ConnPid, <<"DELETE">>, ?RQST_URL, ?TEST_RQST_HEADERS, <<"key=example">>),
  {response, _, 404, _} = gun:await(ConnPid, StreamRef),
  [] = ets:lookup(default_cache, <<"example">>),
  Config.


check_bad_key_post(Config) ->
  ConnPid = prep_gun(),
  StreamPostRef = gun:post(ConnPid, ?RQST_URL, ?TEST_RQST_HEADERS),
  ok = gun:data(ConnPid, StreamPostRef, fin, ?FAIL_RQST_PARAMS),
  {response, _, 400, _} = gun:await(ConnPid, StreamPostRef),
  Config.

check_bad_key_get(Config) ->
  ConnPid = prep_gun(),
  StreamGetRef = gun:request(ConnPid, <<"GET">>, ?RQST_URL, ?TEST_RQST_HEADERS, <<"fail=example">>),
  {response, _, 400, _} = gun:await(ConnPid, StreamGetRef),
  Config.

check_bad_key_delete(Config) ->
  ConnPid = prep_gun(),
  StreamDelRef = gun:request(ConnPid, <<"DELETE">>, ?RQST_URL, ?TEST_RQST_HEADERS, <<"fail=example">>),
  {response, _, 400, _} = gun:await(ConnPid, StreamDelRef),
  Config.

prep_gun() ->
  {ok, Port} = application:get_env(cache_service, http_port),
  {ok, ConnPid} = gun:open("localhost", Port),
  {ok, _Protocol} = gun:await_up(ConnPid),
  ConnPid.
