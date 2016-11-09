%% (The MIT License)
%%
%% Copyright (c) 2013 Matthias Endler
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the 'Software'), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.

-module(podbc).
-author('Matthias Endler <matthias.endler@pantech.at>').

-define(TIMEOUT, 25000).

-type col_name() :: string().

-type col_names() :: [col_name()].

-type commit_mode() :: commit | rollback.

-type commit_reason() :: not_an_explicit_commit_connection | process_not_owner_of_odbc_connection | common_reason().

-type common_reason() :: connection_closed | term().

-type in_or_out() :: in | out | inout.

-type odbc_data_type() :: sql_integer | sql_smallint | sql_tinyint |
{sql_decimal, precision(), scale()} |
{sql_numeric, precision(), scale()} |
{sql_char, size()} | {sql_wchar, size()} | {sql_varchar, size()} | {sql_wvarchar, size()} | {sql_wlongvarchar, size()} |
{sql_float, precision()} | sql_real | sql_double | sql_bit | atom().

-type n_rows() :: non_neg_integer().

-type params() :: [{odbc_data_type(), [value()]} | {odbc_data_type(), in_or_out(), [value()]}].

-type position() :: next | {relative, integer()} | {absolute, integer()}.

-type precision() :: non_neg_integer().

-type result_reason() :: result_set_does_not_exist | process_not_owner_of_odbc_connection | common_reason().

-type result_tuple() :: {updated, n_rows()} | {selected, col_names(), rows()}.

-type row() :: {value()}.

-type rows() :: [row()].

-type scale() :: non_neg_integer().

-type scroll_reason() :: result_set_does_not_exist | driver_does_not_support_function | scrollable_cursors_disabled |
process_not_owner_of_odbc_connection | common_reason().

-type size() :: pos_integer().

-type value() :: null | term().

-type do_fun(T) :: fun((Worker :: pid()) -> T).

-type worker_ref() :: {Pool :: atom(), WorkerPid :: pid()}.

%% podbc API

-export([do/2, do/3, do/4]).
-export([transaction/2, transaction/3]).
-export([get_pool_name/1]).
-export([named_query/3, named_query/4]).

%% odbc API

-export([commit/2, commit/3]).
-export([connect/1, connect/2, connect/3]).
-export([disconnect/1, disconnect/2]).
-deprecated([{disconnect, 2, next_major_release}]).
-export([describe_table/2, describe_table/3]).
-export([first/1, first/2]).
-export([last/1, last/2]).
-export([next/1, next/2]).
-export([param_query/3, param_query/4]).
-export([prev/1, prev/2]).
-export([start/0, start/1]).
-export([stop/0]).
-export([sql_query/2, sql_query/3]).
-export([select_count/2, select_count/3]).
-export([select/3, select/4]).

%% podbc API

-spec do(Pool :: atom(), Fun :: do_fun(T)) -> {ok, T}.
do(Pool, Fun) ->
  do(Pool, Fun, true).

-spec do(Pool :: atom(), Fun :: do_fun(T), Block :: boolean()) -> {ok, T} | full.
do(Pool, Fun, Block) ->
  do(Pool, Fun, Block, ?TIMEOUT).

-spec do(Pool :: atom(), Fun :: do_fun(T), Block :: boolean(), TimeOut :: timeout()) -> {ok, T} | full.
do(Pool, Fun, Block, TimeOut) ->
  case connect(Pool, Block, TimeOut) of
    {ok, WorkerRef} ->
      try
        {ok, Fun(WorkerRef)}
      after
        ok = disconnect(WorkerRef)
      end;
    Other ->
      Other
  end.

%% This is still experimental and may change
transaction(WorkerRef, Fun) ->
  transaction(WorkerRef, Fun, ?TIMEOUT).

%% This is still experimental and may change
transaction(WorkerRef, Fun, TimeOut) ->
  case Fun(WorkerRef) of
    commit ->
      commit(WorkerRef, commit, TimeOut),
      ok;
    rollback ->
      commit(WorkerRef, rollback, TimeOut),
      ok;
    {commit, Result} ->
      commit(WorkerRef, commit, TimeOut),
      {ok, Result};
    {rollback, Result} ->
      commit(WorkerRef, rollback, TimeOut),
      {ok, Result}
  end.

%% TODO consider, if it's better to extract the info from WorkerRef, than making a call to the server.
-spec get_pool_name(WorkerRef :: worker_ref()) -> {ok, Name :: atom()}.
get_pool_name(WorkerRef) ->
  gen_server:call(worker_pid(WorkerRef), get_pool_name).

-spec named_query(WorkerRef :: worker_ref(), QueryName :: atom(), Params :: [any()]) -> result_tuple() | [result_tuple()] | {error, common_reason()}.
named_query(WorkerRef, QueryName, Params) ->
  named_query(WorkerRef, QueryName, Params, ?TIMEOUT).

-spec named_query(WorkerRef :: worker_ref(), QueryName :: atom(), Params :: [any()], TimeOut :: timeout()) -> result_tuple() | [result_tuple()] | {error, common_reason()}.
named_query(WorkerRef, QueryName, Params, TimeOut) ->
  {ok, PoolName} = get_pool_name(WorkerRef),
  {ok, {Sql, ParamTypes}} = podbc_qresolver:get(PoolName, QueryName),
  ParamsLen = length(Params),
  ParamTypesLen = length(ParamTypes),
  if
    ParamsLen =/= ParamTypesLen ->
      {error, {params_mismatch, [{expected, ParamTypesLen, ParamTypes}, {provided, ParamsLen, Params}]}};
    true ->
      case Params of
        [] ->
          sql_query(WorkerRef, Sql, TimeOut);
        Params ->
          MergeParams = merge_params(ParamTypes, Params, []),
          param_query(WorkerRef, Sql, MergeParams, TimeOut)
      end
  end.

%% odbc API

-spec commit(WorkerRef :: worker_ref(), CommitMode :: commit_mode()) -> ok | {error, commit_reason()}.
commit(WorkerRef, CommitMode) ->
  commit(WorkerRef, CommitMode, ?TIMEOUT).

-spec commit(WorkerRef :: worker_ref(), CommitMode :: commit_mode(), TimeOut :: timeout()) -> ok | {error, commit_reason()}.
commit(WorkerRef, CommitMode, TimeOut) ->
  gen_server:call(worker_pid(WorkerRef), {commit, CommitMode, TimeOut}).

-spec connect(Pool :: atom()) -> {ok, pid()}.
connect(Pool) ->
  connect(Pool, true).

-spec connect(Pool :: atom(), Block :: boolean()) -> {ok, pid()} | full.
connect(Pool, Block) ->
  connect(Pool, Block, ?TIMEOUT).

-spec connect(Pool :: atom(), Block :: boolean(), TimeOut :: timeout()) -> {ok, pid()} | full.
connect(Pool, Block, TimeOut) ->
  case poolboy:checkout(Pool, Block, TimeOut) of
    full ->
      full;
    WorkerRef when is_pid(WorkerRef) ->
      {ok, {Pool, WorkerRef}}
  end.

-spec disconnect(WorkerRef :: worker_ref()) -> ok.
disconnect({Pool, WorkerPid} = _WorkerRef) ->
  poolboy:checkin(Pool, WorkerPid).

%% deprecated. use disconnect/1
-spec disconnect(Pool :: atom(), Worker :: worker_ref()) -> ok.
disconnect(_Pool, Worker) ->
  disconnect(Worker).

-spec describe_table(WorkerRef :: worker_ref(), Table :: string()) -> {ok, [{col_name(), odbc_data_type()}]} | {error, common_reason()}.
describe_table(WorkerRef, Table) ->
  describe_table(WorkerRef, Table, ?TIMEOUT).

-spec describe_table(WorkerRef :: worker_ref(), Table :: string(), TimeOut :: timeout()) -> {ok, [{col_name(), odbc_data_type()}]} | {error, common_reason()}.
describe_table(WorkerRef, Table, TimeOut) ->
  gen_server:call(worker_pid(WorkerRef), {describe_table, Table, TimeOut}).

-spec first(WorkerRef :: worker_ref()) -> {selected, col_names(), rows()} | {error, scroll_reason()}.
first(WorkerRef) ->
  first(WorkerRef, ?TIMEOUT).

-spec first(WorkerRef :: worker_ref(), TimeOut :: timeout()) -> {selected, col_names(), rows()} | {error, scroll_reason()}.
first(WorkerRef, TimeOut) ->
  gen_server:call(worker_pid(WorkerRef), TimeOut).

-spec last(WorkerRef :: worker_ref()) -> {selected, col_names(), rows()} | {error, scroll_reason()}.
last(WorkerRef) ->
  last(WorkerRef, ?TIMEOUT).

-spec last(WorkerRef :: worker_ref(), TimeOut :: timeout()) -> {selected, col_names(), rows()} | {error, scroll_reason()}.
last(WorkerRef, TimeOut) ->
  gen_server:call(worker_pid(WorkerRef), {last, TimeOut}).

-spec next(WorkerRef :: worker_ref()) -> {selected, col_names(), rows()} | {error, result_reason()}.
next(WorkerRef) ->
  next(WorkerRef, ?TIMEOUT).

-spec next(WorkerRef :: worker_ref(), TimeOut :: timeout()) -> {selected, col_names(), rows()} | {error, result_reason()}.
next(WorkerRef, TimeOut) ->
  gen_server:call(worker_pid(WorkerRef), {next, TimeOut}).

-spec param_query(WorkerRef :: worker_ref(), SQLQuery :: string(), Params :: params()) -> result_tuple() | {error, common_reason()}.
param_query(WorkerRef, SqlQuery, Params) ->
  param_query(WorkerRef, SqlQuery, Params, ?TIMEOUT).

-spec param_query(WorkerRef :: worker_ref(), SQLQuery :: string(), Params :: params(), TimeOut :: timeout()) -> result_tuple() | {error, common_reason()}.
param_query(WorkerRef, SqlQuery, Params, TimeOut) ->
  gen_server:call(worker_pid(WorkerRef), {param_query, SqlQuery, Params, TimeOut}).

-spec prev(WorkerRef :: worker_ref()) -> {selected, ColNames :: [string()], Rows :: [any()]} | {error, scroll_reason()}.
prev(WorkerRef) ->
  prev(WorkerRef, ?TIMEOUT).

-spec prev(WorkerRef :: worker_ref(), TimeOut :: timeout()) -> {selected, ColNames :: [string()], Rows :: [any()]} | {error, scroll_reason()}.
prev(WorkerRef, TimeOut) ->
  gen_server:call(worker_pid(WorkerRef), {prev, TimeOut}).

-spec start() -> ok.
start() ->
  start(temporary).

-spec start(Type :: permanent | transient | temporary) -> ok.
start(Type) ->
  application:load(?MODULE),
  {ok, Apps} = application:get_key(?MODULE, applications),
  [ensure_started(App) || App <- Apps],
  application:start(?MODULE, Type).

-spec stop() -> ok.
stop() ->
  application:stop(?MODULE).

-spec sql_query(WorkerRef :: worker_ref(), SQLQuery :: string()) -> result_tuple() | [result_tuple()] | {error, common_reason()}.
sql_query(WorkerRef, SqlQuery) ->
  sql_query(WorkerRef, SqlQuery, ?TIMEOUT).

-spec sql_query(WorkerRef :: worker_ref(), SQLQuery :: string(), TimeOut :: timeout()) -> result_tuple() | [result_tuple()] | {error, common_reason()}.
sql_query(WorkerRef, SqlQuery, TimeOut) ->
  gen_server:call(worker_pid(WorkerRef), {sql_query, SqlQuery, TimeOut}).

-spec select_count(WorkerRef :: worker_ref(), SelectQuery :: string()) -> {ok, n_rows()} | {error, common_reason()}.
select_count(WorkerRef, SelectQuery) ->
  select_count(WorkerRef, SelectQuery, ?TIMEOUT).

-spec select_count(WorkerRef :: worker_ref(), SelectQuery :: string(), TimeOut :: timeout()) -> {ok, n_rows()} | {error, common_reason()}.
select_count(WorkerRef, SelectQuery, TimeOut) ->
  gen_server:call(worker_pid(WorkerRef), {select_count, SelectQuery, TimeOut}).

-spec select(WorkerRef :: worker_ref(), Position :: position(), N :: integer()) -> {selected, col_names(), rows()} | {error, scroll_reason()}.
select(WorkerRef, Position, N) ->
  select(WorkerRef, Position, N, ?TIMEOUT).

-spec select(WorkerRef :: worker_ref(), Position :: position(), N :: integer(), TimeOut :: timeout()) -> {selected, col_names(), rows()} | {error, scroll_reason()}.
select(WorkerRef, Position, N, TimeOut) ->
  gen_server:call(worker_pid(WorkerRef), {select, Position, N, TimeOut}).

%% private

ensure_started(App) ->
  case application:start(App) of
    ok ->
      ok;
    {error, {already_started, App}} ->
      ok
  end.

merge_params([], [], Acc) ->
  lists:reverse(Acc);
merge_params([ParamType | RestParamTypes], [Param | RestParams], Acc) ->
  FixedParams = fix_params_types(ParamType, [Param]),
  merge_params(RestParamTypes, RestParams, [FixedParams | Acc]).

fix_params_types({Type, Direction}, Params) when Direction =:= in; Direction =:= out; Direction =:= in_or_out ->
  {Type, Direction, Params};
fix_params_types(Type, Params) ->
  {Type, Params}.

worker_pid({_, WorkerPid}) when is_pid(WorkerPid) ->
  WorkerPid.
