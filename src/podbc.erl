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

-define(TIMEOUT, 5000).

%% podbc API

-export([do/2, do/3, do/4]).
-export([transaction/2, transaction/3]).
-export([get_pool_name/1]).
-export([named_query/3, named_query/4]).

%% odbc API

-export([commit/2, commit/3]).
-export([connect/1, connect/2, connect/3]).
-export([disconnect/2]).
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

do(Pool, Fun) ->
  do(Pool, Fun, true).

do(Pool, Fun, Block) ->
  do(Pool, Fun, Block, ?TIMEOUT).

do(Pool, Fun, Block, TimeOut) ->
  case connect(Pool, Block, TimeOut) of
    {ok, WorkerRef} ->
      try
        {ok, Fun(WorkerRef)}
      after
        ok = disconnect(Pool, WorkerRef)
      end;
    Other ->
      Other
  end.

transaction(WorkerRef, Fun) ->
  transaction(WorkerRef, Fun, ?TIMEOUT).

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

get_pool_name(WorkerRef) ->
  gen_server:call(WorkerRef, get_pool_name).

named_query(WorkerRef, QueryName, Params) ->
  named_query(WorkerRef, QueryName, Params, ?TIMEOUT).

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

commit(WorkerRef, CommitMode) ->
  commit(WorkerRef, CommitMode, ?TIMEOUT).

commit(WorkerRef, CommitMode, TimeOut) ->
  gen_server:call(WorkerRef, {commit, CommitMode, TimeOut}).

connect(Pool) ->
  connect(Pool, true).

connect(Pool, Block) ->
  connect(Pool, Block, ?TIMEOUT).

connect(Pool, Block, TimeOut) ->
  case poolboy:checkout(Pool, Block, TimeOut) of
    full ->
      {error, full};
    WorkerRef when is_pid(WorkerRef) ->
      {ok, WorkerRef}
  end.

disconnect(Pool, Worker) ->
  poolboy:checkin(Pool, Worker).

describe_table(WorkerRef, Table) ->
  describe_table(WorkerRef, Table, ?TIMEOUT).

describe_table(WorkerRef, Table, TimeOut) ->
  gen_server:call(WorkerRef, {describe_table, Table, TimeOut}).

first(WorkerRef) ->
  first(WorkerRef, ?TIMEOUT).

first(WorkerRef, TimeOut) ->
  gen_server:call(WorkerRef, TimeOut).

last(WorkerRef) ->
  last(WorkerRef, ?TIMEOUT).

last(WorkerRef, TimeOut) ->
  gen_server:call(WorkerRef, {last, TimeOut}).

next(WorkerRef) ->
  next(WorkerRef, ?TIMEOUT).

next(WorkerRef, TimeOut) ->
  gen_server:call(WorkerRef, {next, TimeOut}).

param_query(WorkerRef, SqlQuery, Params) ->
  param_query(WorkerRef, SqlQuery, Params, ?TIMEOUT).

param_query(WorkerRef, SqlQuery, Params, TimeOut) ->
  gen_server:call(WorkerRef, {param_query, SqlQuery, Params, TimeOut}).

prev(WorkerRef) ->
  prev(WorkerRef, ?TIMEOUT).

prev(WorkerRef, TimeOut) ->
  gen_server:call(WorkerRef, {prev, TimeOut}).

start() ->
  start(temporary).

start(Type) ->
  application:load(?MODULE),
  {ok, Apps} = application:get_key(?MODULE, applications),
  [ensure_started(App) || App <- Apps],
  application:start(?MODULE, Type).

stop() ->
  application:stop(?MODULE).

sql_query(WorkerRef, SqlQuery) ->
  sql_query(WorkerRef, SqlQuery, ?TIMEOUT).

sql_query(WorkerRef, SqlQuery, TimeOut) ->
  gen_server:call(WorkerRef, {sql_query, SqlQuery, TimeOut}).

select_count(WorkerRef, SelectQuery) ->
  select_count(WorkerRef, SelectQuery, ?TIMEOUT).

select_count(WorkerRef, SelectQuery, TimeOut) ->
  gen_server:call(WorkerRef, {select_count, SelectQuery, TimeOut}).

select(WorkerRef, Position, N) ->
  select(WorkerRef, Position, N, ?TIMEOUT).

select(WorkerRef, Position, N, TimeOut) ->
  gen_server:call(WorkerRef, {select, Position, N, TimeOut}).

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
