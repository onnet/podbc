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

-module(podbc_qresolver).
-author('Matthias Endler <matthias.endler@pantech.at>').

-behaviour(gen_server).

%% API

-export([start_link/0]).
-export([get/2]).

%% gen_server

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% API

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get(PoolName, QueryName) ->
  gen_server:call(?MODULE, {get, PoolName, QueryName}).

%% gen_server callbacks

-record(state, {}).

init(_Args) ->
  podbc_qresolver_map = ets:new(podbc_qresolver_map, [named_table, private]),
  case application:get_env(podbc, mappings) of
    undefined ->
      [];
    {ok, Mappings} ->
      [ets:insert(podbc_qresolver_map, Mapping) || Mapping <- Mappings]
  end,

  ?MODULE = ets:new(?MODULE, [named_table, private]),
  case application:get_env(podbc, named_queries) of
    undefined ->
      [];
    {ok, NamedQueries} ->
      AllQueries = named_queries(NamedQueries, []),
      [ets:insert(?MODULE, Query) || Query <- AllQueries]
  end,
  {ok, #state{}}.

handle_call({get, PoolName, QueryName}, _From, State) ->
  Reply = case ets:lookup(podbc_qresolver_map, PoolName) of
    [] ->
      {error, {not_found, {mapping, PoolName}}};
    [{PoolName, DbType}] ->
      case ets:lookup(?MODULE, {DbType, QueryName}) of
        [] ->
          {error, {not_found, {query_name, {DbType, QueryName}}}};
        [{{DbType, QueryName}, Sql, Params}] ->
          {ok, {Sql, Params}}
      end
  end,
  {reply, Reply, State};
handle_call(_Request, _From, State) ->
  {reply, {error, not_implemented}, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% private

named_queries([], Acc) ->
  lists:reverse(Acc);
named_queries([{DbType, Queries} | Rest], Acc) ->
  DbTypeQueries = [{{DbType, Name}, Sql, Params} || {Name, Sql, Params} <- Queries],
  named_queries(Rest, [DbTypeQueries | Acc]).
