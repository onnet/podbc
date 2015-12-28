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

-module(podbc_worker).
-author('Matthias Endler <matthias.endler@pantech.at>').

-behaviour(gen_server).

%% API

-export([start_link/1]).

%% gen_server

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% API

start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

%% gen_server callbacks

-record(state, {name, conn, dsn, options}).

init(Args) ->
  Name = proplists:get_value(name, Args),
  Dsn = proplists:get_value(dsn, Args),
  Options = proplists:get_value(options, Args, []),
  {ok, ConnRef} = odbc:connect(Dsn, Options),
  {ok, #state{name = Name, conn = ConnRef, dsn = Dsn, options = Options}}.

handle_call({commit, CommitMode, TimeOut}, _From, #state{conn = ConnRef} = State) ->
  {reply, odbc:commit(ConnRef, CommitMode, TimeOut), State};
handle_call({describe_table, Table, TimeOut}, _From, #state{conn = ConnRef} = State) ->
  {reply, odbc:describe_table(ConnRef, Table, TimeOut), State};
handle_call({first, TimeOut}, _From, #state{conn = ConnRef} = State) ->
  {reply, odbc:first(ConnRef, TimeOut), State};
handle_call({last, TimeOut}, _From, #state{conn = ConnRef} = State) ->
  {reply, odbc:last(ConnRef, TimeOut), State};
handle_call({param_query, SqlQuery, Params, TimeOut}, _From, #state{conn = ConnRef, dsn = Dsn, options = Options} = State) ->
  case odbc:param_query(ConnRef, SqlQuery, Params, TimeOut) of
    {error, connection_closed} ->
        ConnRef1 = reconnect(ConnRef, Dsn, Options),
        Ret1 = odbc:param_query(ConnRef1, SqlQuery, Params, TimeOut),
        {reply, Ret1, State#state{conn = ConnRef1}};
    Ret -> {reply, Ret, State}
  end;
  %{reply, odbc:param_query(ConnRef, SqlQuery, Params, TimeOut), State};
handle_call({prev, TimeOut}, _From, #state{conn = ConnRef} = State) ->
  {reply, odbc:prev(ConnRef, TimeOut), State};
handle_call({sql_query, SqlQuery, TimeOut}, _From, #state{conn = ConnRef, dsn = Dsn, options = Options} = State) ->
  case odbc:sql_query(ConnRef, SqlQuery, TimeOut) of
    {error, connection_closed} ->
        ConnRef1 = reconnect(ConnRef, Dsn, Options),
        Ret1 = odbc:sql_query(ConnRef1, SqlQuery, TimeOut),
        {reply, Ret1, State#state{conn = ConnRef1}};
    Ret -> {reply, Ret, State}
  end;
  %{reply, odbc:sql_query(ConnRef, SqlQuery, TimeOut), State};
handle_call({select_count, SelectQuery, TimeOut}, _From, #state{conn = ConnRef} = State) ->
  {reply, odbc:sql_query(ConnRef, SelectQuery, TimeOut), State};
handle_call({select, Position, N, TimeOut}, _From, #state{conn = ConnRef} = State) ->
  {reply, odbc:select(ConnRef, Position, N, TimeOut), State};
handle_call(get_pool_name, _From, #state{name = Name} = State) ->
  {reply, {ok, Name}, State};
handle_call(_Request, _From, State) ->
  {reply, {error, not_implemented}, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, #state{conn = ConnRef}) ->
  ok = odbc:disconnect(ConnRef).

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

reconnect(ConnRef, Dsn, Options) ->
   try odbc:disconnect(ConnRef) of
        _ -> ok
   catch
        {DE, DR} -> error_logger:error_msg("podbcworker disconnect error ~p ~p ~n", [DE, DR]), ok
   end,
   {ok, ConnRef1} = odbc:connect(Dsn, Options),
   ConnRef1.
  
