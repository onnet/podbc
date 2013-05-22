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

-module(podbc_worker_sup).
-author('Matthias Endler <matthias.endler@pantech.at>').

-behaviour(supervisor).

-define(SUPERVISOR, ?MODULE).

%% API

-export([start_link/0]).

%% supervisor

-export([init/1]).
-export([add_child/3]).
-export([remove_child/1]).

%% API

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

add_child(Name, SizeArgs, WorkerArgs) ->
  ChildSpec = pool_child_spec({Name, SizeArgs, WorkerArgs}),
  supervisor:start_child(?SUPERVISOR, ChildSpec).

remove_child(Name) ->
  supervisor:terminate_child(?SUPERVISOR, Name),
  supervisor:delete_child(?SUPERVISOR, Name),
  ok.

%% supervisor callbacks

init([]) ->
  RestartStrategy = one_for_one,
  MaxR = 10,
  MaxT = 10,
  SupFlags = {RestartStrategy, MaxR, MaxT},
  Children = get_specs_from_config(),
  {ok, {SupFlags, Children}}.

%% private

get_specs_from_config() ->
  PoolCfg = case application:get_env(podbc, pools) of
    undefined ->
      [];
    {ok, Pools} ->
      Pools
  end,
  pool_child_specs(PoolCfg, []).

pool_child_specs([], Acc) ->
  lists:reverse(Acc);
pool_child_specs([PoolCfg | Rest], Acc) ->
  pool_child_specs(Rest, [pool_child_spec(PoolCfg) | Acc]).

pool_child_spec({Name, SizeArgs, WorkerArgs}) ->
  PoolArgs = [{name, {local, Name}}, {worker_module, podbc_worker} | SizeArgs],
  poolboy:child_spec(Name, PoolArgs, [{name, Name} | WorkerArgs]).
