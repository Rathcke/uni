-module(mr).
-behavior(gen_fsm).
-compile([export_all]).
%-export([start/0, job/6, stop/0, advanced_job/6]).
%-export([init/1]).
-import(worker, [start/1, assign_map/2, assign_reduce/2, begin_map/1, begin_reduction/1]).
-include("staterec.hrl").

%%% Public API
start() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  ok.

%% New job
job(Pid, NumWork, MapFun, RedFun, Initial, Data) ->
  gen_fsm:send_event(Pid, {job, NumWork, MapFun, RedFun, Initial, Data}),
  gen_fsm:send_event(Pid, initialize).

%% New advanced job
advanced_job(Pid, NumWork, MapFun, RedFun, Initial, Data) ->
  job(Pid, NumWork, MapFun, RedFun, Initial, Data).

begin_mapping(Pid) ->
  gen_fsm:send_event(Pid, begin_map).

begin_reduce(Pid) ->
  gen_fsm:send_event(Pid, begin_reduce).

get_result(Pid) ->
  gen_fsm:send_event(Pid, get_result).

%%% Callback functions
init([]) ->
  {ok, wait, []}.

%%% Master states
%% Initial state, where the master waits for a job.
wait({job, N, MapFun, RedFun, Init, Data},[]) ->
  io:format("Got job! ~n"),
  {next_state, initial, {N, MapFun, RedFun, Init, Data}};
wait(Event, Data) ->
  unexpected(Event, wait),
  {next_state, Data}.

%% Initiate a job by starting the necessary number of workers and reading the data.
initial(initialize, {N, MapFun, RedFun, Init, Data}) ->
  State = #state{workercount = N,
                 initial = Init,
                 mapfun = MapFun,
                 redfun = RedFun,
                 master = self()},
  DataSize = (length(Data) div (N-1)),
  MapperList = split_init_mappers(N-1, State, Data, DataSize),
  {_, Reducer} = worker:start(State),
  worker:assign_reduce(Reducer),
  io:format("transitioning to map state ~n"),
  {next_state, map, {{MapperList, Reducer}, 0, State}};
initial(Event, Data) ->
  unexpected(Event, initial),
  {next_state, initial, Data}.

%% Start the mappers and wait for them to finish.
%% Should keep track of amount of mappers done (when all done transisition to reduce state and notify reduce_worker)
map(map_done, {{MapperList, Reducer}, MappersDone, State}) ->
  case (MappersDone + 1 == State#state.workercount -1) of
    true ->
      io:format("transitioning to reduce state ~n"),
      {next_state, reduce, {Reducer, State}};
    false   ->
      io:format("waiting for all mappers to finish ~n"),
      {next_state, map, {{MapperList, Reducer}, MappersDone + 1, State}}
  end;
map(begin_map, {{MapperList, Reducer}, MappersDone, State}) ->
  io:format("Notifying mappers to begin ~n"),
  lists:map(fun(Pid) -> worker:begin_map(Pid, Reducer) end, MapperList),
  io:format("will begin waiting for mappers to finish ~n"),
  {next_state, map, {{MapperList, Reducer}, MappersDone, State}};
map(Event, Data) ->
  unexpected(Event, map),
  {next_state, map, Data}.

%% Start the reducer(s) and wait for it (them) to finish.
reduce(begin_reduce, {Reducer, State}) ->
  io:format("Notifying reducer to begin ~n"),
  begin_reduction(Reducer),
  {next_state, reduce, {Reducer, State}};
reduce({reduce_done, Result}, {Reducer, State}) ->
  io:format("Reducer done, transitioning to reuslt state ~n"),
  {next_state, result, {Result, State}};
reduce(Event, Data) ->
  unexpected(Event, reduce),
  {next_state, reduce, Data}.

%% Read the reduced result and reply to the process that started the job. Then back to wait.
result(get_result, {Result, State}) ->
  io:format("Result: ~p ~n", [Result]),
  {next_state, wait, #state{}};
result(Event, Data) ->
  unexpected(Event, result),
  {next_state, result, Data}.

%%% gen_fsm callback functions -- WARNING: GENERIC IMPLEMENTATIONS --

handle_event(Event, StateName, Data) ->
  unexpected(Event, StateName),
  {next_state, StateName, Data}.

handle_sync_event(Event, _From, StateName, Data) ->
  unexpected(Event, StateName),
  {next_state, StateName, Data}.

handle_info(Info, StateName, Data) ->
  unexpected(Info, StateName),
  {next_state, StateName, Data}.

code_change(_OldVsn, StateName, Data, _Extra) ->
  {ok, StateName, Data}.

terminate(_Reason, _StateName, _StateData) ->
  io:format("terminating ~n").

%%% Utility functions

%% Initiate list of workers with their respective datachunk to be mapped
split_init_mappers(1, State, Data, _) ->
  {_, Pid} = worker:start(State),
  worker:assign_map(Pid, Data),
  [Pid];
split_init_mappers(N, State, Data, DataSize) ->
  {_, Pid} = worker:start(State),
  {WorkerData, RestData} = lists:split(DataSize, Data),
  worker:assign_map(Pid, WorkerData),
  [Pid|split_init_mappers(N - 1, State, RestData, DataSize)].

%% Output in shell if unexpected event took place
unexpected(Msg, State) ->
  io:format("~p received unknown event ~p while in state ~p~n",
            [self(), Msg, State]).
