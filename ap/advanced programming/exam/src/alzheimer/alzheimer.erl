-module(alzheimer).
-export([start/0, upsert/3, query/2]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_info/2,
         handle_cast/2, terminate/2, code_change/3, filter_error/2]).
%%%=========================================================================
%%%  API
%%%=========================================================================

start() ->
    gen_server:start_link(?MODULE, [], []).

query(Aid, Pred) ->
    gen_server:call(Aid, {query, Pred}).

upsert(Aid, Id, F) ->
    case gen_server:call(Aid, {upsert, Id, F}) of
      {error, Err} -> throw(Err);
      {ok, Ok}     -> Ok
    end.

%%% Server functions
init([]) ->
    {ok, dict:new()}.

handle_call({upsert, Id, F}, _From, DB) ->
    case dict:is_key(Id, DB) of
      false -> try F({new, Id}) of
                 Val -> {reply, {ok, Val}, dict:store(Id, Val, DB)}
               catch
                 throw:Err -> {reply, {error, Err}, DB}
               end;
      true  -> {_, Data} = dict:find(Id, DB),
               try F({existing, {Id, Data}}) of
                 {modify, NewData} -> {reply, {ok,{modify, NewData}}, dict:store(Id, NewData, DB)};
                 ignore            -> {reply, {ok, ignore}, DB}
               catch
                 throw:Err -> {reply, {error, Err}, DB}
               end
    end;
handle_call({query, Pred}, _From, DB) ->
    List = dict:to_list(DB),
    Rows = filter_error(List, Pred),
    %% FIX IF LIST IS EMPTY
    case Rows of
        [] -> 
            {reply,{ok, []}, DB};
        Rws ->
            case lists:last(Rws) of
                {error, _, _, Row} -> {reply, {error, Row}, DB};
                _                  -> {reply, {ok, Rows}, DB}
            end
    end.
   
% If an exceptions is raised while running, recursion terminates
% and returns. The exception will be in the end of the list if one
% occurs (can be used for matching later).
filter_error([], _) ->
  [];
filter_error([{_, Val} = L | Ls], Pred) ->
  try Pred(Val) of
    true  -> [L | filter_error(Ls, Pred)];
    false -> filter_error(Ls, Pred)
  catch
    Exception:Reason -> [{error, Exception, Reason, L}]
  end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason ,_State) ->
    ok.

code_change(_, State, _Extra) ->
    {ok, State}.
