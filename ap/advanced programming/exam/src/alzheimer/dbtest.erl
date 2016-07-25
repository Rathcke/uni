-module(dbtest).

-export([start/0, insert/3, insertThrows/3, getRows/2, getRowsThrows/2]).

-import(alzheimer,[upsert/3, query/2]).


start() ->
  alzheimer:start().

insert(Pid, K, V) ->
  F = fun(X) -> case X of
                  {new, _Id} -> V; 
                  {existing,{_Id, _Data}} -> {modify, V}
                end
      end,
  alzheimer:upsert(Pid, K, F).

insertThrows(Pid, K, V) ->
  F = fun(X) -> case X of
                  {new, Id} -> 
                      if
                        Id == throw_error ->
                            throw(error_new);
                        true -> 
                            V
                      end;
                  {existing,{_Id, Data}} -> 
                      if
                        Data == throw_error ->
                            throw(error_existing);
                        true ->
                            {modify, V}
                      end
                end
      end,
  alzheimer:upsert(Pid, K, F).

getRows(Pid, LookupVal) ->
  Pred = fun(X) -> X == LookupVal end,
  alzheimer:query(Pid, Pred).
getRowsThrows(Pid, LookupVal) ->
  Pred = fun(X) ->
              if
                X == LookupVal ->
                  true;
                X == throw_error ->
                  throw(error);
                X /= LookupVal ->
                  false
              end 
         end,
  alzheimer:query(Pid, Pred).
