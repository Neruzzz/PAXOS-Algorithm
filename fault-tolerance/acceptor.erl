-module(acceptor).
-module(pers).
-export([start/2]).

start(Name, PanelId) ->
  spawn(fun() -> init(Name, PanelId) end).
        
init(Name, PanelId) ->
  open(Name),
  Promised = order:null(), 
  Voted = order:null(),
  Value = na,
  store(Name, Promised, Voted, Value, PanelId),
  acceptor(Name, Promised, Voted, Value, PanelId).

acceptor(Name, Promised, Voted, Value, PanelId) ->
  receive
    {prepare, Proposer, Round} ->
      case order:gr(Round, Promised) of
        true ->
          Proposer ! {promise, Round, Voted, Value},               
      io:format("[Acceptor ~w] Phase 1: promised ~w voted ~w colour ~w~n",
                 [Name, Round, Voted, Value]),
          % Update gui
          Colour = case Value of na -> {0,0,0}; _ -> Value end,
          PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Voted]), 
                     "Promised: " ++ io_lib:format("~p", [Round]), Colour},
		  store(Name, Round, Voted, Value, PanelId),
          acceptor(Name, Round, Voted, Value, PanelId);
        false ->
          Proposer ! {sorry, {prepare, Round}},
		  store(Name, Promised, Voted, Value, PanelId),
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    {accept, Proposer, Round, Proposal} -> %Round: ballot value, Proposal: the value within the ballot
      case order:goe(Round, Promised) of
        true ->
          Proposer ! {vote, Round},
          case order:goe(Round, Voted) of
            true ->
      io:format("[Acceptor ~w] Phase 2: promised ~w voted ~w colour ~w~n",
                 [Name, Promised, Round, Proposal]),
              % Update gui
              PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Round]), 
                         "Promised: " ++ io_lib:format("~p", [Promised]), Proposal},
			  store(Name, Promised, Round, Proposal, PanelId);
              acceptor(Name, Promised, Round, Proposal, PanelId);
            false ->
			  store(Name, Promised, Voted, Value, PanelId),
              acceptor(Name, Promised, Voted, Value, PanelId)
          end;                            
        false ->
          Proposer ! {sorry, {accept, Round}},
		  store(Name, Promised, Voted, Value, PanelId),
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    stop ->
      PanelId ! stop,
      ok
  end.
