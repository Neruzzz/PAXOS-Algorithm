-module(acceptor).
-export([start/2]).

start(Name, PanelId) ->
  spawn(fun() -> init(Name, PanelId) end).

storeAcceptor(Name, Promised, Voted, Value, PanelId) ->
  pers:open(Name),
  pers:store(Name, Promised , Voted, Value,PanelId),
  pers:close(Name).

init(Name, PanelId) ->
  pers:open(Name),
  Promised = order:null(), 
  Voted = order:null(),
  Value = na,
  case PanelId == na of
    false-> 
         pers:store(Name, Promised, Voted, Value, PanelId),
	       pers:close(Name),
		     acceptor(Name, Promised, Voted, Value, PanelId);
    true-> {Pr, Vot, Val, Pn} = pers:read(Name),
	       pers:close(Name),
		   case Pn == na of
			true-> 
				ok;
			false -> %recovering
				case Val == na of
					true ->Pn ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Vot]), 
                            "Promised: " ++ io_lib:format("~p", [Pr]), {0,0,0}};
					false-> Pn ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Vot]), 
                            "Promised: " ++ io_lib:format("~p", [Pr]), Val}
        end,
			  acceptor(Name, Pr, Vot, Val, Pn)
        end
  end.
  
  %case Pn == na of
  %true -> acceptor(Name, Pr, Vot, Val, PanelId),
  %		storeAcceptor(Name, Pr, Vot, Val, PanelId);
  %false -> acceptor(Name, Pr, Vot, Val, Pn)
  %end.
  


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
		  storeAcceptor(Name, Round, Voted, Value, PanelId),
          acceptor(Name, Round, Voted, Value, PanelId);
        false ->
          Proposer ! {sorry, {prepare, Round}},
		  storeAcceptor(Name, Promised, Voted, Value, PanelId),
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
			  storeAcceptor(Name, Promised, Round, Proposal, PanelId),
              acceptor(Name, Promised, Round, Proposal, PanelId);
            false ->
			  storeAcceptor(Name, Promised, Voted, Value, PanelId),
              acceptor(Name, Promised, Voted, Value, PanelId)
          end;                            
        false ->
          Proposer ! {sorry, {accept, Round}},
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    stop ->
	  storeAcceptor(Name, Promised, Voted, Value, PanelId),
	  %pers:delete(Name),
      PanelId ! stop,
      ok
  end.
