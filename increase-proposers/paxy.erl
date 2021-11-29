-module(paxy).
-export([start/1, stop/0, stop/1]).

-define(RED, {255,0,0}).
-define(BLUE, {0,0,255}).
-define(GREEN, {0,255,0}).
-define(ORANGE, {255,165,0}).
-define(BLACK, {0,0,0}).
-define(PURPLE, {191,64,191}).
-define(YELLOW, {255,255,0}).
-define(LIGHTGREEN, {0,250,154}).



% Sleep is a list with the initial sleep time for each proposer
start(Sleep) ->
  AcceptorNames = ["Acceptor a", "Acceptor b", "Acceptor c", "Acceptor d", 
                   "Acceptor e", "Acceptor f", "Acceptor g", "Acceptor h", "Acceptor i", "Acceptor j"],
  AccRegister = [a, b, c, d, e, f, g, h, i, j],
  ProposerNames = [{"Proposer kurtz", ?RED}, {"Proposer kilgore", ?GREEN}, 
                   {"Proposer willard", ?BLUE}, {"Proposer imanol", ?ORANGE}, {"Proposer oriol", ?BLACK}, {"Proposer phelps", ?PURPLE}, {"Proposer socrates", ?YELLOW},{"Proposer carlos", ?LIGHTGREEN}],
  PropInfo = [{kurtz, ?RED}, {kilgore, ?GREEN}, {willard, ?BLUE}, {imanol, ?ORANGE}, {oriol, ?BLACK}, {phelps, ?PURPLE}, {socrates, ?YELLOW}, {carlos, ?LIGHTGREEN}],
  register(gui, spawn(fun() -> gui:start(AcceptorNames, ProposerNames) end)),
  gui ! {reqState, self()},
  receive
    {reqState, State} ->
      {AccIds, PropIds} = State,
      start_acceptors(AccIds, AccRegister),
      spawn(fun() -> 
        Begin = erlang:monotonic_time(),
        start_proposers(PropIds, PropInfo, AccRegister, Sleep, self()),
        wait_proposers(length(PropIds)),
        End = erlang:monotonic_time(),
        Elapsed = erlang:convert_time_unit(End-Begin, native, millisecond),
        io:format("[Paxy] Total elapsed time: ~w ms~n", [Elapsed])
      end)
  end.
    
start_acceptors(AccIds, AccReg) ->
  case AccIds of
    [] ->
      ok;
    [AccId|Rest] ->
      [RegName|RegNameRest] = AccReg,
      register(RegName, acceptor:start(RegName, AccId)),
      start_acceptors(Rest, RegNameRest)
  end.

start_proposers(PropIds, PropInfo, Acceptors, Sleep, Main) ->
  case PropIds of
    [] ->
      ok;
    [PropId|Rest] ->
      [{RegName, Colour}|RestInfo] = PropInfo,
      [FirstSleep|RestSleep] = Sleep,
      proposer:start(RegName, Colour, Acceptors, FirstSleep, PropId, Main),	
      start_proposers(Rest, RestInfo, Acceptors, RestSleep, Main)
  end.

wait_proposers(0) ->
  ok;
wait_proposers(N) ->
  receive
    done ->
      wait_proposers(N-1)
  end.

stop() ->
  stop(a),
  stop(b),
  stop(c),
  stop(d),
  stop(e),
  stop(gui).

stop(Name) ->
  case whereis(Name) of
    undefined ->
      ok;
    Pid ->
      Pid ! stop
  end.

 
