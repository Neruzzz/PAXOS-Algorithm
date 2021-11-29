-module(pers).
-export([open/1, read/1, store/5, close/1, delete/1]).

%% dets module provides term storage on file

open(Name) ->
    dets:open_file(Name, []).

%% returns the object with the key 'perm' stored in the table 'Name'
read(Name) ->
    case dets:lookup(Name, perm) of
        [{perm, Pr, Vt, Ac, Pn}] ->
            {Pr, Vt, Ac, Pn};
        [] ->
            {order:null(), order:null(), na, na}
    end.

%% inserts one object {Pr, Vt, Ac, Pn} into the table 'Name'
store(Name, Pr, Vt, Ac, Pn)->
    dets:insert(Name, {perm, Pr, Vt, Ac, Pn}).

close(Name) ->
    dets:close(Name).

delete(Name) ->
    file:delete(Name).
