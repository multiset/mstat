-module(mstat_util).

-export([ejsonify/1]).

ejsonify([{_, _}|_]=Proplist) ->
    EJSONProps = lists:map(
       fun({Key, Value}) -> {maybe_format_key(Key), ejsonify(Value)} end,
       Proplist
    ),
    {EJSONProps};
ejsonify(NotAProplist) ->
    NotAProplist.

maybe_format_key(K) when is_list(K) ->
    format_list_key(lists:map(fun to_binary/1, K));
maybe_format_key(K) ->
    to_binary(K).

format_list_key(K) ->
    format_list_key(K, <<>>).

format_list_key([], K) ->
    K;
format_list_key([H|T], <<>>) ->
    format_list_key(T, H);
format_list_key([H|T], K) ->
    format_list_key(T, <<K/binary, <<".">>/binary, H/binary>>).

to_binary(I) when is_atom(I) ->
    list_to_binary(atom_to_list(I));
to_binary(I) when is_integer(I) ->
    integer_to_binary(I);
to_binary(I) ->
    I.
