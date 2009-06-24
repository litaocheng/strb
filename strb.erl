-module(strb).
-author('litaocheng@gmail.com').
-vsn('0.1').

-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

-type strb() :: binary().
-type direction() :: 'left' | 'right' | 'both'.

-spec len(S :: strb()) -> integer().
len(S) ->
    byte_size(S).

-spec equal(S1 :: strb(), S2 :: strb()) -> bool().
equal(_S, _S) -> true;
equal(_, _) -> false.

-spec concat(S1 :: strb(), S2 :: strb()) -> strb().
concat(S1, S2) ->
    <<S1/binary, S2/binary>>.

-spec chr(S :: strb(), C :: char()) -> non_neg_integer().
chr(S, C) ->
    chr(S, C, 1).

chr(<<C, _Rest/binary>>, C, I) ->
    I;
chr(<<_C1, Rest/binary>>, C, I) ->
    chr(Rest, C, I + 1);
chr(<<>>, _C, _I) ->
    0.

-spec rchr(S :: strb(), C :: char()) -> non_neg_integer().
rchr(S, C) ->
    rchr(S, C, 1, 0).

rchr(<<C, Rest/binary>>, C, I, _Pre) ->
    rchr(Rest, C, I+1, I);
rchr(<<_C1, Rest/binary>>, C, I, Pre) ->
    rchr(Rest, C, I+1, Pre);
rchr(<<>>, _C, _I, Pre) ->
    Pre.

-spec str(S :: strb(), SubS :: strb()) -> non_neg_integer().
str(S, SubS) ->
    str(S, SubS, 1).

str(<<C, Rest/binary>>, <<C, Sub/binary>> = SubS, I) ->
    case prefix(Sub, Rest) of
        true -> I;
        false -> str(Rest, SubS, I+1)
    end;
str(<<_C1, Rest/binary>>, <<_C2, _Sub/binary>> = SubS, I) ->
    str(Rest, SubS, I+1);
str(<<>>, _SubS, _I) ->
    0.

-spec rstr(S :: strb(), SubS :: strb()) -> non_neg_integer().
rstr(S, SubS) ->
    rstr(S, SubS, 1, 0).

rstr(<<C, Rest/binary>>, <<C, Sub/binary>>, I, Pre) ->
    case prefix(Sub, Rest) of
        true -> 
            rstr(Rest, <<C, Sub/binary>>, I+1, I);
        false ->
            rstr(Rest, <<C, Sub/binary>>, I+1, Pre)
    end;
rstr(<<_C1, Rest/binary>>, <<C2, Sub/binary>>, I, Pre) ->
    rstr(Rest, <<C2, Sub/binary>>, I+1, Pre);
rstr(<<>>, _SubS, _I, Pre) ->
    Pre.

-spec span(S :: strb(), Chars :: strb()) -> non_neg_integer().
span(S, Chars) ->
    span(S, Chars, 0).

span(<<C, Rest/binary>>, Chars, I) ->
    case member(C, Chars) of
        true ->
            span(Rest, Chars, I+1);
        false ->
            I
    end;
span(<<>>, _Chars, I) ->
    I.

-spec cspan(S :: strb(), Chars :: strb()) -> non_neg_integer().
cspan(S, Chars) ->
    cspan(S, Chars, 0).

cspan(<<C, Rest/binary>>, Chars, I) ->
    case member(C, Chars) of
        true ->
            I;
        false ->
            cspan(Rest, Chars, I+1)
    end;
cspan(<<>>, _Chars, I) ->
    I.


-spec substr(S :: strb(), Start :: pos_integer()) -> strb().
substr(S, 1) when is_binary(S) -> S;
substr(S, Start) when Start > 1 ->
    Off = Start-1,
    <<_:Off/binary, Rest/binary>> = S,
    Rest.

-spec substr(S :: strb(), Start :: pos_integer(), L :: pos_integer()) -> strb().
substr(S, Start, L) when is_integer(Start), Start >= 1, is_integer(L), L >= 0 ->
    substr1(substr(S, Start), L).

substr1(Str, L) when byte_size(Str) > L ->
    <<S:L/bytes, _Rest/binary>> = Str,
    S;
substr1(Str, L) when byte_size(Str) =< L ->
    Str.

-spec tokens(S :: strb(), Seps :: strb()) -> [strb()].
tokens(S, Seps) ->
    tokens1(S, Seps, []).

tokens1(<<C, Rest/binary>>, Seps, Toks) ->
    case member(C, Seps) of
        true ->
            tokens1(Rest, Seps, Toks);
        false ->
            tokens2(Rest, Seps, Toks, <<C>>)
    end;
tokens1(<<>>, _Seps, Toks) ->
    lists:reverse(Toks).

tokens2(<<C, Rest/binary>>, Seps, Toks, Bin) ->
    case member(C, Seps) of
        true ->
            tokens1(Rest, Seps, [Bin | Toks]); 
        false ->
            tokens2(Rest, Seps, Toks, <<Bin/binary, C>>)
    end;
tokens2(<<>>, _Seps, Toks, Bin) ->
    lists:reverse([Bin | Toks]).


-spec join(StrList :: [strb()], Seps :: strb()) -> strb().
join([H | T], Seps) ->
    list_to_binary([H] ++ [<<Seps/binary, S/binary>> || S <- T]);
join([], _Seps) ->
    <<>>.

-spec chars(Char :: char(), N :: non_neg_integer()) -> strb().
chars(Char, N) when is_integer(N), N >= 0 ->
    chars(Char, N, <<>>).

-spec chars(Char :: char(), N :: non_neg_integer(), Tail :: strb()) ->
    strb().
chars(Char, N, Tail) ->    
    Bins = <<0:N/unit:8, Tail/binary>>,
    binset(Bins, N, Char).

-spec copies(Str :: strb(), N :: non_neg_integer()) -> strb().
copies(Str, N) when is_integer(N), N >= 0 ->
    copies(Str, N, <<>>).

copies(_Str, 0, Bin) ->
    Bin;
copies(Str, N, Bin) ->
    copies(Str, N-1, <<Bin/binary, Str/binary>>).

-spec words(S :: strb()) -> non_neg_integer().
words(S) ->
    words(S, $\s).

-spec words(S :: strb(), C :: byte()) -> non_neg_integer().
words(S, C) ->
    w_count(strip(S, both, C), C, 0).

w_count(<<C, Rest/binary>>, C, Num) ->
    w_count(strip(Rest, left, C), C, Num+1);
w_count(<<_C1, Rest/binary>>, C, Num) ->
    w_count(Rest, C, Num);
w_count(<<>>, _C, Num) ->
    Num + 1.

-spec sub_word(S :: strb(), Index :: pos_integer()) -> strb().
sub_word(S, Index) ->
    sub_word(S, Index, $\s).

-spec sub_word(S :: strb(), Index :: pos_integer(), C :: byte()) -> strb().
sub_word(S, Index, C) ->
   case words(S, C) of
       Number when Number < Index ->
           <<>>;
       _Number ->
           s_word(strip(S, both, C), C, Index, 1, <<>>)
   end.

s_word(<<C, Rest/binary>>, C, Stop, Cur, Bin) ->
    s_word(strip(Rest), C, Stop, Cur+1, Bin);
s_word(<<C, _Rest/binary>>, C, Stop, Stop, Bin) -> % get the word
    Bin;
s_word(<<C1, Rest/binary>>, C, Stop, Stop, Bin) ->
    s_word(Rest, C, Stop, Stop, <<Bin/binary, C1>>);
s_word(<<_C1, Rest/binary>>, C, Stop, Cur, Bin) ->
    s_word(Rest, C, Stop, Cur, Bin);
s_word(<<>>, _C, _Stop, _Cur, Bin) ->
    Bin.

-spec strip(S :: strb()) -> strb().
strip(S) ->
    strip(S, both).

-spec strip(S :: strb(), Direct :: direction()) -> strb().
strip(S, Direct) ->
    strip(S, Direct, $\s).

-spec strip(S :: strb(), Direct :: direction(), Char :: byte()) -> strb().
strip(S, left, Char) -> strip_left(S, Char);
strip(S, right, Char) -> strip_right(S, Char);
strip(S, both, Char) -> strip_right(strip_left(S, Char), Char).
            
strip_left(<<C, Rest/binary>>, C) ->
    strip_left(Rest, C);
strip_left(S, _C) ->
    S.

strip_right(<<>>, _) ->
    <<>>;
strip_right(S, C) ->
    strip_right(S, byte_size(S)-1, C).

strip_right(S, Offset, C) ->
    <<Pre:Offset/binary, C1>> = S,
    case C1 of
        C ->
            strip_right(Pre, byte_size(Pre)-1, C);
        _ ->
            S
    end.

-spec left(S :: strb(), Num :: non_neg_integer()) -> Left :: strb().
left(S, Num) ->
    left(S, Num, $\s).

-spec left(S :: strb(), Num :: non_neg_integer(), Char :: byte()) -> Left :: strb().
left(S, Num, Char) ->
   case len(S) of
        L when L > Num ->
            substr(S, 1, Num);
        L when L < Num ->
            l_pad(S, Num-L, Char);
        Num ->
            S
    end.

l_pad(S, N, Char) ->
    <<S/binary, (chars(Char, N))/binary>>.

-spec right(S :: strb(), Num :: non_neg_integer()) -> Right :: strb().
right(S, Num) ->
    right(S, Num, $\s).

-spec right(S :: strb(), Num :: non_neg_integer(), Char :: byte()) -> strb().
right(S, Num, Char) ->
    case len(S) of
        L when L > Num ->
            substr(S, L-Num+1, Num);
        L when L < Num ->
            r_pad(S, Num-L, Char);
        Num ->
            S
    end.

r_pad(S, N, Char) ->
    chars(Char, N, S).

-spec centre(S :: strb(), Num :: non_neg_integer()) -> strb().
centre(S, Num) ->
    centre(S, Num, $\s).

-spec centre(S :: strb(), Num :: non_neg_integer(), Char :: byte()) -> strb().
centre(S, Num, Char) ->
    case len(S) of
        L when L > Num ->
            substr(S, (L-Num) div 2 + 1, Num);
        L when L < Num ->
            N = (Num-L) div 2,
            l_pad(r_pad(S, (Num-L-N), Char), N, Char);
        Num ->
            S
    end.

-spec sub_string(S :: strb(), Start :: pos_integer()) -> strb().
sub_string(S, Start) ->
    sub_string(S, Start, len(S)).

-spec sub_string(S :: strb(), Start :: pos_integer(), Stop :: pos_integer()) -> strb().
sub_string(S, Start, Stop) ->
    substr(S, Start, Stop - Start).

-spec to_float(S :: strb()) -> {float(), Rest :: strb()} | {'error', any()}.
to_float(S) ->
    case string:to_float(binary_to_list(S)) of
        {error, R} ->
            {error, R};
        {Float, Rest} ->
            {Float, list_to_binary(Rest)}
    end.

-spec to_integer(S :: strb()) -> {integer(), Rest :: strb()} | {'error', any()}.
to_integer(S) ->
    case string:to_integer(binary_to_list(S)) of
        {error, R} ->
            {error, R};
        {Int, Rest} ->
            {Int, list_to_binary(Rest)}
    end.

-spec to_lower(S :: strb()) -> strb();
              (C :: byte()) -> byte().
to_lower(S) when is_binary(S) ->
    << <<(to_lower(C))>> || <<C>> <= S >>;
to_lower(C) when is_integer(C) ->
    string:to_lower(C).

-spec to_upper(S :: strb()) -> strb();
              (C :: byte()) -> byte().
to_upper(S) when is_binary(S) ->
    << <<(to_upper(C))>> || <<C>> <= S >>;
to_upper(C) when is_integer(C) ->
    string:to_upper(C).

-spec member(C :: char(), S :: strb()) -> bool().
member(C, S) ->
    member1(S, C).

member1(<<_C, _Rest/binary>>, _C) -> true;
member1(<<_C1, Rest/binary>>, C) ->
    member1(Rest, C);
member1(<<>>, _C) -> false.

-spec binset(Bin :: binary(), Char :: byte()) -> binary().
binset(Bin, Char) ->
    << <<(Char)>> || <<_>> <= Bin >>.

-spec binset(Bin :: binary(), N :: non_neg_integer(), Char :: byte()) -> binary().
binset(Bin, N, Char) ->
    <<BinS:N/binary, Rest/binary>> = Bin,
    BinS2 = binset(BinS, Char),
    <<BinS2/binary, Rest/binary>>.


%%
%% Internal API
%%
prefix(<<C, Sub/binary>>, <<C, Rest/binary>>) ->
    prefix(Sub, Rest);
prefix(<<>>, _Rest) ->
    true;
prefix(<<_C1, _Sub/binary>>, <<_C2, _Rest/binary>>) ->
    false.

substr2(Str, 1) -> Str;
substr2(<<_C, Rest/binary>>, Start) ->
    substr2(Rest, Start - 1).


%%
%% unit test
%%
-ifdef(EUNIT).
len_test_() ->
    [
        ?_assert(len(<<"HELLO">>) =:= 5),
        ?_assert(len(<<"">>) =:= 0),
        ?_assert(len(<<"1234567891">>) =:= 10)
    ].

equal_test_() ->
    [
        ?_assertMatch(true, equal(<<"ok">>, <<"ok">>)),
        ?_assertMatch(false, equal(<<"OK">>, <<"ok">>))
    ].

concat_test_() ->
    [
        ?_assert(concat(<<"1">>, <<"2">>) =:= <<"12">>)
    ].

chr_test_() ->
    [
        ?_assert(chr(<<"hello world">>, $h) =:= 1),
        ?_assert(chr(<<"hello world">>, $w) =:= 7),
        ?_assert(chr(<<"hello world">>, $d) =:= 11),
        ?_assert(chr(<<"hello world">>, $x) =:= 0)
    ].

rchr_test_() ->
    [
        ?_assert(rchr(<<"apple orange">>, $a) =:= 9),
        ?_assert(rchr(<<"apple orange">>, $g) =:= 11),
        ?_assert(rchr(<<"apple orange">>, $x) =:= 0)
    ].

str_test_() ->
    [
        ?_assert(str(<<"apple orage">>, <<"app">>) =:= 1),
        ?_assert(str(<<"apple orage">>, <<"or">>) =:= 7),
        ?_assert(str(<<"apple orage">>, <<"xy">>) =:= 0),
        ?_assert(str(<<"apple orage">>, <<"e">>) =:= 5)
    ].

rstr_test_() ->
    [
        ?_assert(rstr(<<"apple orage">>, <<"app">>) =:= 1),
        ?_assert(rstr(<<"apple orage">>, <<"or">>) =:= 7),
        ?_assert(rstr(<<"apple orage">>, <<"xy">>) =:= 0),
        ?_assert(rstr(<<"apple orage">>, <<"e">>) =:= 11)
    ].

span_test_() ->
    [
        ?_assert(span(<<"\t    abcdef">>, <<" \t">>) =:= 5),
        ?_assert(span(<<"\t abcdef">>, <<" \tabcdef">>) =:= 8),
        ?_assert(span(<<"abcdef">>, <<" \t">>) =:= 0)
    ].

cspan_test_() ->
    [
        ?_assert(cspan(<<"\t    abcdef">>, <<" \t">>) =:= 0),
        ?_assert(cspan(<<"\t abcdef">>, <<" \tabcdef">>) =:= 0),
        ?_assert(cspan(<<"abcdef">>, <<"f">>) =:= 5 )
    ].

substr_test_() ->
    [
        ?_assert(substr(<<"hello world">>, 1) =:= <<"hello world">>),
        ?_assert(substr(<<"hello world">>, 5) =:= <<"o world">>),
        ?_assert(substr(<<"hello world">>, 11) =:= <<"d">>),
        ?_assert(substr(<<"hello world">>, 12) =:= <<>>),
        ?_assertException(error, function_clause, substr(<<"hello world">>, 0))
    ].

tokens_test_() ->
    [
        ?_assert(tokens(<<"abc defxxghix jkl">>, <<"x ">>) =:= 
                [<<"abc">>, <<"def">>, <<"ghi">>, <<"jkl">>]),
            ?_assert(tokens(<<"">>, <<"x">>) =:= []),
            ?_assert(tokens(<<"first,second,third">>, <<",">>) =:=
                [<<"first">>, <<"second">>, <<"third">>])
     ].

join_test_() ->
     [
         ?_assert(join([<<"one">>, <<"two">>], <<"||">>) =:=
                <<"one||two">>),
            ?_assert(join([<<"one">>], <<"||">>) =:= <<"one">>),
            ?_assert(join([<<"">>], <<"||">>) =:= <<"">>)
    ].

chars_test_() ->
    [
        ?_assert(chars($A, 5) =:= <<"AAAAA">>),
        ?_assert(chars($\s, 1) =:= <<"\s">>),
        ?_assert(chars($:, 3, <<"ok">>) =:= <<":::ok">>),
        ?_assert(chars($:, 0, <<"ok">>) =:= <<"ok">>)
    ].

copies_test_() ->
    [
        ?_assert(copies(<<"ok">>, 3) =:= <<"okokok">>),
        ?_assert(copies(<<"ok">>, 1) =:= <<"ok">>),
        ?_assert(copies(<<"ok">>, 0) =:= <<"">>)
    ].

words_test_() ->
    [
        ?_assertEqual(3, words(<<"hello ok yes">>)),
        ?_assertEqual(1, words(<<"hellookyes">>)),
        ?_assertEqual(1, words(<<"hello ok yes">>, $\t)),
        ?_assertEqual(3, words(<<"hello ok yes">>, $o))
    ].

sub_word_test_() ->
    [
        ?_assertEqual(<<"ok">>, sub_word(<<"hello ok yes">>, 2)),
        ?_assertEqual(<<"yes">>, sub_word(<<"hello ok yes">>, 3)),
        ?_assertEqual(<<"">>, sub_word(<<"hello ok yes">>, 4))
    ].

strip_test_() ->
    [
        ?_assertEqual(<<"ok">>, strip(<<"   ok    ">>)),
        ?_assertEqual(<<"">>, strip(<<"     ">>)),
        ?_assertEqual(<<"ok...">>, strip(<<"...ok...">>, left, $.)),
        ?_assertEqual(<<"...ok">>, strip(<<"...ok...">>, right, $.)),
        ?_assertEqual(<<"ok">>, strip(<<"...ok...">>, both, $.))
    ].

left_test_() ->
    [
        ?_assertEqual(<<"yes">>, left(<<"yes or no?">>, 3)),
        ?_assertEqual(<<"yes  ">>, left(<<"yes">>, 5)),
        ?_assertEqual(<<"yes..">>, left(<<"yes">>, 5, $.)),
        ?_assertEqual(<<"y">>, left(<<"yes">>, 1, $.))
    ].

right_test_() ->
    [
        ?_assertEqual(<<"no?">>, right(<<"yes or no?">>, 3)),
        ?_assertEqual(<<"  yes">>, right(<<"yes">>, 5)),
        ?_assertEqual(<<"..yes">>, right(<<"yes">>, 5, $.)),
        ?_assertEqual(<<"s">>, right(<<"yes">>, 1, $.))
    ].

centre_test_() ->
    [
        ?_assertEqual(<<" or">>, centre(<<"yes or no">>, 3)),
        ?_assertEqual(<<" yes ">>, centre(<<"yes">>, 5)),
        ?_assertEqual(<<".yes.">>, centre(<<"yes">>, 5, $.)),
        ?_assertEqual(<<"e">>, centre(<<"yes">>, 1, $.))
    ].

to_lower_test_() ->
    [
        ?_assertEqual(<<"ok,yes">>, to_lower(<<"OK,yES">>)),
        ?_assertEqual(<<"123..+yes">>, to_lower(<<"123..+YES">>))
    ].

to_upper_test_() ->
    [
        ?_assertEqual(<<"OK,YES">>, to_upper(<<"oK,yES">>)),
        ?_assertEqual(<<"123..+YES">>, to_upper(<<"123..+yEs">>))
    ].

-endif.
