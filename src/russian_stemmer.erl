%% -*- coding: utf-8 -*-

%% Copyright (c) 2014
%%
%% Russian language Implementation of Porter Stemming Algorithm in Erlang  
%%
%% Alexander Beniaminov (abeniaminov@gmail.com)
%%
%% Permission is  hereby  granted,  free of charge,  to any person
%% obtaining  a copy of this software and associated documentation
%% files (the "Software"),to deal in the Software without restric-
%% tion,  including  without  limitation the rights to use,  copy,
%% modify, merge,  publish,  distribute,  sublicense,  and/or sell
%% copies  of the  Software,  and to  permit  persons to  whom the
%% Software  is  furnished  to do  so,  subject  to the  following
%% conditions:
%%
%% The above  copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF  MERCHANTABILITY,  FITNESS  FOR  A  PARTICULAR  PURPOSE  AND
%% NONINFRINGEMENT. IN  NO  EVENT  SHALL  THE AUTHORS OR COPYRIGHT
%% HOLDERS  BE  LIABLE FOR  ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT,  TORT  OR OTHERWISE,  ARISING
%% FROM,  OUT OF OR IN CONNECTION WITH THE SOFTWARE  OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
%% 



-module(russian_stemmer).

-export([stem/1]).


-define(PERFECTIVE_GERUND_GROUP1, 
        [ <<"ьсишва"/utf8>>, <<"ишва"/utf8>>, <<"ва"/utf8>>,  <<"ьсишвя"/utf8>>, <<"ишвя"/utf8>>, <<"вя"/utf8>> ]).

-define(PERFECTIVE_GERUND_GROUP2, 
        [<<"ьсишви"/utf8>>, <<"ишви"/utf8>>, <<"ви"/utf8>>,  <<"ьсишвы"/utf8>>, <<"ишвы"/utf8>>, <<"вы"/utf8>>]).

-define(ADJECTIVE, lists:append(
          [
            [ <<A/binary, B/binary>> || 
                A <- [<<"е"/utf8>>, <<"й"/utf8>>, <<"м"/utf8>>],
                B <- [<<"е"/utf8>>, <<"и"/utf8>>, <<"ы"/utf8>>, <<"о"/utf8>>]
            ],
            [ <<"оге"/utf8>>, <<"уме"/utf8>>,<<"ого"/utf8>>, <<"умо"/utf8>>,
              <<"ими"/utf8>>, <<"хи"/utf8>>, <<"имы"/utf8>>, <<"хы"/utf8>>,
              <<"юу"/utf8>>, <<"юю"/utf8>>, <<"яа"/utf8>>, <<"яя"/utf8>>, <<"юо"/utf8>>, <<"юе"/utf8>>]
          ])
       ).

-define(PARTICIPLE_GROUP1,
          [ <<A/binary, B/binary>> || 
                A <- [<<"ме"/utf8>>, <<"нн"/utf8>>, <<"шв"/utf8>>, <<"щю"/utf8>>, <<"щ"/utf8>>],
                B <- [<<"а"/utf8>>, <<"я"/utf8>>]
          ]
       ).

-define(PARTICIPLE_GROUP2,
          [ <<"шви"/utf8>>, <<"швы"/utf8>>, <<"щюу"/utf8>>]
       ).

-define(REFLEXIVE, 
          [ <<"яс"/utf8>>, <<"ьс"/utf8>>]
       ).


-define(VERB_GROUP1,
        [ <<A/binary, B/binary>> || 
                A <- [<<"ал"/utf8>>, <<"ан"/utf8>>, <<"ете"/utf8>>, <<"етй"/utf8>>, <<"ил"/utf8>>,
                      <<"й"/utf8>>, <<"л"/utf8>>, <<"ме"/utf8>>, <<"н"/utf8>>, <<"ол"/utf8>>,
                      <<"он"/utf8>>, <<"те"/utf8>>, <<"тю"/utf8>>, <<"ын"/utf8>>, <<"ьт"/utf8>>,
                      <<"ьше"/utf8>>, <<"онн"/utf8>>
                     ],
                B <- [<<"а"/utf8>>, <<"я"/utf8>>]
        ]
       ).

-define(VERB_GROUP2, 
          [ <<"али"/utf8>>,
            <<"алы"/utf8>>,
            <<"ане"/utf8>>,
            <<"етйе"/utf8>>,
            <<"етйу"/utf8>>,
            <<"ети"/utf8>>,
            <<"или"/utf8>>,
            <<"илы"/utf8>>,
            <<"йе"/utf8>>,
            <<"йу"/utf8>>,
            <<"ли"/utf8>>,
            <<"лы"/utf8>>,
            <<"ми"/utf8>>,
            <<"мы"/utf8>>,
            <<"не"/utf8>>,
            <<"оли"/utf8>>,
            <<"олы"/utf8>>,
            <<"оне"/utf8>>,
            <<"тя"/utf8>>,
            <<"теу"/utf8>>,
            <<"тюу"/utf8>>,
            <<"ти"/utf8>>,
            <<"ты"/utf8>>,
            <<"ыне"/utf8>>,
            <<"ьти"/utf8>>,
            <<"ьты"/utf8>>,
            <<"ьши"/utf8>>,
            <<"юу"/utf8>>,
            <<"ю"/utf8>>
        ]
      ).


-define(NOUN, 
          [ <<"а"/utf8>>,
            <<"ве"/utf8>>,
            <<"во"/utf8>>,
            <<"еи"/utf8>>,
            <<"еь"/utf8>>,
            <<"е"/utf8>>,
            <<"имяи"/utf8>>,
            <<"имя"/utf8>>,
            <<"има"/utf8>>,
            <<"ие"/utf8>>,
            <<"ии"/utf8>>,
            <<"и"/utf8>>,
            <<"йеи"/utf8>>,
            <<"йе"/utf8>>,
            <<"йо"/utf8>>,
            <<"йи"/utf8>>,
            <<"й"/utf8>>,
            <<"мяи"/utf8>>,
            <<"мя"/utf8>>,
            <<"меи"/utf8>>,
            <<"ме"/utf8>>,
            <<"ма"/utf8>>,
            <<"мо"/utf8>>,
            <<"о"/utf8>>,
            <<"у"/utf8>>,
            <<"ха"/utf8>>,
            <<"хяи"/utf8>>,
            <<"хя"/utf8>>,
            <<"ы"/utf8>>,
            <<"ь"/utf8>>,
            <<"юи"/utf8>>,
            <<"юь"/utf8>>,
            <<"ю"/utf8>>,
            <<"яи"/utf8>>,
            <<"яь"/utf8>>,
            <<"я"/utf8>>
        ]
      ).

-define(SUPERLATIVE,
            [<<"шйе"/utf8>>, <<"ешйе"/utf8>>]
       ).

-define(DERIVATIONAL,
            [<<"тсо"/utf8>>,  <<"ьтсо"/utf8>>]
       ).

-define(ADJECTIVAL_GROUP1,
            [ <<AA/binary, BB/binary>> || 
                AA <- ?ADJECTIVE, BB <- ?PARTICIPLE_GROUP1] 
       ).

-define(ADJECTIVAL_GROUP2,
        [ <<AA/binary, BB/binary>> || 
            AA <- ?ADJECTIVE, BB <- ?PARTICIPLE_GROUP2] 
       ).


-define(I, [<<"и"/utf8>>]).

-define(DOUBLE_N, [<<"нн"/utf8>>]).

-define(SOFT_SIGN, [<<"ь"/utf8>>]).

-define(STEP1,
        [
          {?PERFECTIVE_GERUND_GROUP1, type1, if_removed_then_end, rv}, 
          {?PERFECTIVE_GERUND_GROUP2, type2, if_removed_then_end, rv},
          {?REFLEXIVE, type2, try_then_continue, rv},
          {?ADJECTIVAL_GROUP1, type1, if_removed_then_end, rv},
          {?ADJECTIVAL_GROUP2, type2, if_removed_then_end, rv},
          {?ADJECTIVE, type2, if_removed_then_end, rv},
          {?VERB_GROUP1, type1, if_removed_then_end, rv},
          {?VERB_GROUP2, type2, if_removed_then_end, rv},
          {?NOUN, type2, if_removed_then_end, rv}
        ]
       ).

-define(STEP2,
        [
          {?I, type2, if_removed_then_end, rv}
        ] 
       ).
-define(STEP3,
        [
          {?DERIVATIONAL, type2, if_removed_then_end, r2}
        ] 
       ). 

-define(STEP4,
        [
          {?DOUBLE_N, type1, if_removed_then_end, rv},
          {?SUPERLATIVE, type2, try_then_continue, rv},
          {?DOUBLE_N, type1, if_removed_then_end, rv},
          {?SOFT_SIGN, type2, if_removed_then_end, rv}
        ]
    ).


is_vowel(Ch) when is_binary(Ch) ->
    case Ch of
        <<$а/utf8>> -> true;
        <<$е/utf8>> -> true;
        <<$и/utf8>> -> true;
        <<$о/utf8>> -> true;
        <<$у/utf8>> -> true;
        <<$ы/utf8>> -> true;
        <<$э/utf8>> -> true;
        <<$ю/utf8>> -> true;
        <<$я/utf8>> -> true;
        _  -> false
    end;


is_vowel(Ch)  ->
    case Ch of
        16#0430 -> true;  %% а
        16#0435 -> true;  %% е
        16#0438 -> true;  %% и
        16#043E -> true;  %% о 
        16#0443 -> true;  %% у
        16#044B -> true;  %% ы
        16#044D -> true;  %% э 
        16#044E -> true;  %% ю
        16#044F -> true;  %% я
        _  -> false
    end.


stem(Word) ->
    WordCodePoint = replace(code_point(Word), 16#0451, 16#0435),  %% ё -> е
    RV = after_first_vowel(WordCodePoint),
    if RV == 0 -> utf8(WordCodePoint);
      true -> 
            R2 = region_R2(WordCodePoint),
            utf8(steps(WordCodePoint,RV,R2))
    end.

region_R2(Word) ->
    R1 = after_vowel_not_vowel( Word),
    if R1 == 0 -> R2 = 0;
      true -> R2 = after_vowel_not_vowel(lists:nthtail(length(Word)-R1, Word))
    end,
    R2.


after_first_vowel([]) -> 0;
after_first_vowel([H|T]) -> 
    case is_vowel(H) of
         true-> length(T);
         false -> after_first_vowel(T)
    end.


after_vowel_not_vowel([]) -> 0;
after_vowel_not_vowel([_])-> 0;
after_vowel_not_vowel([A,B|T]) ->
    case is_vowel(A) and not is_vowel(B) of 
       true -> length(T);
       false -> after_vowel_not_vowel([B|T])
    end.

code_point(Text) when is_binary(Text) -> unicode:characters_to_list(Text);
code_point(Text) -> Text.

utf8(Text) when is_list(Text) -> unicode:characters_to_binary(Text);
utf8(Text) -> Text.     

replace(Word, Letter1, Letter2) -> 
      lists:map(
                  fun(X) -> 
                     if X =:= Letter1 -> Letter2; 
                        true -> X 
                     end 
                  end, Word
               ).


steps(Word, RV, R2) ->
    RWord = lists:reverse(Word),
    {Stem, _RVS, _R2S } = lists:foldl(fun (L, X) -> step(L, X) end, {RWord, RV, R2}, [?STEP1, ?STEP2, ?STEP3, ?STEP4] ),
    lists:reverse(Stem).

step(_StepRules, {WordR, 0, 0}) -> {WordR, 0, 0};
step(StepRules, {WordR, RV, R2}) ->
    {WordRes, RVS, R2S, _State}  = lists:foldl(fun (L, X) -> apply_step_rules(L, X) end, {WordR, RV, R2, continue}, StepRules),
    {WordRes, RVS, R2S}.    

apply_step_rules(StepRule, {WordR, RV, R2, State}) ->
    case State of
      ending -> {WordR, RV, R2, ending};
      skip -> {WordR, RV, R2, continue};
      continue ->
        {ListSuffix, GroupType, GroupRule, Region} = StepRule,
        {WordRes, RVRes, R2Res, ISRemoved} = remove_suffix(ListSuffix, GroupType, Region, WordR, RV, R2),
        if ISRemoved ->
          case GroupRule of  
            if_removed_then_end -> {WordRes, RVRes, R2Res, ending};
            try_then_continue ->   {WordRes, RVRes, R2Res, continue};
            if_removed_then_skip_next -> {WordRes, RVRes, R2Res, skip}
          end;
          true ->  {WordRes, RVRes, R2Res, continue}
        end    
    end.
            

remove_suffix(_ListSuffix, _GroupType, rv, WordR, 0, R2) ->
    {WordR, 0, R2, false};
remove_suffix(_ListSuffix, _GroupType, r2, WordR, RV, 0) ->
    {WordR, RV, 0, false};

remove_suffix(ListSuffix, GroupType, Region, WordR, RV, R2) ->
    RSuffix = find_next_suffix(WordR, ListSuffix),
    if RSuffix =:= not_found -> {WordR, RV, R2, false};
      true ->
        {Suffix, RListSuffix} = RSuffix,
        SuffixLn = length(Suffix),
        SuffixLnV = if GroupType =:= type1 -> SuffixLn - 1;
                       true ->  SuffixLn
                    end,     
        RF =  if Region =:= rv -> RV; 
                true -> R2 
              end,
        if  SuffixLn =< RF -> 
              RVRes = subtract(RV, SuffixLnV),
              R2Res = subtract(R2, SuffixLnV), 
              WordRes = lists:nthtail(SuffixLnV, WordR),
              {WordRes, RVRes, R2Res, true};
            true ->   
              remove_suffix(RListSuffix, GroupType, Region, WordR, RV, R2)
        end       
    end.

find_next_suffix(_Word, []) -> not_found;           
find_next_suffix(Word, [H|T]) ->
    V = code_point(H),
    case lists:prefix(V, Word) of
       true -> {V, T};
       false -> find_next_suffix(Word, T)
    end.    

subtract(A, B) ->
    if A > B -> A - B;
       true -> 0
    end.   



