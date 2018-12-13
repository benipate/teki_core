-module(teki).

-compile(export_all).
-define(DEBUG(Table,Format, DataList), case ets:lookup(Table, debug) of [{debug, true}]->  error_logger:info_msg(Format, DataList);  _->  ok end).


start()->
  teki_local_server:start_link().

-define(EFFECTS_BY_CAT(X), ets:select(effect, [{{'$1','$2'}, [{'==', X, {map_get, <<"effectCategory">>, '$2'}}], ['$1']}])).

new()->
  FitRef = gen_server:call(teki_local_server, new),
  ets:insert(FitRef,{offline_effects,[11,12,13,854]}),
  ets:insert(FitRef,{overload_effects,?EFFECTS_BY_CAT(5)}),
  FitRef.

get_type_key(Root, TypeID, Options, FitRef)->
  case lists:member(skill, Options) of
    true->
      {{TypeID, Root}, first};
    false->
      case ets:select(FitRef, [{{{type, {TypeID, '$1'}}, '$2', '$3', '$4', '_', '_'}, [], [{{'$1', '$2', '$4', '$3'}}]}]) of
        []->
          %% no such item on any root
          {{TypeID, 0}, first};
        List->
          case lists:filter(fun({_ID,_Root,_Options, _Amount})-> (_Root==Root) and (_Options==Options) end, List) of
            []->
              NewID = lists:max([X||{X, _, _, _}<-List])+1,
              {{TypeID, NewID}, first};
            [{_ID,_,_, Amount}]->
              {{TypeID, _ID}, Amount}
          end
      end
  end.

add({new,character}, CharacterID, _Options, FitRef)->
  teki_characters:use(CharacterID, FitRef),
  CharacterID;
add({new,ship,CharacterID}, TypeID, _Options, FitRef)->
  Options = lists:usort(_Options),
  {TypeKey,TypeAmount}=get_type_key(CharacterID, TypeID, Options, FitRef),
  ets:delete_object(FitRef,{ship, none, CharacterID}),
  ets:insert(FitRef,{ship,TypeKey, CharacterID}),
  add({ship, CharacterID}, TypeID, Options, FitRef), TypeKey;
add(Root, TypeID, _Options, FitRef)->
  %% set Charges
  Options = case lists:keyfind(charge,1,_Options) of
    false->
      lists:usort(_Options);
    {charge, ChargeTypeID}->
      ChargeTypeKey = add(Root, ChargeTypeID, [], FitRef),
      lists:keyreplace(charge,1,_Options, {charge,ChargeTypeKey})
  end,
  {TypeKey,TypeAmount}=get_type_key(Root, TypeID, Options, FitRef),
  ets:insert(FitRef,{modifier, add}),
  ets:insert(FitRef,{other,Root, TypeKey}),
  CharacterID = case ets:select(FitRef,[{{ship,Root, '$1'}, [], ['$1']}]) of
    []->
      Root;
    [_CharacterID|_]->_CharacterID
  end,
  case TypeAmount of
    first-> %% no typeid with this option exist
      [{TypeID,Type}]=ets:lookup(type_id, TypeID),
      [{TypeID,AttrMap}]=ets:lookup(type_attr, TypeID),
      [{TypeID,EffectsMap}]=case ets:lookup(type_effect, TypeID) of
        []->error_logger:info_msg("~p has no effects", [TypeID]),
          [{TypeID,#{}}];
        _Res->
          _Res
      end,
      %% get skill requerment
      SkillReqMap = get_skills_requirements(CharacterID, AttrMap, FitRef),
      %% add type record
      Amount = case lists:keyfind(amount,1,Options) of
        false->1;
        {amount, _Amount}->_Amount
      end,

      ets:insert(FitRef,  {{type, TypeKey}, Root, Amount, Options, SkillReqMap, Type}),
      %effects filter
      {AND,NOT} = case lists:member(offline, Options) of
          true ->
            case ets:lookup(FitRef, offline_effects) of
              []->
                {[], []};
              [{overload_effects,Offline_effects}]->
                {Offline_effects, []}
            end;
          false->
            case ets:lookup(FitRef, overload_effects) of
              []->
                {all, []};
              [{overload_effects,Overload_effects}]->
                {all, Overload_effects}
            end
        end,

      %% add effects
      maps:fold(fun(EffectID, Default, Acc)->
          case AND of
            all->
              set_effect(TypeKey,EffectID,lists:member(EffectID,NOT)==false, Options,FitRef);
            _->
              set_effect(TypeKey,EffectID,(lists:member(EffectID,AND)) and (lists:member(EffectID,NOT)==false),Options,FitRef)
      end  end,<<>>,EffectsMap),


      ets:update_element(FitRef, {type, TypeKey}, {4,Options}),

      %% add attr
      ReEffects = maps:fold(fun(AttrID, ValueMap, Acc)->
          Value = maps:fold(fun(_K,V,Acc)-> V end, undefined, ValueMap),
          set_attr(TypeKey, AttrID, Value,FitRef),
          [usefull_effects(AttrID, FitRef)|Acc]
        end,[],AttrMap),

      lists:foreach(fun(EffectKey)->apply_effect(EffectKey, FitRef) end,lists:usort(lists:flatten(ReEffects))),
      % apply effects
      maps:fold(fun(EffectID, Default, Acc)->
        apply_effect({effect, TypeKey,EffectID}, FitRef)
      end,<<>>,EffectsMap),
      TypeKey;
    _->
      ets:update_element(FitRef, {type,TypeKey}, {3, TypeAmount+1}),
      error_logger:info_msg("Added alredy known typeid ~p | ~p", [TypeKey,TypeAmount]),
      TypeKey
  end.

del({TypeID,_}=TypeKey, FitRef)->
  ets:insert(FitRef,{modifier, del}),
  [{TypeID,AttrMap}]=ets:lookup(type_attr, TypeID),
  [{TypeID,EffectsMap}]=case ets:lookup(type_effect, TypeID) of
    []->error_logger:info_msg("~p has no effects", [TypeID]),
      [{TypeID,#{}}];
    _Res->
      _Res
  end,
  maps:fold(fun(AttrID, ValueMap, Acc)->
    del_attr(TypeKey, AttrID, FitRef)
  end,[],AttrMap),
  maps:fold(fun(EffectID, Default, Acc)->
    apply_effect({effect, TypeKey,EffectID}, FitRef)
  end,<<>>,EffectsMap),
  ets:insert(FitRef,{modifier, add}),
  ets:delete(FitRef,{type, TypeKey}).


get_skills_requirements(CharacterID, AttrMap, FitRef)->
 lists:foldr(fun(AttrID,Acc)->
   case maps:get(AttrID, AttrMap, undefined) of
     undefined -> Acc;
     ValueMap->
      SkillID = to_int(ValueMap),
      case ets:lookup(type_attr, {SkillID,CharacterID}) of
        []->
          Acc#{SkillID => true};
        [{TypeID,AttrMap}]->
          Map = get_skills_requirements(CharacterID, AttrMap, FitRef),
          maps:merge(Acc, Map#{SkillID => true})
      end
   end end, #{}, [182,183,184]).

%% Atributes

set_attr(ItemKey, AttrID, Value, FitRef)->
  Stackable = case ets:lookup(attr, AttrID) of
    [{AttrID, #{<<"stackable">> := <<"true">>}}] ->
      false; %% no penalties
    _-> true
  end,
  ets:insert(FitRef, {{attr, ItemKey, AttrID}, Value, #{}, #{ <<"stackackable">> => Stackable}}), {attr, ItemKey, AttrID}.

del_attr(ItemKey, AttrID, FitRef)->
  ets:delete(FitRef, {attr, ItemKey, AttrID}).


compile_attr(AttrKey, FitRef)->
  element(1,dirty_compile_attr(AttrKey, FitRef)).
dirty_compile_attr({attr, TypeKey, AttrID}=AttrKey, FitRef)->
  Amount=get_type_amount(TypeKey, FitRef),
  case ets:lookup(FitRef, AttrKey) of
    []-> {0, Amount};
    [{_, OldValue, Modifier, Opt}]->
      case maps:get(<<"compiled">>, Opt, undefined) of
        undefined->
          {dirty_compile_attr2(TypeKey, OldValue, Modifier, FitRef), Amount};
        true->
          {OldValue, Amount}
      end
  end.

get_type_amount(TypeKey, FitRef)->
  case ets:select(FitRef, [{{{type, TypeKey},'_', '$2', '_', '_', '_'}, [], ['$2']}]) of
    []->0;
    [Res]->Res
  end.


-define(MOD_NAME_AND_FUN, [
  {<<"PreAssignment">>, fun(X,_Mul,Acc)-> X end},
  {<<"PreDiv">>, fun(X,Mul,Acc)-> Acc/math:pow(X,Mul) end},
  {<<"PreMul">>, fun(X,Mul,Acc)-> Acc*math:pow(X,Mul) end},
  {<<"ModAdd">>, fun(X,Mul,Acc)-> Acc+(X*Mul) end},
  {<<"ModSub">>, fun(X,Mul,Acc)-> Acc-(X*Mul) end},
  {<<"PostDiv">>, fun(X,Mul,Acc)-> Acc/math:pow(X,Mul) end},
  {<<"PostMul">>, fun(X,Mul,Acc)-> Acc*math:pow(X,Mul) end},
  {<<"PostPercent">>, fun(X,Mul,Acc)-> Acc*math:pow((1+X/100),Mul) end}
]).

dirty_compile_attr2(TypeKey, OldValue, Modifier, FitRef)->
  case maps:get(<<"PostAssignment">>, Modifier,undefined) of
    undefined->
      lists:foldl(fun({ModName,ModFun},Acc)->
        ModMap = maps:get(ModName, Modifier,#{}),
        apply_mod_map(Acc, ModFun, ModMap, FitRef)
      end ,OldValue,?MOD_NAME_AND_FUN);
    _PostAssignment->
      error_logger:info_msg("[COMPILE] ~p compiled with PostAssignment mod (result ~p)", [Modifier, _PostAssignment]),
      0
  end.


apply_mod_map(OldValue, ModFun, Map, FitRef)->
  Res1 = maps:fold(fun
    ({effect,TypeID,_} = _EffectID,{AttrKey,false}, {Acc,AccList})->
      {Value,Amount}=dirty_compile_attr(AttrKey, FitRef),
      {ModFun(Value,Amount,Acc),AccList};
    ({effect,TypeID,_} = _EffectID,{AttrKey,true}, {Acc,AccList})->
      Res=dirty_compile_attr(AttrKey, FitRef),
      {Acc,[Res|AccList]}
  end, {OldValue, []}, Map),
  element(1,apply_pen_attr(ModFun, Res1, FitRef)).

apply_pen_attr(ModFun, {Value,ValueList}, FitRef)->
  SortedValueList = lists:sort(fun({X,Ax},{Y,Ay})->  abs(X) >= abs(Y) end,ValueList),
  lists:foldl(fun({0,_ModAmount}, {AccValue, AccAmount})-> {AccValue, AccAmount};
    ({ModValue,ModAmount}, {AccValue, AccAmount})->
    lists:foldl(fun(X, {AccValue2, _Last})->
      Mul = math:exp(-math:pow((X/2.67),2)),
      ?DEBUG(FitRef,"[apply_pen_attr] ~p -> [PEN ~p] -> ~p", [ModValue, X, ModValue*Mul]),
      {ModFun(ModValue*Mul,1,AccValue2), X+1}
    end, {AccValue,AccAmount}, lists:seq(AccAmount,AccAmount+ModAmount-1))
  end, {Value,0}, SortedValueList).



%% Effects


usefull_effects(AttrID, FitRef)->
  ets:select(FitRef, [{{{effect,'$1','$2'}, '_', '$3'}, [{'==', true, {map_get, AttrID, '$3'}}], [{{effect,'$1','$2'}}]}]).


set_effect(TypeKey,EffectID, Options,FitRef)->
  set_effect(TypeKey,EffectID,true,Options,FitRef).
set_effect(TypeKey,EffectID,Active, Options,FitRef)->
  [{EffectID,#{<<"postExpression">> := PostExpression,<<"preExpression">> := PreExpression}=EffectMap}]=ets:lookup(effect, EffectID),
  Active2 = case {maps:get(<<"durationAttributeID">>,EffectMap, undefined),lists:member(passive, Options)} of
    {Duration, true} when Duration=/=undefined ->
      false;
    _->
      true
  end,
  ModifiersList = maps:from_list([{X,true}||#{<<"modifiedAttributeID">> := X}<-maps:get(<<"modifierInfo">>, EffectMap, [])]),
  NewPostExpression = teki:gen_expression_tree(PostExpression),
  #{<<"affected">> := Affected}=NewPreExpression = teki:gen_expression_tree(PreExpression),
  ets:insert(FitRef, {{effect,TypeKey,EffectID}, EffectMap#{<<"active">> => Active and Active2, <<"postExpression">> => NewPostExpression,<<"preExpression">> => NewPreExpression}, maps:merge(ModifiersList,Affected)}).



%apply_effect({effect,Key,16}, FitRef)->
  %ets:insert(FitRef, {debug, true}),
%  ShipKey = operand:ets_get(FitRef, ship, 2),
%  error_logger:info_msg("Before ~p  ~p / ~p",[Key, teki:compile_attr({attr,ShipKey, 48}, FitRef), teki:compile_attr({attr,ShipKey, 49}, FitRef)]),
%  Res = apply_effect({effect,Key,16}, [], FitRef),
%  error_logger:info_msg("After ~p / ~p",[teki:compile_attr({attr,ShipKey, 48}, FitRef), teki:compile_attr({attr,ShipKey, 49}, FitRef)]),
%  Res;
  %ets:insert(FitRef, {debug, false});
apply_effect(EffectKey, FitRef)->
  apply_effect(EffectKey, [], FitRef).
apply_effect({effect,TypeID, Effect}=EffectKey, ApplyToList, FitRef)->
  %%error_logger:info_msg("~p effect", [EffectKey]),
  ets:insert(FitRef, {self, TypeID, Effect}),
  ets:insert(FitRef, {currentself, TypeID, Effect}),
  ets:insert(FitRef, {apply_list, ApplyToList}),
  [{Effect,#{<<"preExpression">> := PreExpressionID}=EffectMap}]=ets:lookup(effect, Effect),
  [{EffectKey,#{<<"preExpression">> := PreExpression, <<"active">> := Active},_}]=ets:lookup(FitRef, EffectKey),
  ModifierInfo = maps:get(<<"modifierInfo">>, EffectMap, undefined),
  ModifierReady = modifier:handled_effect(element(3,EffectKey)),
  if
    (Active) and ((ModifierInfo=/=undefined)or(ModifierReady))->
      modifier:eval(FitRef, EffectKey, ModifierInfo);
    (Active) and (PreExpressionID=/=59)->
      operand:eval(FitRef, PreExpression);
    true->
      ok
  end.

gen_expression_tree(ID)->
  _Res = ets:lookup(expression, ID),
  case _Res of
    []->
      {#{},[]};
    [{ID, Res}]->
      OperandID = maps:get(<<"operandID">>, Res),
      AffectedAttrNodes = operand:affected(OperandID),
      NotAffectedNodes = [<<"arg2">>,<<"arg1">>]--AffectedAttrNodes,
      Map1 = lists:foldr(fun(AffectedAttrNode, Acc)->
        Res1 = case maps:get(AffectedAttrNode, Acc, undefined) of
          undefined->
            Acc#{<<"affected">> => #{maps:get(<<"expressionAttributeID">>, Acc, 0) => true}};
          Arg ->
            #{<<"affected">> := Affected}=_Res2=gen_expression_tree(Arg),
            Acc1 = maps:update_with(<<"affected">>, fun(V) -> maps:merge(V, Affected) end, Acc),
            maps:update(AffectedAttrNode, _Res2, Acc1)
        end
      end, Res#{<<"affected">> => #{}}, AffectedAttrNodes),
      Map2 = lists:foldr(fun(NotAffectedNode, Acc)->
        case maps:get(NotAffectedNode, Acc, undefined) of
          undefined->
            Acc;
          Arg ->
            maps:update(NotAffectedNode, gen_expression_tree(Arg), Acc)
        end
      end, Map1, NotAffectedNodes)
  end.

%% skills
-define(SkillCategoryID, 16).

get_user_skills(all5)->
  %% generate all5 user
  SkillGroupList = ets:select(group_id, [{{'$1',?SkillCategoryID}, [], ['$1']}]),
  lists:foldr(fun(GroupID, Acc)-> ets:select(type_id, [{ {'$1', '$2'},[{'==', GroupID, {map_get, <<"groupID">>,'$2'}}], ['$1']}]) ++ Acc end, [], SkillGroupList).

set_skill_level(SkillTypeID, CharID, Level, FitRef)->
  case ets:lookup(FitRef, {attr, {SkillTypeID,CharID}, 280}) of
    []->
      ets:insert(FitRef, {{attr, {SkillTypeID,CharID}, 280}, Level, #{}, #{ <<"compiled">> => true, <<"base">> => 0}});
    [{_, OldValue, Modifier, Opt}]->
      ets:insert(FitRef, {{attr, {SkillTypeID,CharID}, 280}, Level, Modifier, Opt#{ <<"compiled">> => true, <<"base">> => OldValue}})
  end.


%% support

to_int(Map) when is_map(Map)->
  to_int(maps:to_list(Map));
to_int([{ <<"valueFloat">> , Float}|_])->
  round(Float);
to_int([{ <<"valueInt">>, Int}|_])->
  Int.

has_penalty(AttrID) when is_atom(AttrID)-> false;
has_penalty(AttrID)->
  case ets:lookup(attr, AttrID) of
    [{AttrID, #{<<"stackable">> := <<"true">>}}]-> false;
    _-> true
  end.

pen_mod(<<"PreAssignment">>)->false;
pen_mod(<<"ModAdd">>)->false;
pen_mod(<<"ModSub">>)->false;
pen_mod(_)->true.

pen_type(FitRef,TypeKey)->
  (not is_skill(FitRef,TypeKey)) and
  (TypeKey=/=operand:ets_get(FitRef, ship, 2)).

is_skill(FitRef,TypeKey)->
  case ets:lookup(FitRef, {type, TypeKey}) of
    []-> false;
    [{_, _,_, Opts, _,_}]->
      lists:member(skill, Opts)
  end.

test()->
  A=teki:new(),
  Character = teki:add({new,character}, all5, [], A),
  ShipKey = teki:add({new,ship,Character}, 587, [], A),
  teki:add(ShipKey, 20347, [], A),
  A.

prep_eff_start(_Table)->
  Key = ets:first(effect),
  prep_eff(Key).

prep_eff('$end_of_table')->
  effect;
prep_eff(Key)->
  [{ID,Map}]=ets:lookup(effect, Key),
  case maps:get(<<"modifierInfo">>, Map, undefined) of
    undefined->
        ok;
    ModifierInfoBin->
        case fast_yaml:decode(ModifierInfoBin,[]) of
          {ok,[ModifierList]}->
            ets:insert(effect, {ID,Map#{<<"modifierInfo">> => lists:map(fun(X)->maps:from_list(X) end,ModifierList)}});
          {ok,[]}->
            ets:insert(effect, {ID,Map#{<<"modifierInfo">> => []}})
        end
  end,
  prep_eff(ets:next(effect, Key)).

prep_attr_start(_)->
  Key = ets:first(type_id),
  prep_attr(Key).
prep_attr('$end_of_table')->
  effect;
prep_attr(Key)->
  NewAttrMap = prep_attr2(Key),
  case  maps:size(NewAttrMap) of
    0->
      ok;
    _->
      ets:insert(type_attr, {Key,NewAttrMap})
  end,
  prep_attr(ets:next(type_id, Key)).

prep_attr2(Key)->
  [{ID,TypeMap}]=ets:lookup(type_id, Key),
  AttrMap = case ets:lookup(type_attr, Key) of
    [] ->
      #{};
    [{ID,_AttrMap}]->
      _AttrMap
  end,
  NewAttrMap = maps:fold(fun(K,V,Acc)->
    case ets:select(attr, [{{'$1','$2'}, [{'==', {const, K}, {map_get, <<"attributeName">>, '$2'}}], ['$1']}]) of
      []->
        Acc;
      [AttrID]->
        AttrValueMap = if
          is_integer(V) ->
            Acc#{AttrID => #{<<"valueInt">> => V}};
          is_float(V)->
            Acc#{AttrID => #{<<"valueFloat">> => V}};
          true->
            Acc
        end
    end end , AttrMap,TypeMap).

speed_test(FitRef)->
  B = timer:tc(?MODULE, add, [{34317,0},34319,[],FitRef]),
  A = timer:tc(?MODULE, del, [{34319,0}, FitRef]),
  {A,B}.

fork(FitRef)->
  NewFitRef = gen_server:call(teki_local_server, new),
  fork(FitRef, NewFitRef, ets:first(FitRef)).

fork(FitRef, NewFitRef, '$end_of_table')->
  NewFitRef;
fork(FitRef, NewFitRef, Key)->
  [Record] = ets:lookup(FitRef, Key),
  ets:insert(NewFitRef, Record),
  fork(FitRef, NewFitRef,ets:next(FitRef, Key)).


to_map(List, []) when is_list(List) ->
  to_map(List);
to_map(List, [{KeyID1,KeyID2}]) when is_list(List) ->
  Key1 = proplists:get_value(KeyID1, List),
  Key2 = proplists:get_value(KeyID2, List),
  #{Key1 => Key2};
to_map(List, [DropKeys|KeysList]) when is_list(List) and is_list(DropKeys) ->
  Res = lists:foldr(fun(X,Acc)-> proplists:delete(X,Acc) end,List, DropKeys),
  to_map(Res, KeysList);
to_map(List, [KeyID|KeysList]) when is_list(List) ->
  Key = proplists:get_value(KeyID, List),
  #{Key => to_map(proplists:delete(KeyID,List), KeysList)}.
to_map(List) when is_list(List) ->
  lists:foldr(fun({K,V},Acc)-> Acc#{K => to_map(V)}; (X,Acc)-> Acc end, #{},List);
to_map(List)->
  List.
