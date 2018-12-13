-module(operand).

-compile(export_all).

-define(DEBUG(Table,Format, DataList), case ets:lookup(Table, debug) of [{debug, true}]->  error_logger:info_msg(Format, DataList);  _->  ok end).
-define(ARG2_LIST, [12,31]).
-define(ALL_LIST, [17, 52]).

affected(X)->
  case lists:member(X, ?ALL_LIST) of
    true->
      [<<"arg2">>,<<"arg1">>];
    false->
      case lists:member(X, ?ARG2_LIST) of
        true->
          [<<"arg2">>];
        false->
          [<<"arg1">>]
      end
  end.

ets_get(Table, Key, Element)->
  Res = ets_get(Table, Key),
  if
    is_tuple(Res) ->
      element(Element,Res);
    true->
      throw({badarg, {?MODULE, ets_get,Table, Key, Element}, Res})
  end.
ets_get(Table, Key)->
  case ets:lookup(Table, Key) of
    [Res]->
      Res;
    _Res->
      throw({badarg, {?MODULE, ets_get,Table, Key},_Res})
  end.

%% add two numbers
eval(FitRef, #{<<"operandID">> := 1, <<"arg1">> := Arg1, <<"arg2">> := Arg2})->
  eval(FitRef, Arg1)+eval(FitRef, Arg2);
%% logical and operation
eval(FitRef, #{<<"operandID">> := 10, <<"arg1">> := Arg1, <<"arg2">> := Arg2})->
  Res1 = eval(FitRef, Arg1),
  if
    Res1 ->
      eval(FitRef, Arg2);
    true->
      false
  end;
% add owner required skill modifier
eval(FitRef, #{<<"operandID">> := 11, <<"arg1">> := Arg1,  <<"arg2">> := Arg2})->
  Modifier = eval(FitRef, Arg1),
  [{currentself, SelfId, _}] = ets:lookup(FitRef, currentself),
  ModifierAttrKey = {attr, SelfId, eval(FitRef, Arg2)},
  set_modifier(FitRef, Modifier, <<"AORSM(11)">>, ModifierAttrKey);

%add gang groupl modifier
% eval(#{<<"operandID">> := 2, <<"arg1">> := Arg1, <<"arg2">> := Arg2})->
eval(FitRef, #{<<"operandID">> := 12, <<"arg1">> := Arg1, <<"arg2">> := Arg2})->
  Root = eval(FitRef, Arg1),
  Res2 = eval(FitRef, Arg2),
  ?DEBUG(FitRef," [Operand | 12] ~p,  ~p)", [Root, Res2]),
  if
    is_list(Root)-> %% working with groups of item
      lists:map(fun(RootID)->
        _Arg2 = eval(FitRef, Arg2),
        case ets:lookup(FitRef, {attr, RootID, _Arg2}) of
          []->
            %%error_logger:info_msg("~p not found", [{attr, RootID, _Arg2}]),
            0;
          [{AttrId, _, _, _}]->
            AttrId
        end end, Root);
    true->
      case ets:lookup(FitRef, {attr, Root, eval(FitRef, Arg2)}) of
        []->
          undefined;
        [{AttrId, _, _, _}]->AttrId
      end
  end;
%%executes two statements
eval(FitRef, #{<<"operandID">> := 17, <<"arg1">> := Arg1, <<"arg2">> := Arg2})->
  eval(FitRef, Arg1),
  eval(FitRef, Arg2);
%% "define attribute association type",
eval(FitRef, #{<<"operandID">> := 21, <<"expressionValue">> := Value})->
  Value;
%% "define attribute"
eval(FitRef, #{<<"operandID">> := 22, <<"expressionAttributeID">> := AttrId})->
  AttrId;
%% define bool constant
eval(FitRef, #{<<"operandID">> := 23, <<"expressionValue">> := Value})->
  Value==1;
%% defines a float constant
eval(FitRef, #{<<"operandID">> := 25, <<"expressionValue">> := Value})->
  Value;
%% define environment index
eval(FitRef, #{<<"operandID">> := 24, <<"expressionValue">> := Value})->
  case Value of
    <<"Ship">> ->
      ets_get(FitRef, ship, 2);
    <<"shipID">> ->
      ets_get(FitRef, ship, 2);
    <<"CurrentShip">> ->
      ets_get(FitRef, currentship, 2);
    <<"Self">> ->
      ets_get(FitRef, self, 2);
    <<"Char">> ->
      ets_get(FitRef, character, 2);
    <<"charID">> ->
      ets_get(FitRef, character, 2);
    <<"CurrentSelf">> ->
      ets_get(FitRef, currentself, 2);
    <<"null">> ->
      ets_get(FitRef, self, 2);
    <<"itemID">> ->
      ets_get(FitRef, self, 2);
    <<"Other">> ->
      SelfKey=ets_get(FitRef, currentself, 2),
      case ets:select(FitRef, [{{other,'$1', SelfKey}, [], ['$1']}]) of
        []->
          undefined;
        [Root]->
          Root
      end;
    <<"otherID">> ->
      SelfKey=ets_get(FitRef, currentself, 2),
      case ets:select(FitRef, [{{other,'$1', SelfKey}, [], ['$1']}]) of
        []->
          undefined;
        [Root]->
          Root
      end;
    <<"structureID">> ->
      [];
    <<"Target">> ->
      undefined
  end;
%% define group
eval(FitRef, #{<<"operandID">> := 26, <<"expressionGroupID">> := Value})->
  Value;
eval(FitRef, #{<<"operandID">> := 26, <<"expressionName">> := Value})->
  Value;
%% defines an int constant
eval(FitRef, #{<<"operandID">> := 27, <<"expressionValue">> := Value})->
  Value;
%% defines a string constant
eval(FitRef, #{<<"operandID">> := 28, <<"expressionValue">> := Value})->
  Value;
%% define a type ID
eval(FitRef, #{<<"operandID">> := 29, <<"expressionTypeID">> := Value})->
  Value;


%% define association type
eval(FitRef, #{<<"operandID">> := 31, <<"arg1">> := Arg1, <<"arg2">> := Arg2})->
  {modifier, eval(FitRef, Arg1),  eval(FitRef, Arg2)};
eval(FitRef, #{<<"operandID">> := 35, <<"arg1">> := Arg1, <<"arg2">> := Arg2})->
  Type = eval(FitRef, Arg1),
  Attr = eval(FitRef, Arg2),
  teki:compile_attr({attr, Type, Attr}, FitRef);
%% gets type of item
eval(FitRef, #{<<"operandID">> := 36, <<"arg1">> := Arg1})->
  eval(FitRef, Arg1);  %[{self, SelfId, EffectId}]=ets:lookup(FitRef, self), SelfId;
%% checks whether expression 1  is greater than expression 2
eval(FitRef, #{<<"operandID">> := 38, <<"arg1">> := Arg1, <<"arg2">> := Arg2})->
  eval(FitRef, Arg1) > eval(FitRef, Arg2);
%% checks whether an expression is greater than or equal to another
eval(FitRef, #{<<"operandID">> := 39, <<"arg1">> := Arg1, <<"arg2">> := Arg2})->
  eval(FitRef, Arg1) >= eval(FitRef, Arg2);
%% if operator
eval(FitRef, #{<<"operandID">> := 41, <<"arg1">> := Arg1, <<"arg2">> := Arg2})->
  Res = eval(FitRef, Arg1),
  if
    Res ->
      eval(FitRef, Arg2);
    true->
      false
  end;

%LaunchMissile
eval(FitRef, #{<<"operandID">> := 44, <<"arg1">> := Arg1,  <<"expressionName">> := <<"LaunchMissile()">>})->
  Res = eval(FitRef, Arg1),
  if
    Res ->
        ShipKey = ets_get(FitRef, ship, 2),
        TypeKey = ets_get(FitRef, self, 2),
        ets:insert(FitRef, {{attr, TypeKey, alpha}, 0, #{}, #{}}),
        ets:insert(FitRef, {{attr, TypeKey, 54}, 0, #{}, #{}}), %% optimal
        modifier:set_modifiers(FitRef, [

        {<<"ModAdd">>, {attr,TypeKey, alpha}, {attr, TypeKey, 114}},
        {<<"ModAdd">>, {attr,TypeKey, alpha}, {attr, TypeKey, 116}},
        {<<"ModAdd">>, {attr,TypeKey, alpha}, {attr, TypeKey, 117}},
        {<<"ModAdd">>, {attr,TypeKey, alpha}, {attr, TypeKey, 118}},

        {<<"PreAssignment">>, {attr,TypeKey, 54}, {attr, TypeKey, 37}}, %% optimal
        {<<"PostMul">>, {attr,TypeKey, 54},{attr, TypeKey, 281}}
        ]);
    true->
      false
  end;

%% location - groupid item group
eval(FitRef, #{<<"operandID">> := 48, <<"arg1">> := Arg1, <<"arg2">> := Arg2})->
  Root  = eval(FitRef, Arg1),
  GroupID  = eval(FitRef, Arg2),
  Res = ets:select(FitRef, [{{{type, '$1'}, Root, '_', '_', '_', '$2'}, [{'==', GroupID, {map_get, <<"groupID">>, '$2'}}], ['$1']}]),
  ?DEBUG(FitRef," [Operand | 48] Found ~p modified with ~p groupid", [length(Res), GroupID]),
  Res;
%% location - skill required item group
eval(FitRef, #{<<"operandID">> := 49, <<"arg1">> := Arg1, <<"arg2">> := Arg2})->
  Root  = eval(FitRef, Arg1),
  SkillId  = eval(FitRef, Arg2),
  Res =ets:select(FitRef, [{{{type, '$1'},Root, '_', '_', '$2', '_'}, [{'==', true, {map_get, {const, SkillId}, '$2'}}], ['$1']}]),
  ?DEBUG(FitRef," [Operand | 48] looking for ~p skillreq  in ~p groupid = ~p", [SkillId, Root, Res]),
  Res;
% multiplies two numbers
eval(FitRef, #{<<"operandID">> := 51, <<"arg1">> := Arg1, <<"arg2">> := Arg2})->
  eval(FitRef, Arg1)*eval(FitRef, Arg2);
% logical OR operation
eval(FitRef, #{<<"operandID">> := 52, <<"arg1">> := Arg1, <<"arg2">> := Arg2}=A)->
  Eval = true =/= eval(FitRef, Arg1),
  if
    Eval->
      eval(FitRef, Arg2);
    true->
      true
  end;
%% CapasitorBoost
eval(FitRef, #{<<"operandID">> := 53}=A)->
  [{currentself,TypeKey,_}] = ets:lookup(FitRef, currentself),
  [{Root, Amount, Opt}]=ets:select(FitRef, [{{{type, TypeKey}, '$1', '$2', '$3', '_', '_'}, [], [{{'$1', '$2', '$3'}}]}]),
  case proplists:get_value(charge, Opt, none) of
    none ->
      true;
    ChargeKey ->
      ets:insert(FitRef, {{attr, TypeKey, cappersecond}, 0, #{}, #{}}),
      ets:insert(FitRef, {{attr, TypeKey, cappersecond2}, 0, #{}, #{}}),
      modifier:set_modifiers(FitRef, [
        {<<"PreAssignment">>, {attr,TypeKey, cappersecond}, {attr, ChargeKey, 67}},
        {<<"PostMul">>, {attr,TypeKey, cappersecond},{attr, TypeKey, 38}},
        {<<"PostDiv">>, {attr,TypeKey, cappersecond},{attr, ChargeKey, 128}},
        {<<"PostDiv">>, {attr,TypeKey, cappersecond},{attr, TypeKey, cappersecond2}},

        {<<"PreAssignment">>, {attr,TypeKey, cappersecond2}, {attr, TypeKey, 73}},
        {<<"PreDiv">>, {attr,TypeKey, cappersecond2}, {attr, ChargeKey, 128}},
        {<<"PreMul">>, {attr,TypeKey, cappersecond2}, {attr, TypeKey, 38}},
        {<<"ModAdd">>, {attr,TypeKey, speedboost}, {attr, TypeKey, 1795}}
      ]),
      true
  end;

%% 6 .AddItemModifier
eval(FitRef, #{<<"operandID">> := 6, <<"arg1">> := Arg1, <<"arg2">> := Arg2})->
  Res1= eval(FitRef, Arg1),
  Res2= eval(FitRef, Arg2),
  ?DEBUG(FitRef," [Operand | 6] set_modifier(~p, ~p, ~p)", [Res1, <<"AddItemModifier(6)">>, Res2]),
  case Res1 of
    {modifier,Res,undefined}->
      {modifier,Res,undefined};
    Modifier ->
      [{currentself, SelfId, _}] = ets:lookup(FitRef, currentself),
      ModifierAttrKey = {attr, SelfId, Res2},
      set_modifier(FitRef, Modifier, <<"AddItemModifier(6)">>, ModifierAttrKey)
  end;

%%sets an item attribute
eval(FitRef, #{<<"operandID">> := 65, <<"arg1">> := Arg1, <<"arg2">> := Arg2})->
  AttrId = eval(FitRef, Arg1),
  ets:update_element(FitRef, AttrId, [{2,eval(FitRef, Arg2)}]);

%% 67 "SkillCheck(Arg1)
eval(FitRef, #{<<"operandID">> := 67, <<"arg1">> := Arg1})->
  [{self, SelfId, _}] = ets:lookup(FitRef, self),
  SkillCheckType = eval(FitRef, Arg1),
  skill_check(FitRef, SelfId, SkillCheckType);
%% 7
eval(FitRef, #{<<"operandID">> := 7, <<"arg1">> := Arg1,  <<"arg2">> := Arg2})->
  Modifier = eval(FitRef, Arg1),
  [{currentself, SelfId, _}] = ets:lookup(FitRef, currentself),
  ModifierAttrKey = {attr, SelfId, eval(FitRef, Arg2)},
  set_modifier(FitRef, Modifier, <<"AddLocationGroupModifier(7)">>, ModifierAttrKey);
eval(FitRef, #{<<"operandID">> := 73, <<"arg1">> := Arg1})->
  SelfKey = operand:ets_get(FitRef, self, 2),
  Key = operand:ets_get(FitRef, ship, 2),
  A = {teki:compile_attr({attr,Key, 49}, FitRef), teki:compile_attr({attr,Key, 48}, FitRef)},
  throw({73, Arg1, A});
%% 8
eval(FitRef, #{<<"operandID">> := 8, <<"arg1">> := Arg1,  <<"arg2">> := Arg2})->
  Modifier = eval(FitRef, Arg1),
  [{currentself, SelfId, _}] = ets:lookup(FitRef, currentself),
  ModifierAttrKey = {attr, SelfId, eval(FitRef, Arg2)},
  set_modifier(FitRef, Modifier, <<"AddLocationModifier(8)">>, ModifierAttrKey);


%% add location required skill modifier
eval(FitRef, #{<<"operandID">> := 9, <<"arg1">> := Arg1,  <<"arg2">> := Arg2})->
  Modifier = eval(FitRef, Arg1),
  Res2= eval(FitRef, Arg2),
  [{currentself, SelfId, _}] = ets:lookup(FitRef, currentself),
  ModifierAttrKey = {attr, SelfId, eval(FitRef, Arg2)},
  ?DEBUG(FitRef," [Operand | 9] AddLocationRequiredSkillModifier(~p, ~p)", [Modifier, ModifierAttrKey]),
  set_modifier(FitRef, Modifier, <<"AddLocationRequiredSkillModifier(9)">>, ModifierAttrKey);

eval(FitRef, #{<<"expressionName">> := <<"EMPWave">>, <<"expressionValue">> := _, <<"operandID">> := 32})->
  ok;

eval(FitRef, Map)->
  throw({unkown_operand, Map}).

set_modifier(FitRef, {modifier,ModType,AttrList}, _ModType, ModifierKey) when is_list(AttrList)->
  lists:foreach(fun(Attr)-> set_modifier(FitRef, {modifier,ModType,Attr}, _ModType,  ModifierKey) end, AttrList);
set_modifier(FitRef, {modifier,ModType,Attr}, _ModType, ModifierKey)->
  case ets:lookup(FitRef, Attr) of
    []->  ?DEBUG(FitRef," [Empty Modifier] ~p", [Attr]),
    0;
    [{{attr,_TypeKey,_AttrID}=Attr, _Value,  CurentModifiersMap, _}]->
      [SelfID]=ets:lookup(FitRef, self),
      ModifierValue = case ets:lookup(FitRef, ModifierKey) of
        [{ModifierKey, _ModifierValue, _, _}]->
          _ModifierValue;
        []->
          undef
      end,
      CurrentModTypeMap = maps:get(ModType, CurentModifiersMap, #{}),
      Newmap = case ets:lookup(FitRef, modifier) of
        [{modifier, add}]->
          {attr,TypeKey,AttrID} = ModifierKey,
          _Newmap = CurentModifiersMap#{ModType => CurrentModTypeMap#{setelement(1,SelfID, effect) => {ModifierKey, penalized(FitRef, ModType, ModifierValue, TypeKey, AttrID, _AttrID)}}},
          ?DEBUG(FitRef," [Upd Modifier] ~p", [_Newmap]),
          _Newmap;
        [{modifier, del}]->
          _Newmap = CurentModifiersMap#{ModType => maps:remove(setelement(1,SelfID, effect), CurrentModTypeMap)},
          ?DEBUG(FitRef," [Upd Modifier] ~p", [_Newmap]),
          _Newmap
      end,
      %[AttrList]=ets:lookup(FitRef, changed_attr),
      %ets:insert(FitRef, {changed_attr, [Attr|AttrList]}),
      ets:update_element(FitRef, Attr, {3, Newmap})
  end.

penalized(FitRef, ModType, ModifierValue, TypeKey, AttrID, _AttrID)->
  (teki:pen_type(FitRef,TypeKey)) and
  (teki:pen_mod(ModType)) and
  (not pen_value(AttrID, ModifierValue)) and
  (teki:has_penalty(_AttrID)).

pen_value(AttrID, ModifierValue)->
  case ets:lookup(attr, AttrID) of
    [{AttrID, #{<<"defaultValue">> := Default}}]-> Default==ModifierValue;
    _-> false
  end.

skill_check(FitRef, SelfId, <<"OnlineHasSkillPrerequisites">>)->
    [{TypeID,AttrMap}]=ets:lookup(type_attr, element(1,SelfId)),
    lists:foldr(fun({AttrID,SkillLevelReqAttrID},Acc)->
      case maps:get(AttrID, AttrMap, undefined) of
        undefined -> Acc;
        ValueMap->
          SkillID = teki:to_int(ValueMap),
          SkillLevelReq = maps:get(SkillLevelReqAttrID, AttrMap, 0),
          case ets:lookup(FitRef, {attr, SkillID, 280}) of
            []->
              0 >= SkillLevelReq;
            [{_, SkillLevel, _, #{ <<"compiled">> := true}}] ->
              SkillLevel >= SkillLevelReq;
            [{_, _, _, _}] ->
              SkillLevel= teki:compile_attr({attr, SkillID, 280}, FitRef),
              SkillLevel >= SkillLevelReq
          end
      end
    end, true, [{182, 287},{183, 288},{184, 279}]).
