-module(modifier).

-compile(export_all).

-define(DEBUG(Table,Format, DataList), case ets:lookup(Table, debug) of [{debug, true}]->  error_logger:info_msg(Format, DataList);  _->  ok end).
-define(OPERATOR_NAME(X), maps:get(X,?OPERATORS)).
-define(OPERATORS, #{0 => <<"PreMul">>,
  1 => <<"PreDiv">>,
  2 => <<"ModAdd">>,
  3 => <<"ModSub">>,
  4 => <<"PostMul">>,
  5 => <<"PostDiv">>,
  6 => <<"PostPercent">>,
  7 => <<"PostAssignment">>}).

-define(HANDLED_EFFECTS, [6730, 6731,3774, 101, 4928]).

reactive_cycle(FitRef,TypeKey, Damage)->
  reactive_cycle(FitRef,TypeKey, Damage, 1).

reactive_cycle(FitRef,TypeKey, Damage, 0)->
  ok;
reactive_cycle(FitRef,TypeKey, Damage, N)->
  ShipID = operand:ets_get(FitRef, ship, 2),
  DamageTypeList = [267, 270, 269, 268],
  DamageValues = tuple_to_list(Damage),
  io:format("~n"),
  Res1 = lists:zipwith(fun(TypeID, DamageValue)->
    Resists = teki:compile_attr({attr,ShipID, TypeID}, FitRef),
    io:format("| ~p", [teki:compile_attr({attr,TypeKey, TypeID}, FitRef)]),
    {TypeID, DamageValue*Resists}
  end, DamageTypeList, DamageValues),
  io:format("~n"),
  AllDMG = lists:foldr(fun({_,X},Acc)-> X+Acc end,0, Res1),
  Res2 = lists:map(fun({Y,X})->{Y,X/AllDMG} end, Res1),
  Res3 = lists:keysort(2,Res2),
  lists:foldl(fun
    ({TypeID, DamageValue}, {4,_,Persent})->
      ModValue = modify_resist(FitRef, {attr, TypeKey, TypeID}, -Persent),
      %error_logger:info_msg("Add ~p resists to ~p", [ModValue, TypeID]),
      {4,0};
    ({TypeID, DamageValue}, {Step, 1,Persent})->
      ModValue = modify_resist(FitRef, {attr, TypeKey, TypeID}, 0.03),
      case ModValue of
          0->
            {Step+1, 1,Persent};
          _->
            {Step+1, 0,Persent+ModValue}
        end;
    ({TypeID, DamageValue}, {Step, 0,Persent})->
      if
        DamageValue < 0.15->
          ModValue = modify_resist(FitRef, {attr, TypeKey, TypeID}, 0.06),
          %error_logger:info_msg("remove ~p resists from ~p", [ModValue, TypeID]),
          case ModValue of
            0->
              {Step+1, 1,Persent};
            _->
              {Step+1, 0,Persent+ModValue}
          end;
        DamageValue < 0.25->
          ModValue = modify_resist(FitRef, {attr, TypeKey, TypeID}, 0.03),
          %error_logger:info_msg("remove ~p resists from ~p", [ModValue, TypeID]),
          {Step+1,0,Persent+ModValue};
        true->
          if
            Step==3 ->
              ModValue = modify_resist(FitRef, {attr, TypeKey, TypeID}, -Persent/2),
              %error_logger:info_msg("Add ~p resists from ~p", [ModValue, TypeID]),
              {Step+1,0,Persent+ModValue};
            true->
              {Step+1,0,Persent}
          end
      end
  end, {1,0,0},Res3),
  reactive_cycle(FitRef,TypeKey, Damage, N-1).

modify_resist(FitRef, Attr, Int)->
  modify_attr(FitRef, Attr, fun(X) -> Res = X+Int, if (Res>1) -> if ((X+Int/2)>1) -> {X, 0}; true-> {X+Int/2, Int/2} end; true-> {Res, Int} end end).
modify_attr(FitRef, Attr, Fun)->
  [{Attr, Value, _, _}]=ets:lookup(FitRef, Attr),
  {NewValue,Acc}=Fun(Value),
  ets:update_element(FitRef, Attr, {2,NewValue}),Acc.

handled_effect()->
  ?HANDLED_EFFECTS.
handled_effect(X)->
  lists:member(X,?HANDLED_EFFECTS).

handle_effect(FitRef,TypeKey,EffectTypeID)->
  [{_,EffectMap}]=ets:lookup(effect, EffectTypeID),
  handle_effect(FitRef,TypeKey,EffectTypeID,EffectMap).

handle_effect(FitRef,TypeKey,4928,EffectMap)->
  ShipID = operand:ets_get(FitRef, ship, 2),
  set_modifiers(FitRef, [ %% set serists to get pen from dc
    {<<"PreMul">>, {attr,ShipID, 268}, {attr, TypeKey, 268}},
    {<<"PreMul">>, {attr,ShipID, 269}, {attr, TypeKey, 269}},
    {<<"PreMul">>, {attr,ShipID, 270}, {attr, TypeKey, 270}},
    {<<"PreMul">>, {attr,ShipID, 267}, {attr, TypeKey, 267}}
  ]);
handle_effect(FitRef,TypeKey,6730,EffectMap)->
  error_logger:info_msg("Enable ~p as microwarpdrive", [TypeKey]);
handle_effect(FitRef,TypeKey,101,EffectMap)->
  ShipID = operand:ets_get(FitRef, ship, 2),
  [{Root, Amount, Opt}]=ets:select(FitRef, [{{{type, TypeKey}, '$1', '$2', '$3', '_', '_'}, [], [{{'$1', '$2', '$3'}}]}]),
  ets:insert(FitRef, {{attr, TypeKey, dps}, 0, #{}, #{}}),
  ets:insert(FitRef, {{attr, TypeKey, alpha}, 0, #{}, #{}}),
  ets:insert(FitRef, {{attr, TypeKey, maxrounds}, 0, #{}, #{}}),
  ets:insert(FitRef, {{attr, TypeKey, dpscycle}, 0, #{}, #{}}),
  case proplists:get_value(charge, Opt, none) of
    none ->
      true;
    ChargeKey ->
      set_modifiers(FitRef, [
        {<<"PreAssignment">>, {attr,TypeKey, alpha}, {attr, ChargeKey, alpha}},
        {<<"PreAssignment">>, {attr,TypeKey, dps}, {attr, TypeKey, alpha}},

        %% maxrounds = capacity/(volume*chargeRate) -    38/(161*)

        {<<"PreAssignment">>, {attr,TypeKey, maxrounds}, {attr, TypeKey, 38}},
        {<<"PostDiv">>, {attr,TypeKey, maxrounds}, {attr, ChargeKey, 161}},
        {<<"PostDiv">>, {attr,TypeKey, maxrounds}, {attr, TypeKey, 56}},

        %dps = maxrounds*alpha/dpscycle

        {<<"PostMul">>, {attr,TypeKey, dps}, {attr, TypeKey, maxrounds}},
        {<<"PostDiv">>, {attr,TypeKey, dps}, {attr, TypeKey, dpscycle}},

        %%dpscycle = rof*maxrounds+reloadtime

        {<<"PreAssignment">>, {attr,TypeKey, dpscycle}, {attr, TypeKey, maxrounds}},
        {<<"PreMul">>, {attr,TypeKey, dpscycle}, {attr, TypeKey, 51}},
        {<<"ModAdd">>, {attr,TypeKey, dpscycle}, {attr, TypeKey, 1795}}
      ])
  end;
handle_effect(FitRef,TypeKey,3774,EffectMap)->
  ShipID = operand:ets_get(FitRef, ship, 2),
  set_modifiers(FitRef, [
    {<<"ModAdd">>, {attr,ShipID, 101}, {attr, TypeKey, 1369}}, %%launcherSlotsLeft
    {<<"ModAdd">>, {attr,ShipID, 102}, {attr, TypeKey, 1368}}, %%turretSlotsLeft
    {<<"ModAdd">>, {attr,ShipID, 12}, {attr, TypeKey, 1376}}, %%lowSlots
    {<<"ModAdd">>, {attr,ShipID, 13}, {attr, TypeKey, 1375}}, %%medSlots
    {<<"ModAdd">>, {attr,ShipID, 14}, {attr, TypeKey, 1374}} %%hiSlots
  ]);
handle_effect(FitRef,TypeKey,6731,EffectMap)->
  ShipID = operand:ets_get(FitRef, ship, 2),
  %% fake attr
  ets:insert(FitRef, {{attr, TypeKey, speedboost}, 0, #{}, #{}}),
  set_modifiers(FitRef, [
    {<<"ModAdd">>, {attr,ShipID, 4}, {attr, TypeKey, 796}},
    {<<"PostPercent">>, {attr,ShipID, 37}, {attr, TypeKey, speedboost}},
    {<<"PreAssignment">>, {attr,TypeKey, speedboost}, {attr, TypeKey, 20}},
    {<<"PostDiv">>, {attr,TypeKey, speedboost}, {attr, ShipID, 4}},
    {<<"PostMul">>, {attr,TypeKey, speedboost}, {attr, TypeKey, 567}}
  ]),
  error_logger:info_msg("Enable ~p as afterburner", [TypeKey]).


set_modifiers(FitRef, ModList)->
  lists:foreach(fun({ModType,Attr,ModifierKey})-> operand:set_modifier(FitRef, {modifier,ModType,Attr}, <<>>, ModifierKey) end,ModList).


eval(A,{effect,_,_}=Key,undefined)->
  eval(A,Key,[]);
eval(A,{effect,TypeKey,EffectTypeID},[])->
  case lists:member(EffectTypeID,?HANDLED_EFFECTS) of
    true->
      handle_effect(A,TypeKey,EffectTypeID);
    false->
      true
  end;
eval(A,{_,_,6641}=_EffectKey,[Map|ModList])->
  %ets:insert(A, {debug, true}),
  %?DEBUG(A," [6641] ~p", [Map]),
  mod(A,_EffectKey,Map),
  %ets:insert(A, {debug, false}),
  eval(A,_EffectKey,ModList);
eval(A,_EffectKey,[Map|[]])->
  mod(A,_EffectKey,Map);
eval(A,_EffectKey,[Map|ModList])->
  mod(A,_EffectKey,Map),
  eval(A,_EffectKey,ModList);
eval(A,1,1)->
  ok.



mod(FitRef,_EffectKey, #{<<"domain">> := _Root, <<"func">> := <<"LocationRequiredSkillModifier">>,
  <<"modifiedAttributeID">> := ModifiedAttributeID, <<"modifyingAttributeID">> := ModifyingAttributeID,
  <<"operator">> := Operator,<<"skillTypeID">> := SkillTypeID})->
  Root = operand:eval(FitRef, #{<<"operandID">> => 24, <<"expressionValue">> => _Root}),
  [{currentself, SelfId, _}] = ets:lookup(FitRef, currentself),
  Res = ets:select(FitRef, [{{{type, '$1'},Root, '_', '_', '$2', '_'}, [{'==', true, {map_get, {const, SkillTypeID}, '$2'}}], ['$1']}]),
  AttrList = [{attr, X, ModifiedAttributeID}||X<-Res],
  ?DEBUG(FitRef," [6641 mod] ~p", [{modifier,?OPERATOR_NAME(Operator),AttrList}]),
  operand:set_modifier(FitRef, {modifier,?OPERATOR_NAME(Operator),AttrList}, <<"LocationRequiredSkillModifier">>, {attr, SelfId, ModifyingAttributeID});

mod(FitRef,_EffectKey, #{<<"domain">> := _Root, <<"func">> := <<"OwnerRequiredSkillModifier">>,
  <<"modifiedAttributeID">> := ModifiedAttributeID, <<"modifyingAttributeID">> := ModifyingAttributeID,
  <<"operator">> := Operator,<<"skillTypeID">> := SkillTypeID})->
  Root = operand:eval(FitRef, #{<<"operandID">> => 24, <<"expressionValue">> => _Root}),
  [{currentself, SelfId, _}] = ets:lookup(FitRef, currentself),
  Ships = ets:select(FitRef, [{{ship,'$1', Root}, [], ['$1']}]),
  Res = lists:flatten([ets:select(FitRef, [{{{type, '$1'},Ship, '_', '_', '$2', '_'}, [{'==', true, {map_get, {const, SkillTypeID}, '$2'}}], ['$1']}])||Ship<-Ships]),
  AttrList = [{attr, X, ModifiedAttributeID}||X<-Res],
  operand:set_modifier(FitRef, {modifier,?OPERATOR_NAME(Operator),AttrList}, <<"OwnerRequiredSkillModifier">>, {attr, SelfId, ModifyingAttributeID});

mod(FitRef,_EffectKey, #{<<"domain">> := _Root, <<"func">> := <<"LocationGroupModifier">>,
  <<"modifiedAttributeID">> := ModifiedAttributeID, <<"groupID">> := GroupID,  <<"modifyingAttributeID">> := ModifyingAttributeID,
  <<"operator">> := Operator}=A)->
  Root = operand:eval(FitRef, #{<<"operandID">> => 24, <<"expressionValue">> => _Root}),
  [{currentself, SelfId, _}] = ets:lookup(FitRef, currentself),
  Res = ets:select(FitRef, [{{{type, '$1'}, Root, '_', '_', '_', '$2'}, [{'==', GroupID, {map_get, <<"groupID">>, '$2'}}], ['$1']}]),
  lists:foreach(fun(X)->
    operand:set_modifier(FitRef, {modifier,?OPERATOR_NAME(Operator),{attr, X, ModifiedAttributeID}}, <<"LocationGroupModifier">>, {attr, SelfId, ModifyingAttributeID})
 end, Res);
mod(FitRef,_EffectKey, #{<<"domain">> := _Root, <<"func">> := <<"ItemModifier">>,
  <<"modifiedAttributeID">> := ModifiedAttributeID, <<"modifyingAttributeID">> := ModifyingAttributeID,
  <<"operator">> := Operator}=A)->
  Root = operand:eval(FitRef, #{<<"operandID">> => 24, <<"expressionValue">> => _Root}),
  case Root of
    undefined -> throw(A);
    _->
      [{currentself, SelfId, _}] = ets:lookup(FitRef, currentself),
      operand:set_modifier(FitRef, {modifier,?OPERATOR_NAME(Operator),{attr, Root, ModifiedAttributeID}}, <<"ItemModifier">>, {attr, SelfId, ModifyingAttributeID})
  end.
