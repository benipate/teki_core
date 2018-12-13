-module(teki_characters).

-export([start_link/0, use/2]).
%% gen_server callbacks
-export([init/1, handle_call2/5, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


use(Character, ETS)-> %% blocking call, TODO cast variant
  {ReceiveToken, Timeout}= gen_server:call(?MODULE, {use, Character, ETS}),
  receive
    {?MODULE, ReceiveToken, Result}->
      Result
  after
    Timeout ->
      timeout
  end.

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
%%====================================================================
%% gen_server callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
  CharactersDir = application:get_env(teki, characters_dir ,"characters"),
  {ok, load_characters(CharactersDir)}.
%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({use, Character, ETS}, {FromPid,_Ref}= _From, State) ->
  case maps:get(Character, State, []) of
    []->
      {reply, {error, 1}, State};
    {id, CharacterID} ->
      handle_call({use, CharacterID, ETS}, _From, State);
    ETSTable ->
      Token = crypto:strong_rand_bytes(2),
      spawn(?MODULE,handle_call2, [ETSTable, ETS, Token, self(), FromPid]),
      {reply, {Token, 20000}, State}
  end;
handle_call({generate, Character}, _From, State) ->
  CharcterETS = ets:new(character, [public]), %% public table for now, shoud be protected with owner managment
  gen_character(Character, CharcterETS),
  save_character_table(CharcterETS),
  {reply, {Character, Character}, State#{Character => CharcterETS}};
handle_call(esi_load, _From, State) ->
  Url = esi_box:get_auth_url("skill"),
  {reply, Url, State};
handle_call({esi_load, Code}, _From, State) ->
  case catch esi_box:auth(Code) of
    {CharacterID, CharacterName}->
      CharcterETS = ets:new(character, [public]), %% public table for now, shoud be protected with owner managment
      gen_character({CharacterID, CharacterName}, CharcterETS),
      save_character_table(CharcterETS),
      {reply, {CharacterID, CharacterName}, State#{CharacterID => CharcterETS, CharacterName => {id, CharacterID}}};
    Error->
      {reply, {error, Error}, State}
  end;
handle_call(_Request, _From, State) ->
  Reply = maps:get(_Request, State, undefined),
  {reply, Reply, State}.

handle_call2(FromETS, ToETS, ReceiveToken, ServerPID, From)->
  Key = ets:first(FromETS),
  handle_call2(Key, FromETS, ToETS, ReceiveToken, ServerPID, From).
handle_call2('$end_of_table', FromETS, ToETS, ReceiveToken, ServerPID, From)->
  From ! {?MODULE, ReceiveToken, done};
handle_call2(Key, FromETS, ToETS, ReceiveToken, ServerPID, From)->
  ets:insert(ToETS, ets:lookup(FromETS,Key)),
  handle_call2(ets:next(FromETS, Key), FromETS, ToETS, ReceiveToken, ServerPID, From).
%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.
%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.
%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.
%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
load_characters(Dir)->
  Files = filelib:wildcard(filename:join(Dir, "*.pilot")),
  lists:foldr(fun(File,Acc)->{ok,TableRef} = ets:file2tab(File),
    [{character, CharacterID}]=ets:lookup(TableRef,character),
    [{character_name, CharacterName}]=ets:lookup(TableRef,character_name),
    Acc#{CharacterID => TableRef, CharacterName => {id, CharacterID}}
  end,#{}, Files).

parse_character(CharcterETS, CharacterIDBin)->
  #{<<"skills">> := SkillList } = esi_box:req(get, "/characters/~s/skills/", {[CharacterIDBin],[]},CharacterIDBin),
  lists:foldr(fun(#{<<"active_skill_level">> := SkillLevel,<<"skill_id">> := SkillID}, Acc)-> Acc#{SkillID => SkillLevel} end,#{}, SkillList).


-define(SkillCategoryID, 16).

get_skills()->
  %% generate all5 user
  SkillGroupList = ets:select(group_id, [{{'$1',?SkillCategoryID}, [], ['$1']}]),
  lists:foldr(fun(GroupID, Acc)-> ets:select(type_id, [{ {'$1', '$2'},[{'==', GroupID, {map_get, <<"groupID">>,'$2'}}], ['$1']}]) ++ Acc end, [], SkillGroupList).

gen_character(<<"all5">>, TableRef)->
  TypeList = get_skills(),
  ets:insert(TableRef,{character,<<"all5">>}),
  ets:insert(TableRef,{character_name,<<"All5 character">>}),
  ets:insert(TableRef,{ship, none, <<"all5">>}),
  [teki:add(<<"all5">>,TypeID,[skill],TableRef)||TypeID<-TypeList],
  lists:foreach(fun(SkillID)-> set_skill_level(SkillID, <<"all5">>, 5, TableRef) end, TypeList), ok;
gen_character(Character, TableRef) when is_map(Character)->
  {CharacterID, SkillMap} = maps:take(id, Character),
  TypeList = get_skills(),
  ets:insert(TableRef,{character,CharacterID}),
  ets:insert(TableRef,{character_name,maps:get(name, Character, CharacterID)}),
  ets:insert(TableRef,{ship, none, CharacterID}),
  [teki:add(CharacterID,TypeID,[skill],TableRef)||TypeID<-TypeList],
  maps:map(fun(SkillID,SkillLevel)-> set_skill_level(SkillID, CharacterID, SkillLevel, TableRef) end, SkillMap), ok;
gen_character({CharacterID, CharacterName}, TableRef) when is_binary(CharacterID) and is_binary(CharacterName)->
  TypeList = get_skills(),
  ets:insert(TableRef,{character,CharacterID}),
  ets:insert(TableRef,{character_name,CharacterName}),
  ets:insert(TableRef,{ship, none, CharacterID}),
  [teki:add(CharacterID,TypeID,[skill],TableRef)||TypeID<-TypeList],
  %% get skills list from esi
  #{<<"skills">> := SkillList } = esi_box:req(get, "/characters/~s/skills/", {[CharacterID],[]},CharacterID),
  lists:foreach(fun(#{<<"active_skill_level">> := SkillLevel,<<"skill_id">> := SkillID})-> set_skill_level(SkillID, CharacterID, SkillLevel, TableRef) end, SkillList).


set_skill_level(SkillTypeID, CharID, Level, FitRef)->
  case ets:lookup(FitRef, {attr, {SkillTypeID,CharID}, 280}) of
    []->
      ets:insert(FitRef, {{attr, {SkillTypeID,CharID}, 280}, Level, #{}, #{ <<"compiled">> => true, <<"base">> => 0}});
    [{_, OldValue, Modifier, Opt}]->
      ets:insert(FitRef, {{attr, {SkillTypeID,CharID}, 280}, Level, Modifier, Opt#{ <<"compiled">> => true, <<"base">> => OldValue}})
  end.

save_character_table(TableRef)->
  CharactersDir = application:get_env(teki, characters_dir ,"characters"),
  [{character, CharacterID}]=ets:lookup(TableRef,character),
  [{character_name, CharacterName}]=ets:lookup(TableRef,character_name),
  error_logger:info_msg("[tab2file] ~p -> ~p", [TableRef,filename:join(CharactersDir,iolist_to_binary(io_lib:format("~s(~s).pilot", [CharacterName, CharacterID])))]),
  ok = ets:tab2file(TableRef, binary_to_list(filename:join(CharactersDir,iolist_to_binary(io_lib:format("~s(~s).pilot", [CharacterName, CharacterID])))), [{sync, true}]).
