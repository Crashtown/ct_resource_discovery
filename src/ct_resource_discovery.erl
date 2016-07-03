%%% Resource discovery module. Borrowed from "Erlang/OTP in action book" %%%
-module(ct_resource_discovery).

-behaviour(gen_server).

%% API %%
-export([start_link/0,
         add_local_resource/2,
         add_target_resource_type/1,
         fetch_resources/1,
         trade_resources/0]).

%% gen_server callbacks %%
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).


-record(state, {local_resources, % I have {Type(), Instance()}
                target_resource_types, % I want (Type())
                resources}). % Discovered resources of wanted type()

%%% API definition %%%
start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_local_resource(Type, Instance) ->
  gen_server:cast(?SERVER, {add_local_resource, {Type, Instance}}).

add_target_resource_type(Type) ->
  gen_server:cast(?SERVER, {add_target_resource_type, Type}).

fetch_resources(Type) ->
  gen_server:call(?SERVER, {fetch_resources, Type}).

trade_resources() ->
  gen_server:cast(?SERVER, trade_resources).

%%% gen_server callbacks definition %%%
init([]) ->
  io:format("Service discovery initialized on node ~p~n", [node()]),
  State = #state{local_resources = dict:new(),
                 target_resource_types = [],
                 resources = dict:new()},
  {ok, State}.

handle_cast({add_local_resource, {Type, Instance}}, State) ->
  #state{local_resources = LocalResources} = State,
  NewLocalResources = add_resource(LocalResources, Type, Instance),
  {noreply, State#state{local_resources = NewLocalResources}};
handle_cast({add_target_resource_type, Type}, State) ->
  #state{target_resource_types = TargetResourceTypes} = State,
  NewTargetResourceTypes = [Type | lists:delete(Type, TargetResourceTypes)],
  {noreply, State#state{target_resource_types = NewTargetResourceTypes}};
handle_cast(trade_resources, #state{local_resources = LocalResources} = State) ->
  lists:foreach(fun(Node) ->
                  gen_server:cast({?SERVER, Node},
                                  {trade_resources, {node(), LocalResources}})
                end,[node() | nodes()]),
  {noreply, State};
handle_cast({trade_resources, {ReplyToken, RemoteResources}}, State) ->
  #state{local_resources = LocalResources,
         target_resource_types = TargetResourceTypes,
         resources = Resources} = State,
  FoundResources = get_resources_for_types(TargetResourceTypes, RemoteResources),
  io:format("DISCOVERED RESOURCES: ~p~n", [FoundResources]),
  NewResources = add_resources(Resources, FoundResources),
  case ReplyToken of
    noreply -> ok;
    ReplyToken -> gen_server:cast({?SERVER, ReplyToken},
                                  {trade_resources, {noreply, LocalResources}})
  end,
  {noreply, State#state{resources = NewResources}};
handle_cast(_Any, State) ->
  {noreply, State}.

handle_call({fetch_resources, Type}, _From, State) ->
  {reply, dict:find(Type, State#state.resources), State};
handle_call(_Any, _From, State) ->
  {noreply, State}.

handle_info(_Request, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVersion, State, _Extra) ->
  {ok, State}.

%%% Internal functions %%%
add_resource(Dict, Type, Instance) ->
  case dict:find(Type, Dict) of
    {ok, ResourceList} ->
      NewList = [Instance | lists:delete(Instance, ResourceList)],
      dict:store(Type, NewList, Dict);
    error ->
      dict:store(Type, [Instance], Dict)
  end.

add_resources(Dict, Resources) ->
  lists:foldl(fun({Type, Instances}, Acc) ->
    lists:foldl(fun(Instance, Acc1) ->
      add_resource(Acc1, Type, Instance)
    end, Acc, Instances)
  end, Dict, Resources).

get_resources_for_types(Types, Resources) ->
  lists:foldl(fun(Type, Acc) ->
                case dict:find(Type, Resources) of
                  {ok, Resource} -> [{Type, Resource} | Acc];
                  error -> Acc
                end
              end, [], Types).
