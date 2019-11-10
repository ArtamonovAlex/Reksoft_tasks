-module(db).
-behaviour(gen_server).

%% API
-export([stop/1, new/1, insert/3, delete/2, delete_all_objects/1, find/2]).

%%Behaviour functions
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {records = []}).

%%--------------------
%%	API Functions
%%--------------------
stop(Name) ->
   gen_server:call(Name, stop).

new(Name) ->
   gen_server:start_link({local, Name}, ?MODULE, [], []).

insert(Name, Key, Value) ->
	gen_server:cast(Name, {insert, Key, Value}).

delete(Name, Key) ->
	gen_server:cast(Name, {delete, Key}).

delete_all_objects(Name) ->
	gen_server:cast(Name, delete_all).

find(Name, Key) ->
	gen_server:call(Name, {find, Key}).

%%-----------------------------
%%	GEN_SERVER FUNCTIONS
%%-----------------------------

init(_Args) ->
   {ok, #state{records=[]}}.

handle_call(stop, _From, State) ->
   {stop, normal, stopped, State};

handle_call({find, Key}, _From, #state{records = Database} = State) ->
   {reply, find_internal(Key, Database), State};

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast({insert, Key, Value},  #state{records = Database}) ->
	NewDatabase = [{K, V} || {K, V} <- Database, K /= Key],
	NewState = #state{records = [{Key, Value} | NewDatabase]},
	{noreply, NewState};

handle_cast({delete, Key},  #state{records = Database}) ->
	NewDatabase = [{K, V} || {K, V} <- Database, K /= Key],
	NewState = #state{records = NewDatabase},
	{noreply, NewState};

handle_cast(delete_all,  _State) ->
	NewState = #state{records = []},
	{noreply, NewState};

handle_cast(_Msg, State) ->
   {noreply, State}.

handle_info(_Info, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

%%-----------------------------------------
%%	INTERNAL BUSINESS FUNCTIONS
%%-----------------------------------------

find_internal(Key, [{Key, Value} | _Db]) -> 
	{ok, Value};

find_internal(Key, [_Element | Db]) -> 
	find_internal(Key, Db);

find_internal(_Key, []) -> 
	not_found.



