-module(atm).

-behaviour(gen_statem).

%% API
-export([start_link/1, insert_card/1, push_button/1]).

%% gen_statem callbacks
-export([
  init/1,
  handle_event/4,
  terminate/3,
  code_change/4,
  callback_mode/0
]).

%% gen_statem states
-export([
  waiting_for_card/3,
  waiting_for_pin/3,
  waiting_for_sum/3
]).

-record(state, {accounts = [], insertedCard = nil, input = []}).

-record(account, {card, pin, balance}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Accounts) ->
  gen_statem:start_link({local, ?MODULE}, ?MODULE, Accounts, []).

insert_card(Card) -> 
  gen_statem:call(?MODULE, {insert, Card}).

push_button(enter) ->
  gen_statem:call(?MODULE, {button, enter});

push_button(cancel) ->
  gen_statem:call(?MODULE, {button, cancel});

push_button(Button) when is_integer(Button) ->
  gen_statem:cast(?MODULE, {button, Button});

push_button(_Button) ->
  {error, "There is no such button"}.

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

init(Accounts) ->
  {ok, waiting_for_card, #state{accounts = Accounts}}.

callback_mode() ->
  state_functions.

waiting_for_card({call, From}, {insert, Card}, #state{accounts = Accounts}) ->
  case check_if_user_is_in_list(Card, Accounts) of
    true ->
      NewState = #state{
        accounts = Accounts,
        insertedCard = Card
      },
      {next_state, waiting_for_pin, NewState, [{reply, From, {ok, "Enter the pin"}}, {state_timeout, 10000, timeout}]};
    false ->
      {keep_state_and_data, [{reply, From, {error, "ATM doesn't servise this card"}}]}
  end;

waiting_for_card({call, From}, {button, _Button}, _State) ->
  {keep_state_and_data, [{reply, From, ok}]};

waiting_for_card(_EventType, _EventContent, _State) ->
  keep_state_and_data.

waiting_for_pin(state_timeout, timeout, State) ->
    io:format("Timeout~n"),
    {next_state, waiting_for_card, State};

waiting_for_pin({call, From}, {button, cancel}, #state{accounts = Accounts}) ->
  NewState = #state{accounts = Accounts},
  {next_state, waiting_for_card, NewState, [{reply, From, {ok, "Card returned"}}, {state_timeout, cancel}]};

waiting_for_pin({call, From}, {button, enter}, #state{accounts = Accounts, input = Input, insertedCard = Card}) ->
  case check_pin(Card, lists:reverse(Input), Accounts) of
    true ->
      NewState = #state{
        accounts = Accounts,
        insertedCard = Card
      },
      {next_state, waiting_for_sum, NewState, [{reply, From, {ok, "Valid pin, enter the sum to withdraw"}}, {state_timeout, cancel}]};
    false ->
      NewState = #state{
        accounts = Accounts
      },
      {next_state, waiting_for_card, NewState, [{reply, From, {error, "Wrong pin, get your card"}}, {state_timeout, cancel}]}
  end;

waiting_for_pin(cast, {button, Button}, #state{accounts = Accounts, input = Input, insertedCard = Card}) ->
  NewState = #state{accounts = Accounts, input = [Button + 48 | Input], insertedCard = Card},
  {keep_state, NewState, [{state_timeout, cancel}, {state_timeout, 10000, timeout}]};

waiting_for_pin({call, From}, {insert, _Card}, _State) ->
  {keep_state_and_data, [{reply, From, {error, "There is a card in cardholder"}}]};

waiting_for_pin(_EventType, _EventContent, _State) ->
  keep_state_and_data.

waiting_for_sum({call, From}, {button, cancel}, #state{accounts = Accounts}) ->
  NewState = #state{accounts = Accounts},
  {next_state, waiting_for_card, NewState, [{reply, From, {ok, "Card returned"}}]};

waiting_for_sum({call, From}, {button, enter}, #state{accounts = Accounts, input = Input, insertedCard = Card}) ->
  Sum = list_to_integer(lists:reverse(Input)),
  case check_sum(Card, Sum, Accounts) of
    true ->
      NewAccounts = change_balance(Card, -Sum, Accounts),
      NewState = #state{
        accounts = NewAccounts
      },
      {next_state, waiting_for_card, NewState, [{reply, From, {ok, "Get your money and card"}}]};
    false ->
      NewState = #state{
        accounts = Accounts
      },
      {next_state, waiting_for_sum, NewState, [{reply, From, {error, "You don't have enough money"}}]}
  end;

waiting_for_sum(cast, {button, Button}, #state{accounts = Accounts, input = Input, insertedCard = Card}) ->
  NewState = #state{accounts = Accounts, input = [Button + 48 | Input], insertedCard = Card},
  {keep_state, NewState};

waiting_for_sum({call, From}, {insert, _Card}, _State) ->
  {keep_state_and_data, [{reply, From, {error, "There is a card in cardholder"}}]};

waiting_for_sum(_EventType, _EventContent, _State) ->
  keep_state_and_data.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name.  If callback_mode is statefunctions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Actions} |
%%                   {stop, Reason, NewState} |
%%    				 stop |
%%                   {stop, Reason :: term()} |
%%                   {stop, Reason :: term(), NewData :: data()} |
%%                   {stop_and_reply, Reason, Replies} |
%%                   {stop_and_reply, Reason, Replies, NewState} |
%%                   {keep_state, NewData :: data()} |
%%                   {keep_state, NewState, Actions} |
%%                   keep_state_and_data |
%%                   {keep_state_and_data, Actions}
%% @end
%%--------------------------------------------------------------------

handle_event(_EventType, _EventContent, _StateName, State) ->
  NextStateName = the_next_state_name,
  {next_state, NextStateName, State}.


terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

check_if_user_is_in_list(Card, Accounts) ->
  case lists:keyfind(Card, #account.card, Accounts) of
    false -> 
      false;
    _ ->
      true
  end.

check_pin(Card, Pin, Accounts) ->
  case lists:keyfind(Card, #account.card, Accounts) of
    false ->
      false;
    Account ->
      Pin == Account#account.pin
  end.

check_sum(Card, Sum, Accounts) ->
  case lists:keyfind(Card, #account.card, Accounts) of
    false ->
      false;
    Account ->
      Sum =< Account#account.balance
  end.

change_balance(Card, Sum, Accounts) ->
  {value, #account{pin = Pin, balance = Balance}, NewAccounts} = lists:keytake(Card, #account.card, Accounts),
  NewAccount = #account{
    card = Card,
    pin = Pin,
    balance = Balance + Sum
  },
  [NewAccount | NewAccounts].


%%rd(account, {card, pin, balance}).
%%Ac1 = #account{card = 1, pin = "1234", balance = 1000}.