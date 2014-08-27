-module (merkxx_request).

-behaviour(gen_server).

-include ("merkxx.hrl").

-export ([new_request/1, match_next_request/1,close_request/1]).
-export ([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define (PENDING_REQUESTS_STORE_TABLE_ID, merkxx_pending_requests_store).

%% Public API
start_link() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

new_request(Request) when is_record(Request, provision_request)->
   gen_server:call(?MODULE, {store_new_request, Request }). 

match_next_request({Cpu, Memory, Ports}) ->
   gen_server:call(?MODULE, {match_next_request, {Cpu, Memory, Ports} }).

close_request(Identifier) ->
   gen_server:call(?MODULE, {close_request, Identifier }).

%% GEN SERVER callbacks
init([]) ->
    ets:new(?PENDING_REQUESTS_STORE_TABLE_ID,[protected, named_table, {keypos, #provision_request.identifier}]),
    {ok, []}.

handle_call( {close_request,  Identifier }, _, State) ->
    true = ets:delete(?PENDING_REQUESTS_STORE_TABLE_ID,Identifier),
    {reply, ok ,State};

handle_call({match_next_request, _ }, _, State) ->
    case ets:match(?PENDING_REQUESTS_STORE_TABLE_ID, '$1', 1) of % just grap the first one for now
        '$end_of_table'  ->
            {reply, {ok,[]} ,State};
         {[Results], _} ->
            {reply, {ok,Results} ,State}
    end;

handle_call({store_new_request,  Request }, _, State) ->   
    Identifier = merkxx_util:generate_uuid(),
    Request1 = Request#provision_request{identifier = Identifier},
    true = ets:insert(?PENDING_REQUESTS_STORE_TABLE_ID, Request1),
    {reply, {ok,Identifier} ,State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.