-module(wx_gofl).

%temp exports
-export([neighours/2]).

-export([start_link/1, tick/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-behaviour(gen_server).

-include_lib("wx/include/wx.hrl").

-record(state, {wx_env, dim, check_boxes, timer}).

-define(ID_TICK, 1).
-define(ID_PLAY, 2).

%%% API

start_link(Dim) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Dim], []).

tick() ->
    gen_server:cast(?MODULE, tick).

%%% GenServer

init([Dim]) ->
    wx:new(),
    WxEnv = wx:get_env(),

    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "wx Game of Life"),
    Panel = wxPanel:new(Frame),

    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    ToolsSizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(MainSizer, ToolsSizer, [{proportion, 1}, {flag, ?wxEXPAND}]),
    TickButton = wxButton:new(Panel, ?ID_TICK, [{label, "Tick"}]),
    PlayButton = wxButton:new(Panel, ?ID_PLAY, [{label, "Play"}]),
    wxSizer:add(ToolsSizer, TickButton, [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxSizer:add(ToolsSizer, PlayButton, [{proportion, 1}, {flag, ?wxEXPAND}]),

    wxEvtHandler:connect(TickButton, command_button_clicked),
    wxEvtHandler:connect(PlayButton, command_button_clicked),

    CheckBoxesWithCoords = lists:foldl(fun(X, Acc) ->
                                               RowSizer = wxBoxSizer:new(?wxHORIZONTAL),
                                               wxSizer:add(MainSizer, RowSizer, [{proportion, 1}, {flag, ?wxEXPAND}]),
                                               NewCheckBoxes = [{{X,Y}, wxCheckBox:new(Frame, ?wxID_ANY, "", [])} || Y <- lists:seq(1,Dim)],
                                               lists:map(fun({{_,_}, ChBox}) ->
                                                                 Checked = case rand:uniform(2) of
                                                                               1 -> true;
                                                                               2 -> false
                                                                           end,
                                                                 wxCheckBox:setValue(ChBox, Checked),
                                                                 wxSizer:add(RowSizer, ChBox, [{proportion, 1}, {flag, ?wxEXPAND}])
                                                         end,
                                                         NewCheckBoxes),
                                               Acc ++ NewCheckBoxes
                                       end,
                                       [],
                                       lists:seq(1,Dim)),

    wxWindow:setSizer(Panel, MainSizer),
    wxSizer:setSizeHints(MainSizer, Panel),
    wxWindow:setMinSize(Panel, wxWindow:getSize(Frame)),
    wxFrame:connect(Frame, close_window),
    wxFrame:show(Frame),

    gen_server:cast(self, tick),

    {ok, #state{wx_env = WxEnv, dim = Dim, check_boxes = maps:from_list(CheckBoxesWithCoords), timer = undefined}}.

handle_cast(tick, State) ->
    tick(State);
handle_cast(_Req, State) ->
    {noreply, State}.

handle_call(_Req, _From, State) ->
    {noreply, State}.

handle_info(#wx{id = ?ID_TICK, event = #wxCommand{type = command_button_clicked}}, State) ->
    tick(State);
handle_info(#wx{id = ?ID_PLAY,
                obj = Button,
                event = #wxCommand{type = command_button_clicked}},
            #state{timer = TimerRef} = State) ->
    What = wxButton:getLabel(Button),
    io:format("What: ~p~n", [What]),
    NewTimerRef = case wxButton:getLabel(Button) of
                      "Play" ->
                          wxButton:setLabel(Button, "Stop"),
                          erlang:send_after(1000, self(), timer_expired);
                      "Stop" ->
                          wxButton:setLabel(Button, "Play"),
                          erlang:cancel_timer(TimerRef),
                          undefined
                  end,
    {noreply, State#state{timer = NewTimerRef}};
handle_info(timer_expired, State) ->
    erlang:send_after(1000, self(), timer_expired),
    tick(State);
handle_info(#wx{event = #wxClose{}}, State) ->
    {stop, normal, State};
handle_info(Event, State) ->
    io:format("Unhandled event: ~p~n", [Event]),
    {stop, normal, State}.

%%% Internal functions

tick(#state{dim = Dim, check_boxes = CheckBoxes} = State) ->
    AfterEvaluation = maps:map(fun({X,Y}, CheckBox) ->
                                       NeighboursForCoord = neighours({X,Y}, Dim),
                                       OwnState = case wxCheckBox:getValue(CheckBox) of
                                                      true -> alive;
                                                      _ -> dead
                                                  end,
                                       NeighboursStates = lists:map(fun(Bool) ->
                                                                            case Bool of 
                                                                                true -> alive;
                                                                                _ -> dead
                                                                            end
                                                                    end,
                                                                    [wxCheckBox:getValue((maps:get({NX,NY}, CheckBoxes))) || {NX,NY} <- NeighboursForCoord]),
                                       NextOwnState = game_of_life:evolve(OwnState, NeighboursStates),
                                       %io:format("NeighboursForCoord: {x,y}: ~p, ~p~n", [{X,Y}, NeighboursForCoord]),
                                       %io:format("NeighboursStates:  ~p~n", [NeighboursStates]),
                                       %io:format("NextOwnState:  ~p~n", [NextOwnState]),
                                       {CheckBox, NextOwnState}
                               end,
                               CheckBoxes),
    io:format("After Evaluation: ~p~n", [AfterEvaluation]),
    maps:map(fun(_Key, {CheckBox, BoolState}) ->
                      case BoolState of
                          alive -> wxCheckBox:setValue(CheckBox, true);
                          _ -> wxCheckBox:setValue(CheckBox, false)
                      end
              end,
              AfterEvaluation),
    {noreply, State}.

neighours({X,Y}, Dim) when X =< 1; X >= Dim; Y =< 1; Y >= Dim -> [];
neighours({X,Y}, _Dim) -> [{X-1,Y}, {X+1,Y}, {X,Y-1}, {X,Y+1}].

