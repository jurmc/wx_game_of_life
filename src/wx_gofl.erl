-module(wx_gofl).

-export([start_link/1, tick/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-behaviour(gen_server).

-include_lib("wx/include/wx.hrl").

-record(state, {wx_env, dim, check_boxes}).

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
    StartButton = wxButton:new(Frame, ?wxID_ANY, [{label, "Start"}]),
    wxSizer:add(ToolsSizer, StartButton, [{proportion, 1}, {flag, ?wxEXPAND}]),

    CheckBoxesWithCoords = lists:foldl(fun(X, Acc) ->
                                               RowSizer = wxBoxSizer:new(?wxHORIZONTAL),
                                               wxSizer:add(MainSizer, RowSizer, [{proportion, 1}, {flag, ?wxEXPAND}]),
                                               NewCheckBoxes = [{X,Y, wxCheckBox:new(Frame, ?wxID_ANY, "")} || Y <- lists:seq(1,Dim)],
                                               lists:map(fun({_,_, ChBox}) ->
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

    {ok, #state{wx_env = WxEnv, dim = Dim, check_boxes = CheckBoxesWithCoords}}.

handle_cast(tick, #state{dim = Dim, check_boxes = CheckBoxes} = State) ->
    AfterEvaluation = lists:map(fun({X,Y,CheckBox}) ->
                                        NeighboursForCoord = neighours({X,Y}, Dim),
                                        io:format("NeighboursForCoord: {x,y}: ~p, ~p~n", [{X,Y}, NeighboursForCoord]),
                                        {X,Y, CheckBox, wxCheckBox:isChecked(CheckBox)}
                                end,
                                CheckBoxes),
    %io:format("After Evaluation: ~p~n", [AfterEvaluation]),
    {noreply, State};
handle_cast(_Req, State) ->
    {noreply, State}.

handle_call(_Req, _From, State) ->
    {noreply, State}.

handle_info(#wx{event = #wxClose{}}, State) ->
    {stop, normal, State}.

%%% Internal functions

neighours({X,Y}, Dim) ->
    lists:filter(fun({NX, _}) when NX =< 1; NX >= Dim -> false;
                    ({_, NY}) when NY =< 1; NY >= Dim -> false;
                    (_) -> true
                 end,
                 [{X-1,Y}, {X+1,Y}, {X,Y-1}, {X,Y+1}]).

