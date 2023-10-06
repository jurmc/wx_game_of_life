-module(wx_gof).

-export([start_link/0]).
-export([handle_call/3, handle_cast/2, init/1]).

-behaviour(gen_server).

-include_lib("wx/include/wx.hrl").

-record(state, {wx_env}).

%%% API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%% GenServer

init([]) ->
    wx:new(),
    WxEnv = wx:get_env(),

    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "wx Game of Life"),
    Panel = wxPanel:new(Frame),

    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    ToolsSizer = wxBoxSizer:new(?wxHORIZONTAL),
    GridSizer = wxBoxSizer:new(?wxHORIZONTAL),

    wxSizer:add(MainSizer, ToolsSizer, [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxSizer:add(MainSizer, GridSizer, [{proportion, 1}, {flag, ?wxEXPAND}]),

    wxWindow:setSizer(Panel, MainSizer),
    wxSizer:setSizeHints(MainSizer, Panel),
    wxWindow:setMinSize(Panel, wxWindow:getSize(Frame)),
    wxFrame:connect(Frame, close_window),
    wxFrame:show(Frame),

    {ok, #state{wx_env = WxEnv}}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_call(_Req, _From, State) ->
    {noreply, State}.

%%% Internal functions
