%%%-------------------------------------------------------------------
%%% @author ahmad
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% Based on arduino-erlang firmata by
%%% HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%% https://github.com/hiroeorz/arduino-erlang
%%% @end
%%%-------------------------------------------------------------------
-module(mc).

-behaviour(gen_server).

-export([start_link/0, version/0, digital_write/2, start/0, digital_on/1,
	digital_off/1, pwm/2, analog_write/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).

-define(SYSEX_START_CODE, 16#F0).
-define(SYSEX_END_CODE, 16#F7).

-type pin_mode() :: in | out | analog | pwm | servo.

-include_lib("kernel/include/logger.hrl").

%%--------------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @doc get firmata version from arduino.
%% @end
%%--------------------------------------------------------------------
-spec version() -> ok.
version() ->
	gen_server:cast(?SERVER, version_report_request).

digital_on(Pin) ->
	gen_server:cast(?SERVER, {digital_on, Pin}).
digital_off(Pin) ->
	gen_server:cast(?SERVER, {digital_off, Pin}).
digital_write(Port, Pins) ->
	gen_server:cast(?SERVER, {digital_write, Port, Pins}).
analog_write(Pin, Level) when is_integer(Pin), 0 =< Level andalso
	Level =< 1024 ->
	gen_server:cast(?SERVER, {analog_write, Pin, Level}).
pwm(Pin, Level) when is_integer(Pin), 0 =< Level andalso Level =< 1024 ->
	gen_server:cast(?SERVER, {pwm_set, Pin, Level}).


%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start() -> start_link().
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->

	?LOG_INFO(#{what => "connecting..."}),
	{ok, Serial} = gen_serial:open(ttyACM0, []),
	?LOG_INFO(#{what => "connected", serial => Serial}),
	{ok, #{
		serial => Serial,
		buffer => <<>>,
		digital => #{
			0 => #{0 => 0, 1 => 0, 2 => 0, 3 => 0, 4 => 0, 5 => 0, 6 => 0, 7 => 0},
			1 => #{0 => 0, 1 => 0, 2 => 0, 3 => 0, 4 => 0, 5 => 0, 6 => 0, 7 => 0}
		}
	}}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(version_report_request, #{serial := Serial} = State) ->
	gen_serial:asend(Serial, firmata:format(version_report_request)),
	{noreply, State};

%% digital
handle_cast({digital_on, Pin}, State) ->
	handle_cast({digital_pin_value, Pin, 1}, State);
handle_cast({digital_off, Pin}, State) ->
	handle_cast({digital_pin_value, Pin, 0}, State);
handle_cast({digital_pin_value, Pin, Value}, #{digital := Digital} = State) ->
	Port = pin_port(Pin),
	PinAtPort = Pin - (8 * Port),
	OldValues = maps:get(Port, Digital),
	NewValues = OldValues#{PinAtPort => Value},
	handle_cast({digital_write, Port, NewValues},
		set_digital_pin_mode(Pin, out, State));
handle_cast({digital_write, Port, Vals}, State) when is_map(Vals) ->
	handle_cast({digital_write, Port, values_map_to_list(Vals)}, State);
handle_cast({digital_write, Port, Vals},
	#{serial := Serial, digital := Digital} = State) when is_list(Vals) ->
%%	io:format("d ~~> ~w -> ~w~n", [Port, Vals]),
	gen_serial:asend(Serial, firmata:format(digital_io_message, {Port, Vals})),
	NewDigital = Digital#{Port => values_list_to_map(Vals)},
	{noreply, State#{digital => NewDigital}};

%% analog
handle_cast({analog_write, Pin, Level}, #{serial := Serial} = State) ->
%%	io:format("a ~~> ~w -> ~w~n", [Pin, Level]),
	NewState = set_digital_pin_mode(Pin, analog, State),
	gen_serial:asend(Serial, firmata:format(analog_io_message, {Pin, Level})),
	{noreply, NewState};

%% pwm
handle_cast({pwm_set, Pin, Level}, #{serial := Serial} = State) ->
%%	io:format("p ~~> ~w -> ~w~n", [Pin, Level]),
	NewState = set_digital_pin_mode(Pin, pwm, State),
	gen_serial:asend(Serial, firmata:format(analog_io_message, {Pin, Level})),
	{noreply, NewState};

%% other
handle_cast(_Request, State) -> {noreply, State}.

%%--------------------------------------------------------------------------
%% handle serial data
%%--------------------------------------------------------------------------
handle_info({serial, {gen_serial, _, _}, Data}, State) ->
	handle_serial({data, Data}, State);

%%--------------------------------------------------------------------------
%% unknown
%%--------------------------------------------------------------------------
handle_info(Info, State) ->
	?LOG_WARNING(#{what => "unknown info", info => Info}),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------------
%% serial data
%%--------------------------------------------------------------------------
handle_serial({data, Queue = <<?SYSEX_START_CODE:8, _/binary>>},
	#{buffer := <<>>} = State) ->
	process_firmata_sysex(Queue, State);

%% accumulate the buffer
handle_serial({data, Bin},
	#{buffer := <<?SYSEX_START_CODE:8, _/binary>> = RecvQueue} = State) ->
	Queue = <<RecvQueue/binary, Bin/binary>>,
	process_firmata_sysex(Queue, State);


%%--------------------------------------------------------------------------
%% normal firmata data
%%--------------------------------------------------------------------------
handle_serial({data, <<>>}, #{buffer := <<>>} = State) ->
	{noreply, State};
handle_serial({data, Bin}, #{buffer := RecvQueue} = State) ->
	Queue = <<RecvQueue/binary, Bin/binary>>,
	<<Code:8, TailOfTotal/binary>> = Queue,
	Size = firmata:size(Code),
	if Size =:= unknown ->
		?LOG_ERROR(#{
			what => "code not matched in getting size",
			code => Code,
			buffer => Queue
		}),
		{noreply, State#{buffer => <<>>}};
		true ->
			if byte_size(TailOfTotal) >= Size ->
				<<Code:8, Body:Size/binary, TailBin/binary>> = Queue,
				Reply = firmata:parse(Code, Body),
				NewState = process_firmata(Reply, State),
				handle_serial({data, TailBin}, NewState#{buffer => <<>>});
				true ->
					{noreply, State#{buffer => Queue}}
			end
	end.

%%--------------------------------------------------------------------
%% @private
%% @doc handle firmata message from arduino.
%% @end
%%--------------------------------------------------------------------
process_firmata({sysex, {name_and_version_report, {_, _, _SketchName}}},
	State) ->
%%	io:format("Arduino sketch: ~s~n", [SketchName]),
	State;

process_firmata({version_report, {Major, Minor}}, State) ->
	?LOG_INFO(#{
		what => "Firmata version",
		major => Major,
		minor => Minor,
		state => State
	}),
	State;

process_firmata({digital_io_message, {ArduinoPortNo, Status}}, State) ->
	PortNo = ArduinoPortNo, %%+ State#state.digital_port_offset,
	?LOG_INFO(#{
		what => "digital port changed",
		port => PortNo,
		status => Status
	}),
%%  gen_event:notify(arduino_event, {digital_port_changed, PortNo, Status}),
%%  true = ets:insert(arduino_digital, {PortNo, Status}),
	State;

process_firmata({analog_io_message, {_PinNo, _Val}}, State) ->
%%  io:format("analog_recv: ~p~n", [{analog_recv, PinNo, Val}]),
%%  gen_event:notify(arduino_event, {analog_recv, PinNo, Val}),
%%  true = ets:insert(arduino_analog, {PinNo, Val}),
	State;

process_firmata(Msg, State) ->
	?LOG_ERROR(#{what => "unknown message", message => Msg}),
	State.

%%--------------------------------------------------------------------
%% @private
%% @doc process firmata sysex protocol.
%% @end
%%--------------------------------------------------------------------
-spec process_firmata_sysex(binary(), map()) -> {noreply, #{}}.
process_firmata_sysex(Queue, State) ->
	case has_sysex_end(Queue) of
		{true, Size} ->
			<<?SYSEX_START_CODE:8, Body:Size/binary, ?SYSEX_END_CODE:8,
				TailBin/binary>> = Queue,
			Reply = firmata:parse(?SYSEX_START_CODE, Body),
			NewState = process_firmata(Reply, State),
			handle_info({data, TailBin}, NewState#{buffer => <<>>});
		false ->
			{noreply, State#{buffer => Queue}}
	end.

%%--------------------------------------------------------------------
%% @private
%% @doc search sysex end code.
%%
%% when sysex end code is exist, return {true, Size}.
%% when not exist,               return false
%% @end
%%--------------------------------------------------------------------
-spec has_sysex_end(binary()) -> {true, non_neg_integer()} | false.
has_sysex_end(Bin) -> has_sysex_end(Bin, 0).
has_sysex_end(<<>>, _) -> false;
has_sysex_end(<<?SYSEX_END_CODE:8, _/binary>>, Size) -> {true, Size - 1};
has_sysex_end(<<_:8/integer, TailBin/binary>>, Size) ->
	has_sysex_end(<<TailBin/binary>>, Size + 1).

%%--------------------------------------------------------------------
%% @private
%% @doc set digital pin mode.
%% @end
%%--------------------------------------------------------------------
-spec set_digital_pin_mode(Pin, Mode, State) -> ok when
	Pin :: non_neg_integer(),
	Mode :: pin_mode(),
	State :: map().
set_digital_pin_mode(Pin, Mode, #{serial := Serial} = State) ->
	ModeInt = case Mode of
		          in -> 0;
		          out -> 1;
		          analog -> 2;
		          pwm -> 3;
		          servo -> 4
	          end,
%%	io:format("set pin ~p mode ~p~n", [Pin, ModeInt]),
	?LOG_DEBUG(#{
		what => "set pin mode",
		pin => Pin,
		mode => Mode,
		mode_int => ModeInt
	}),
	gen_serial:asend(Serial, firmata:format(set_pin_mode, {Pin, ModeInt})),
	State.

pin_port(Pin) -> trunc(Pin / 8).
values_map_to_list(#{
	0 := V0,
	1 := V1,
	2 := V2,
	3 := V3,
	4 := V4,
	5 := V5,
	6 := V6,
	7 := V7
}) -> [V0, V1, V2, V3, V4, V5, V6, V7].
values_list_to_map([V0, V1, V2, V3, V4, V5, V6, V7]) -> #{
	0 => V0,
	1 => V1,
	2 => V2,
	3 => V3,
	4 => V4,
	5 => V5,
	6 => V6,
	7 => V7
}.