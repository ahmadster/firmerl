%%%-------------------------------------------------------------------
%%% @author ahmad
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Dec 2019 2:34 AM
%%%-------------------------------------------------------------------
-module(ws).
-author("ahmad").

%% API
-export([]).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-include_lib("kernel/include/logger.hrl").

init(Req, Opts) -> {cowboy_websocket, Req, Opts}.

websocket_init(State) ->
	_AlphaDevices = #{
		<<"Red">> => #{
			id => <<"Red">>,
			name => <<"Red">>,
			state => <<"">>,
			kind => <<"led">>,
			pin => 1
		},
		<<"Green">> => #{
			id => <<"Green">>,
			name => <<"Green">>,
			state => <<"">>,
			kind => <<"led">>,
			pin => 2
		},
		<<"Orange">> => #{
			id => <<"Orange">>,
			name => <<"Orange">>,
			state => <<"">>,
			kind => <<"led">>,
			pin => 3
		},
		<<"Siren">> => #{
			id => <<"Siren">>,
			name => <<"Siren">>,
			state => <<"">>,
			kind => <<"led">>,
			pin => 5
		}
	},

	_BetaDevices = #{
		<<"Red">> => #{
			id => <<"Red">>,
			name => <<"Red">>,
			state => <<"">>,
			kind => <<"led">>,
			pin => 3
		},
		<<"Green">> => #{
			id => <<"Green">>,
			name => <<"Green">>,
			state => <<"">>,
			kind => <<"led">>,
			pin => 5
		},
		<<"Blue">> => #{
			id => <<"Blue">>,
			name => <<"Blue">>,
			state => <<"">>,
			kind => <<"led">>,
			pin => 6
		},
		<<"RGB">> => #{
			id => <<"RGB">>,
			name => <<"Color">>,
			state => <<"">>,
			kind => <<"rgb">>,
			pins => #{
				r => 3,
				g => 5,
				b => 6
			}
		}
	},
	Devices = _AlphaDevices,
	Reply = #{
		devices => Devices,
		re => <<"devices">>
	},
	reply(Reply, State#{devices => Devices}).

websocket_handle({text, Msg}, State) ->
	Decoded = jsone:decode(Msg, [{keys, atom}]),
	case Decoded of
		#{re := <<"ping">>} -> ok;
		#{re := <<"setColor">>} -> ok;
		_ -> ?LOG_INFO(#{
			what => "incoming",
			msg => Decoded
		})
	end,
	handle_request(Decoded, State);

websocket_handle(_Data, State) -> noreply(State).

websocket_info({timeout, _Ref, Msg}, State) ->
	erlang:start_timer(1000, self(), <<"How' you doin'?">>),
	{[{text, Msg}], State};
websocket_info(_Info, State) ->
	noreply(State).


handle_request(#{re := <<"ping">>} = _Request, State) ->
	reply(#{re => <<"pong">>}, State);

handle_request(#{
	re := <<"deviceCommand">>,
	name := DeviceName,
	command := Command
} = _Request, #{devices := Devices} = State) ->
	case maps:get(DeviceName, Devices, error) of
		error -> reply(#{re => <<"error">>, error => <<"device unknown">>}, State);
		Device -> do_command(Command, Device, State)
	end;

handle_request(#{
	re := <<"setColor">>,
	name := DeviceName,
	command := Color
} = _Request, #{devices := Devices} = State) ->
	case maps:get(DeviceName, Devices, error) of
		error -> reply(#{re => <<"error">>, error => <<"device unknown">>}, State);
		Device ->
			Colors = jsone:decode(Color, [{keys, atom}]),
%%			?LOG_INFO(#{what => "colors", colors => Colors, device => Device}),
			do_command(rgb, Device#{color => Colors}, State)
	end;

handle_request(_Request, _State) -> ok.

%% --
noreply(State) -> {[], State}.
reply(Reply, State) -> {[{text, jsone:encode(Reply)}], State}.

do_command(rgb, #{
	pins := #{
		r := RPin,
		g := GPin,
		b := BPin
	},
	color := #{
		r := R,
		g := G,
		b := B
	}
} = _Device, State) ->
	mc:pwm(RPin, R),
	mc:pwm(GPin, G),
	mc:pwm(BPin, B),
	noreply(State);

do_command(Command, #{pin := Pin, name := DeviceName} = _Device, State) ->
	case Command of
		<<"activate">> ->
			mc:digital_on(Pin),
			reply(#{
				re => <<"deviceEvent">>,
				name => DeviceName,
				state => <<"activated">>}, State);
		_ -> mc:digital_off(Pin),
			reply(#{
				re => <<"deviceEvent">>,
				name => DeviceName,
				state => <<"deactivated">>}, State)
	end.
