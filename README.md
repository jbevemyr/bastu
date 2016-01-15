# Bastu

Project for remote control of a sauna heater.

The goal of this project is to be able to start and stop a sauna
heater using a browser application, using, for example, a mobile phone
and to read the current temperature to see when it is ready to use.

The solution is built around three main hardware components, 

* a Raspberry PI (version 2)
* one or two 25A 240VAC solid state relays (Crydom D2425)
* a I2C - 1wire temperature sensor (Dallas DS18B20)

with a few supporting components detailed below.

The software consists of two parts, one server side that the browser
connects to, and one part that runs on the Raspberry PI. Both parts
are written in Erlang. The client side uses two GPIO pins on the raspberry
to control the relay(s), and one GPIO pin for the i2c protocol communication
with the Dallas DS18B20 temperature sensor.

The client uses a Comet (https://en.wikipedia.org/wiki/Comet_%28programming%29)
request to talk to the server. This allow the server to control the client
even when the server cannot initiate a connection to the client due to it
being located behind a firewall. More on this under the Software section
below.

### Hardware

For hardware we choose the Raspberry PI 2 since it is easy to program
and fairly inexpensive, and also great to experiment with.

To connect the Dallas DS18B20 we need three wires, ground, 3.3v and
GPIO4, and a resistor between 3.3v and the GPIO4 pin.

![DS18B20 4k7](http://www.sbprojects.com/projects/raspberrypi/ds1820connect.png)

The solid state relay is a bit expensive but has the advantage of
being able to handle hight loads while at the same time being
controlled by a low current, just 7 mA, and low voltage (3-32V).

The Raspberry PI can supply a maximum of 16 mA on the GPIO pins,
and outputs 3.3V so it should not be a problem to control the
relays directly from the GPIO pins. However, to be on the safe side
we are using a level shifter that will raise the voltage from 3.3V
to 5V. We use a Texas Instruments 74AHCT125. The max output current
of the 74AHCT125 is 8mA which is enough to drive the D2425.

#### Connecting to the Sauna Heater

This solution is created for old style sauna heaters with mechanical
controls. Newer, digitally controlled heaters, may require a
different solution.

Heaters come in two setups, those with an external control box,
and those with the controls built into the unit. Most have some
switch that controls a mechanical relay and does not directly
switch the high load circuitry going to the heater. Some have both
an on/off switch and a timer, while some only has a mechanical
timer (egg clock style).

The idea is to connect our relay in parallel with the switches
in the sauna controller. If there is an on/off switch, we connect
one relay in parallel with that, if there is a timer switch we
connect a relay in parallel with that. When the sauna should be
turned on we activate the relays in tandem, and the same when
turning off the sauna. This means that if the sauna is turned
on using this device, it must be turned off using it as well.
And it also means that the sauna can be controlled using the
original sauna controls as well.


#### Schematics

### Software

The PI runs a version of Linux with Erlang installed and the
erlang_ale library for controlling the hardware. It is set up
to automatically start the sauna software on startup through
the following code in the /etc/rc.local file:

```
(screen -d -m /usr/local/src/gitlab/bastu/pi/bastu/start.sh || true)
```

The server runs inside a screen process which means that it
is possible to log on to the box and inspect the current
state of the server through the Erlang shell.

The software on the PI consists of two parts: 1) [bastu.erl] a
gen-server that controls the sauna through the GPIO pins, and
regularly reads the temperature (every 5 seconds), and 2)
[bastu_comet.erl] a server communication part that connects to the
web-site server and waits for orders to activate/deactivate the sauna,
or to report the current temperature.

The bastu.erl server keeps has the following state


```erlang
-record(state, {
          hw_switch,         %% GPIO pin for controlling the on/off switch on the sauna
	  hw_timer,          %% GPIO pin for controlling the timer switch on the sauna
	  ref,               %% timer reference for the 3 hour on timer
	  temp,              %% current temperature
	  status="off",      %% current state on or off
	  start_time=gnow()  %% time when the sauna was turned on
         }).

```

```erlang
init([]) ->
    %% initialize state of server loop
    {ok, Gpio10} = gpio:start_link(10, output),
    {ok, Gpio11} = gpio:start_link(11, output),
    gpio:write(Gpio10, 0),
    gpio:write(Gpio11, 0),
    timer:send_after(5000, temp),
    {ok, #state{hw_switch=Gpio10, hw_timer=Gpio11}}.

handle_call(sauna_on, _From, S) ->
    %% turn sauna on
    if S#state.hw_timer == undefined ->
	    ok;
       true ->
	    timer:cancel(S#state.hw_timer)
    end,
    gpio:write(S#state.hw_switch, 1),
    gpio:write(S#state.hw_timer, 1),
    %% run for three hours
    {ok, Ref} = timer:send_after(3*60*60*1000, sauna_timeout),
    {reply, ok, S#state{ref=Ref, status="on", start_time=gnow()}};
handle_call(sauna_off, _From, S) ->
    %% turn sauna off
    gpio:write(S#state.hw_switch, 0),
    gpio:write(S#state.hw_timer, 0),
    timer:cancel(S#state.ref),
    {reply, ok, S#state{ref=undefined, status="off"}};
handle_call(get_temp, _From, S) ->
    %% report current temperature of sauna
    {reply, S#state.temp, S};
handle_call(get_status, _From, S) ->
    %% report if the sauna is turned on or off
    {reply, S#state.status, S};
handle_call(get_end_time, _From, S) ->
    %% Report for how much longer the sauna will be on
    Remain = S#state.start_time+3*60*60-gnow(),
    {reply, Remain, S};
```

### Installation

### Usage
