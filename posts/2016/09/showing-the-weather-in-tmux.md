---
title: Showing The Weather In Tmux
date: 2016-09-23
excerpt: Bash scripting for fun and profit. And the weather, obviously.
location: Gdynia, Poland
tags: bash
---

The weather is unpredictable here on the North coast of Poland where I live,
especially around this time of year. On those infrequent occasions when I do
leave my apartment (I work from home), I'm never sure how warmly I should dress
or how careful I should be on the road — cold-snaps are increasingly frequent.

I spend most of my day staring at my terminal, so that's where I'd like to keep
all of the information I care about. My Tmux status bar currently contains my
laptop's remaining battery life, the current time and date, and now the local
weather. Here's how that looks:

![](/static/img/tmux_weather.png)

## We're Not In Kansas Anymore

The first thing we need to do is find our approximate geographical coordinates.
I say approximate because I'm not willing to pay money for a high level of
accuracy. Kinda-sorta where I live is good enough. Curiously enough, Google's
geolocation API seemed to be broken for me and — after some research — many
other people.

I found a free service called IP-API. As the name suggests, it returns your
location based on your IP address. The service allows up to 150 requests per
minute which is plenty for our needs — we won't be making requests more than
once per second.

Running the following command gives us a collection of values about our
geographical data, separated by commas:

```
curl --silent http://ip-api.com/csv
success,Poland,PL,PM,"Pomeranian Voivodeship",Gdańsk,80-009,54.3608,18.6583,Europe/Warsaw,"Neostrada Plus","Neostrada Plus",<redacted>,<redacted>
```

Using the `cut` command, we can split the comma-separated values on those
commas, and the `-f` flag allows us to choose which field we're interested in.
For my script, I'm pulling fields six, eight, and nine to grab the city,
latitude, and longitude values respectively.

```bash
LOCATION=$(curl --silent http://ip-api.com/csv)
CITY=$(echo "$LOCATION" | cut -d , -f 6)
LAT=$(echo "$LOCATION" | cut -d , -f 8)
LON=$(echo "$LOCATION" | cut -d , -f 9)
```

You'll notice I'm being careful to wrap each of those `$LOCATION` variables in
double-quotes to prevent word-splitting. If you start writing more Bash scripts
(and you should), this should become a habit.

If you don't move around much, you can skip the geolocation step and just
hard-code your geographical coordinates. I do travel quite frequently, so I want
the weather in my status bar to reflect the weather outside.

## Show Me The Data

Now that we have our location, we need to ask another service for our weather
data. There are a number of services online that provide an API for querying
weather data, but again, I am not willing to pay actual money for this; it's
just for fun.

I found a service for querying weather data called OpenWeatherMap. You'll need
to register an account with them to obtain an API key, but they allow up to 60
requests per minute which again is enough for our needs.

If we send a request to OpenWeatherMap with our geographical coordinates and our
API key, the service returns a big lump of JSON full of the data you need.
Parsing this JSON string is too hairy a task for any native UNIX tools, but you
can use your favourite package manager to install a JSON parser called _jq_.
Accessing fields with _jq_ is syntactically the same as looking up array indexes
and object properties in JavaScript.

```bash
WEATHER=$(curl --silent http://api.openweathermap.org/data/2.5/weather\?lat="$LAT"\&lon="$LON"\&APPID="$API_KEY"\&units=metric)

CATEGORY=$(echo "$WEATHER" | jq .weather[0].id)
TEMP="$(echo "$WEATHER" | jq .main.temp | cut -d . -f 1)°C"
WIND_SPEED="$(echo "$WEATHER" | jq .wind.speed | awk '{print int($1+0.5)}')ms"
ICON=$(weather_icon "$CATEGORY")

printf "%s" "$CITY:$ICON $TEMP, $WIND_SPEED"
```

I only care about whole numbers for temperature and wind speed, so I'm using
`cut` and `awk` to truncate and round those values respectively. I truncate the
temperature instead of rounding it because I am originally from London, which
means I have pessimism as a hereditary trait.

The `weather_icon` function simply maps weather category IDs to some emoji.
You'll see it in the full script below. You'll notice too that I'm asking for
the data in metric units. You can switch that to imperial if you're so inclined.

## Get In My Status If You Want To Live

The last step is to save the script somewhere appropriate — for me it's under
`~/.bin/weather` — and then run a `chmod u+x ~/.bin/weather` to make the script
executable. The weather script can now be called from your Tmux configuration.

When I open up my `~/.tmux.conf` file, I have these lines:

```bash
set -g status-right-length 50
set -g status-right '#[fg=green][#[default]#($HOME/.bin/weather)#[fg=green]] #[fg=green][#[fg=blue]%Y-%m-%d #[fg=white]%H:%M#[default]#[fg=green]] #[fg=green][#($HOME/.bin/battery)#[fg=green]]'
set -g status-interval 1
```

The first line sets the available length for the right-side of my status bar.
I'm not sure what the default length is, but it isn't long enough to display
everything I want without truncating. The second line is my literal status line.
I use a combination of colours, whitespace and punctuation to separate the
different parts of my status line. The third line tells Tmux that I want to
update my status line every second, which is important for telling accurate
time.

And that's all there is to it! For completeness, here's the entire `weather`
script:

```bash
#!/bin/bash
#
# Weather
# =======
#
# By Jezen Thomas <jezen@jezenthomas.com>
#
# This script sends a couple of requests over the network to retrieve
# approximate location data, and the current weather for that location. This is
# useful if for example you want to display the current weather in your tmux
# status bar.

# There are three things you will need to do before using this script.
#
# 1. Install jq with your package manager of choice (homebrew, apt-get, etc.)
# 2. Sign up for a free account with OpenWeatherMap to grab your API key
# 3. Add your OpenWeatherMap API key where it says API_KEY

# OPENWEATHERMAP API KEY (place yours here)
API_KEY="<redacted>"

set -e

# Not all icons for weather symbols have been added yet. If the weather
# category is not matched in this case statement, the command output will
# include the category ID. You can add the appropriate emoji as you go along.
#
# Weather data reference: http://openweathermap.org/weather-conditions
weather_icon() {
  case $1 in
    500) echo 🌦
      ;;
    800) echo ☀️
      ;;
    801) echo 🌤
      ;;
    803) echo ⛅️
      ;;
    804) echo ☁️
      ;;
    *) echo "$1"
  esac
}

LOCATION=$(curl --silent http://ip-api.com/csv)
CITY=$(echo "$LOCATION" | cut -d , -f 6)
LAT=$(echo "$LOCATION" | cut -d , -f 8)
LON=$(echo "$LOCATION" | cut -d , -f 9)

WEATHER=$(curl --silent http://api.openweathermap.org/data/2.5/weather\?lat="$LAT"\&lon="$LON"\&APPID="$API_KEY"\&units=metric)

CATEGORY=$(echo "$WEATHER" | jq .weather[0].id)
TEMP="$(echo "$WEATHER" | jq .main.temp | cut -d . -f 1)°C"
WIND_SPEED="$(echo "$WEATHER" | jq .wind.speed | awk '{print int($1+0.5)}')ms"
ICON=$(weather_icon "$CATEGORY")

printf "%s" "$CITY:$ICON $TEMP, $WIND_SPEED"
```

_n.b._ I realise I could just stand on my balcony to see what the weather is like,
but what kind of nerd would I be if I didn't script it somehow?!
