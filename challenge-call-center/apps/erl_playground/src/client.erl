-module(client).

-export([functionalities/0]).

-record(functionality, {name, handler}).

functionalities() ->
    [
        #functionality{name = "Request user ID", handler = fun () -> userID end},%%handle_userID/0},
        #functionality{name = "Joke of the day", handler = fun () -> jokes end},%%handle_joke/0},
        #functionality{name = "Weather forecasts", handler = fun () -> forecasts end},%%handle_weather/0},
        #functionality{name = "Aske for an operator", handler = fun () -> operator end},%%handle_operator_req/0},
        #functionality{name = "Close the call", handler = fun () -> quit end}
    ].

