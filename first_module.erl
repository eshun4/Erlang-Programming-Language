-module(first_module).
-export([my_function/1]).
-import(math,[sin/1, cos/1]).

my_function(X) ->
    X * 2 * sin(10). %10 in radians