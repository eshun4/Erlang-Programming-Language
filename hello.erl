% The purpose of this file is to just show how we complie in erlang
% When we create an erlang file with an erl extension we are essentialy creating a module
-module(hello).

% We export functions below
% The list contains the name of the function followed by a bakslash. The number of input parameters follows the backslash
-export([world/0]).

world() -> 
    io: format("Hello, World!~n").

% The io is a library in erlang