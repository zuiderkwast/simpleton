%% @doc A module for working with sets of variables.  They can be used for
%% detecting the last use of every variable and such things.
%%
%% This module provides the same interface as the module `sets`, but with
%% an unspecified representation.
-module(lsrvarsets).

-define(SETS, ordsets). % The module
-define(SET, ordset).   % The type

-export_type([lsrvarset/0]).
-export([from_list/1, is_element/2, new/0, subtract/2, union/1, union/2]).

-opaque lsrvarset() :: ?SETS:?SET(string()).

new() ->
	?SETS:new().

is_element(A, B) ->
	?SETS:is_element(A, B).

subtract(A, B) ->
	?SETS:subtract(A, B).

union(A) ->
	?SETS:union(A).

union(A, B) ->
	?SETS:union(A, B).

from_list(A) ->
	?SETS:from_list(A).

