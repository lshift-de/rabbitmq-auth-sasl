%%%-------------------------------------------------------------------
%%% @author mateuszkorszun
%%% @copyright (C) 2015, LShift
%%% @doc
%%%
%%% @end
%%% Created : 14. Jul 2015 10:29 AM
%%%-------------------------------------------------------------------
-author("mateuszkorszun").

-define(INFO(F, A), rabbit_log:info("[~p] " ++ F, [?MODULE | A])).
-define(ERR(F, A), rabbit_log:error("[~p] " ++ F, [?MODULE | A])).

-define(INFO(MSG), rabbit_log:info("[~p] " ++ MSG, [?MODULE])).
-define(ERR(MSG), rabbit_log:error("[~p] " ++ MSG, [?MODULE])).
