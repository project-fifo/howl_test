-module(libsnarl_intercepts).

-compile(export_all).
-include("intercept.hrl").
-define(M, libsnarl_orig).

auth_ok(_User, _Pass) ->
    {ok, {token, <<"intercepted">>}}.

allowed_ok({token, <<"intercepted">>}, _) ->
    true.
