%% @author https://github.com/mkrogemann
%% @copyright 2014 author
%%
%% @doc OAuth2 mongoDB backend
%% This module is influenced by the code found in https://github.com/kivra/oauth2
%% In particular, the behaviour declared in module 'oauth2_backend' of the
%% repo above has been influential in the design of 'oauth2_mongo'
%% However, only a small subset of the behaviour declared in above project
%% is implemented here.

-module(oauth2_mongo).


-include("include/oauth2_request.hrl").


-export([start/0, stop/0, add_resowner/2, add_resowner/3, delete_resowner/1]).
-export([authenticate_username_password/3]).


-define(ACCESS_TOKEN_COLLECTION, access_tokens).
-define(RES_OWNER_COLLECTION, resowners).


-record(resowner, {
        recindex    =  1,
        docid       :: binary(),
        username    :: binary(),
        password    :: binary(),
        scope       :: [binary()]
    }).

-record(access_token, {
        recindex    =  1,
        docid       :: binary(),
        token       :: binary(),
        expires     :: integer(),
        token_type  :: binary(),
        scope       :: [binary()]
    }).



-spec start() -> ok.
%% make all mongo-relevant records known to erlmongo so they can be
%% stored / retrieved etc. Also verify that we can write and read.
start() ->
    ok = mongodb:singleServer(def),
    ok = mongodb:connect(def),

    mongoapi:recinfo(#access_token{}, record_info(fields, access_token)),
    mongoapi:recinfo(#resowner{}, record_info(fields, resowner)),

    Mong = mongoapi:new(def,<<"erlmongo">>),

    case Mong:update([{#access_token.token, "placeholder"}],
                        #access_token{token = "placeholder",
                                        expires = "0",
                                        token_type = "none",
                                        scope = ""},
                        [upsert]) of
        ok ->
            ok;
        _ ->
            io:format("cannot connect to mongoDB!~n")
    end,
    ok = Mong:ensureIndex(#access_token{}, [{#access_token.token, 1}], [{"unique",true}]),
    ok = Mong:ensureIndex(#resowner{}, [{#resowner.username, 1}], [{"unique",true}]),
    ok.


-spec stop() -> ok.
stop() ->
    ok.


-spec add_resowner(Username, Password, Scope) -> ok when
    Username  :: binary(),
    Password  :: binary(),
    Scope     :: [binary()].
add_resowner(Username, Password, Scope) ->
    Mong = mongoapi:new(def, <<"erlmongo">>),
    case Mong:save(resowner,
                    #resowner{username = Username, password = Password,
                    scope = Scope}) of
        %% Even if we cannot insert the data due to a unique index, we still get back
        %% an object_id. Erlmongo currently does not call getLastError.
        %% See more about it in this open issue:
        %% https://github.com/SergejJurecko/erlmongo/issues/22
        {oid, <<_Oid/binary>>} -> ok;
        _ -> {error, writei_failed}
    end.


-spec add_resowner(Username, Password) -> ok when
    Username :: binary(),
    Password :: binary().
add_resowner(Username, Password) ->
    add_resowner(Username, Password, []),
    ok.


-spec delete_resowner(Username) -> ok when
    Username :: binary().
delete_resowner(Username) ->
    Mong = mongoapi:new(def, <<"erlmongo">>),
    %% Again, we have no way to find out whether the execution was successful or not,
    %% given the current lack of calls to 'getlasterror'
    case Mong:remove(#resowner{}, [{#resowner.username, list_to_binary(Username)}]) of
        ok -> ok;
        _ -> {error, delete_failed}
    end.


authenticate_username_password(Username, Password, _AppContext) ->
    start(), % TODO: convert oauth2_mongo into an application
    User = binary_to_list(Username),
    case read_resowner(User) of
        {ok, ResOwner} ->
            case verify_password(ResOwner, Password) of
                true -> {ok, ResOwner};
                false -> {error, badpass}
            end;
        _ ->
            {error, notfound}
    end.


%%% Please note that we do not use any salting/hashing here.
%%% You should obviously not do this in prodution, ever!
%%% Proper password hashing is out of scope for this Demo.
verify_password(ResOwner, Password) when is_binary(Password) ->
    Password =:= ResOwner#resowner.password;
verify_password(ResOwner, Password) ->
    list_to_binary(Password) =:= ResOwner#resowner.password.


read_resowner(Username) ->
    Mong = mongoapi:new(def, <<"erlmongo">>),
    Mong:findOne(#resowner{username = Username}).

