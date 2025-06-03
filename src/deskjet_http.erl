%%% @author Teichmuller matheussilvatech@gmail.com
%%% @copyright (C) 2025, Teichmuller
%%% @doc 
%%%
%%% @end
%%% Created : 03 Jun 2025 by Teichmuller matheussilvatech@gmail.com
-module(deskjet_http).

-export([get/1,
         delete/1, 
         post/2, 
         patch/2, 
         put/2,
         put_with_file/2,
         post_with_file/2,
         patch_with_file/2,
         perform/1, 
         perform/2, 
         perform/3,
         perform_with_file/3, 
         loop/2, 
         loop_async/2, 
         start/0]).

start() ->
    inets:start(),
    ssl:start(),
    ok.

loop(Times, Fun) when Times =< 0 -> Fun();
loop(Times, Fun) when Times > 0 ->
    Fun(),
    loop(Times-1, Fun).

loop_async(Times, Fun) when Times =< 0 -> Fun();
loop_async(Times, Fun) when Times > 0 -> 
    spawn(Fun),
    loop_async(Times-1, Fun).
    

get(URL) -> 
    SSLConfig = [{verify, verify_peer}, {cacertfile, "../cert/cert.pem"}],
    HTTPOptions = [{ssl, SSLConfig}],
    case httpc:request(get, {URL, []}, HTTPOptions, []) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
            {ok, Body};
        {ok, {{_Version, StatusCode, _ReasonPhrase}, _Headers, Body}} ->
            {error, {status, StatusCode, Body}};
        {error, Reason} ->
            {error, Reason}
    end.

delete(URL) -> 
    SSLConfig = [{verify, verify_peer}, {cacertfile, "../cert/cert.pem"}],
    HTTPOptions = [{ssl, SSLConfig}],
    case httpc:request(delete, {URL, []}, HTTPOptions, []) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
            {ok, Body};
        {ok, {{_Version, StatusCode, _ReasonPhrase}, _Headers, Body}} ->
            {error, {status, StatusCode, Body}};
        {error, Reason} ->
            {error, Reason}
    end.

post(URL, Body) ->
    SSLConfig = [{verify, verify_peer}, {cacertfile, "../cert/cert.pem"}],
    HTTPOptions = [{ssl, SSLConfig}],
    Headers = [{"Content-Type", "application/json"}],
    RequestBody = list_to_binary(Body), 
    case httpc:request(post, {URL, Headers, "application/json", RequestBody}, HTTPOptions, []) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, RespBody}} ->
            {ok, RespBody};
        {ok, {{_Version, StatusCode, _ReasonPhrase}, _Headers, RespBody}} ->
            {error, {status, StatusCode, RespBody}};
        {error, Reason} ->
            {error, Reason}
    end.

patch(URL, Body) ->
    SSLConfig = [{verify, verify_peer}, {cacertfile, "../cert/cert.pem"}],
    HTTPOptions = [{ssl, SSLConfig}],
    Headers = [{"Content-Type", "application/json"}],
    RequestBody = list_to_binary(Body), 
    case httpc:request(patch, {URL, Headers, "application/json", RequestBody}, HTTPOptions, []) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, RespBody}} ->
            {ok, RespBody};
        {ok, {{_Version, StatusCode, _ReasonPhrase}, _Headers, RespBody}} ->
            {error, {status, StatusCode, RespBody}};
        {error, Reason} ->
            {error, Reason}
    end.

put(URL, Body) ->
    SSLConfig = [{verify, verify_peer}, {cacertfile, "../cert/cert.pem"}],
    HTTPOptions = [{ssl, SSLConfig}],
    Headers = [{"Content-Type", "application/json"}],
    RequestBody = list_to_binary(Body), 
    case httpc:request(put, {URL, Headers, "application/json", RequestBody}, HTTPOptions, []) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, RespBody}} ->
            {ok, RespBody};
        {ok, {{_Version, StatusCode, _ReasonPhrase}, _Headers, RespBody}} ->
            {error, {status, StatusCode, RespBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%%% logic to encapsulate.
post_with_file(URL, Path) ->
    case file:read_file(Path) of
        {ok, BinaryBody} ->
            deskjet_http:post(URL, binary_to_list(BinaryBody));
        {error, Reason} ->
            {error, {file_read, Reason}}
    end.

patch_with_file(URL, Path) ->
    case file:read_file(Path) of
        {ok, BinaryBody} ->
            deskjet_http:patch(URL, binary_to_list(BinaryBody));
        {error, Reason} ->
            {error, {file_read, Reason}}
    end.

put_with_file(URL, Path) ->
    case file:read_file(Path) of
        {ok, BinaryBody} ->
            deskjet_http:put(URL, binary_to_list(BinaryBody));
        {error, Reason} ->
            {error, {file_read, Reason}}
    end.

perform(URL) -> deskjet_http:get(URL).

perform(URL, Method) when Method == "get" -> deskjet_http:get(URL);

perform(URL, Method) when Method == "delete" -> deskjet_http:delete(URL).

perform(URL, Body, Method) when Method == "patch" -> 
    deskjet_http:patch(URL, Body);

perform(URL, Body, Method) when Method == "post" -> 
    deskjet_http:post(URL, Body).

perform_with_file(URL, Path, Method) when Method == "patch" -> 
    deskjet_http:patch_with_file(URL, Path);

perform_with_file(URL, Path, Method) when Method == "post" -> 
    deskjet_http:post_with_file(URL, Path);

perform_with_file(URL, Path, Method) when Method == "put" -> 
    deskjet_http:put_with_file(URL, Path).
