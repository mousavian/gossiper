-module(gossiper).

-export ([start_link/0, init/1, handle_info/2]).
-export ([code_change/3, handle_call/3, handle_cast/2, terminate/2]).

-define (INTERVAL, 2000).
-define (KAFKA_HOST, "kafka.dev").
-define (KAFKA_PORT, 9092).
-define (TOPIC, <<"servers_resources">>).

-behavior(gen_server).


start_link() ->
    application:set_env(ekaf, ekaf_bootstrap_broker, {?KAFKA_HOST, ?KAFKA_PORT}),
    {ok, _} = application:ensure_all_started(ekaf),
    {ok, _} = application:ensure_all_started(os_mon),
    gen_server:start_link(?MODULE, [], []).

init([])->
    timer:send_interval(?INTERVAL, interval),
    {ok, start}.

handle_info(interval, _State)->
    % Host
    {ok, HostnameStr} = inet:gethostname(),
    Hostname = list_to_binary(HostnameStr),

    % CPU
    SystemLoad = cpu_sup:avg1(),
    CpuUtilSpec = cpu_sup:util([per_cpu]),
    CpuUsage = [ 
            {list_to_binary(integer_to_list(CpuId)), round_to_decimal(Busy)}
            || {CpuId, Busy, _NonBusy, _Misc} <- CpuUtilSpec
        ],

    % DISK
    DiskData = disksup:get_disk_data(),
    DiskRootPercentage = hd([ Capacity || {Id, _KByte, Capacity} <- DiskData, Id=:="/"]),

    % MEMORY
    MemDataList = memsup:get_system_memory_data(),
    RamData = [ X || X <- MemDataList,
        element(1, X)=:=free_memory orelse element(1, X)=:=system_total_memory],

    SwapData = [ X || X <- MemDataList,
        element(1, X)=:=total_swap orelse element(1, X)=:=free_swap],

    RamPercentage = get_ram_percentage(RamData),
    SwapPercentage = get_swap_percentage(SwapData),
    
    JsonData = jiffy:encode({[
            {hostname, Hostname},
            {systemload, SystemLoad},
            {cpu, {CpuUsage}},
            {disk, DiskRootPercentage},
            {memory, RamPercentage},
            {swap, SwapPercentage}
        ]}),
    
    ekaf:produce_sync(?TOPIC, JsonData),
    {noreply, continue}.


get_ram_percentage([{free_memory, FreeSize}|Rest]) ->
    {system_total_memory, TotalSize} = hd(Rest),
    round_to_decimal(100 - (FreeSize / TotalSize * 100));
get_ram_percentage([{system_total_memory, TotalSize}|Rest]) ->
    {free_memory, FreeSize} = hd(Rest),
    round_to_decimal(100 - (FreeSize / TotalSize * 100)).


get_swap_percentage([{free_swap, FreeSize}|Rest]) ->
    {total_swap, TotalSize} = hd(Rest),
    round_to_decimal(100 - (FreeSize / TotalSize * 100));
get_swap_percentage([{total_swap, TotalSize}|Rest]) ->
    {free_swap, FreeSize} = hd(Rest),
    round_to_decimal(100 - (FreeSize / TotalSize * 100)).


round_to_decimal(Number) ->
    round(Number*100)/100.


code_change(_OldVsn, State, _Extra) -> {ok, State}. 
handle_call(_V1, _V2, _V3) -> {stop, normal, ok, _V3}.
handle_cast(_Msg, State) -> {noreply, State}.
terminate(_V1, _V2) -> ok.
