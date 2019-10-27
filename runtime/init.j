// scope Init
// REQUIRES Print Parser Interpreter Wrap Auto Types Convert Ins Modified

globals
    integer array _fn_ids
    constant integer _fn_max = 24
    
    integer array _g_ids
    constant integer _g_max = 9
    
    boolean _already_init = false
    
    
    integer _sync_globals_count
    integer _sync_function_count
    
    integer _sync_globals_max
    integer _sync_function_max
    
    string array _sync_globals_data
    string array _sync_function_data
endglobals

function _parse_from_globals takes nothing returns nothing
    local integer _cnt = _sync_function_max
    local integer _g = 0
    
    set Parser#_prev_ins = 0
    loop
    exitwhen _cnt == 0
        call Parser#_parse_and_init(_sync_function_data[_cnt])
        set _cnt = _cnt -1
    endloop
    
    set _cnt = _sync_globals_max
    set Parser#_prev_ins = 0
    loop
    exitwhen _cnt == 0
        set _g = Parser#_parse_globals(_sync_globals_data[_cnt], _g)
        set _cnt = _cnt -1
    endloop
    
    // execute _g
    if _g != 0 then
        call Interpreter#_exec_globals(_g)
    endif
endfunction

function _received_synced takes nothing returns nothing
    local string _prefix = BlzGetTriggerSyncPrefix()
    local string _data = BlzGetTriggerSyncData()
    
    if _prefix == "JHCR-GC" then
        set _sync_globals_max = S2I(_data)
        set _sync_globals_count = _sync_globals_max
        
    elseif _prefix == "JHCR-FC" then
        set _sync_function_max = S2I(_data)
        set _sync_function_count = _sync_function_max
        
    elseif _prefix == "JHCR-GD" then
        set _sync_globals_data[_sync_globals_count] = _data
        set _sync_globals_count = _sync_globals_count -1
        
        // once we've synced all data which is when _sync_globals_count is equal
        // to 0 we parse and execute everything
        if _sync_function_count == 0 then
            call _parse_from_globals()
        endif
    elseif _prefix == "JHCR-FD" then
        set _sync_function_data[_sync_function_count] = _data
        set _sync_function_count = _sync_function_count -1
    endif
endfunction

function _parse_and_sync takes nothing returns nothing
    local integer _cnt = _fn_max
    local string array _tmp
    local integer _g = 0
    
    //call PreloadGenEnd("trace.txt")
    //call Print#_print("ESC")

    loop
    exitwhen _cnt == 0
        set _tmp[_cnt] = BlzGetAbilityTooltip(_fn_ids[_cnt], 1)
        set _cnt = _cnt -1
    endloop
    
    set _cnt = _g_max
    loop
    exitwhen _cnt == 0
        set _tmp[_cnt + _fn_max] = BlzGetAbilityTooltip(_g_ids[_cnt], 1)
        set _cnt = _cnt -1
    endloop

    call Preloader("JHCR.txt")
    
    set _cnt = GetPlayerTechMaxAllowed(Player(0), 1)
    call BlzSendSyncData("JHCR-FC", I2S(_cnt))
    loop
    exitwhen _cnt == 0
        call BlzSendSyncData("JHCR-FD", BlzGetAbilityTooltip(_fn_ids[_cnt], 0))
        call BlzSetAbilityTooltip(_fn_ids[_cnt], _tmp[_cnt], 0)
        set _cnt = _cnt -1
    endloop
    
    set _cnt = GetPlayerTechMaxAllowed(Player(0), 2)
    call BlzSendSyncData("JHCR-GC", I2S(_cnt))
    loop
    exitwhen _cnt == 0
        call BlzSendSyncData("JHCR-GD", BlzGetAbilityTooltip(_g_ids[_cnt], 0))
        call BlzSetAbilityTooltip(_g_ids[_cnt], _tmp[_cnt + _fn_max], 0)
        set _cnt = _cnt -1
    endloop

endfunction

function _parse takes nothing returns nothing
    local integer _cnt = _fn_max
    local string array _tmp
    local integer _g = 0
    
    //call PreloadGenEnd("trace.txt")
    //call Print#_print("ESC")

    loop
    exitwhen _cnt == 0
        set _tmp[_cnt] = BlzGetAbilityTooltip(_fn_ids[_cnt], 1)
        set _cnt = _cnt -1
    endloop
    
    set _cnt = _g_max
    loop
    exitwhen _cnt == 0
        set _tmp[_cnt + _fn_max] = BlzGetAbilityTooltip(_g_ids[_cnt], 1)
        set _cnt = _cnt -1
    endloop

    call Preloader("JHCR.txt")
    
    set _cnt = GetPlayerTechMaxAllowed(Player(0), 1)
    set _sync_function_max = _cnt
    loop
    exitwhen _cnt == 0
        set _sync_function_data[_cnt] = BlzGetAbilityTooltip(_fn_ids[_cnt], 0)
        call BlzSetAbilityTooltip(_fn_ids[_cnt], _tmp[_cnt], 0)
        set _cnt = _cnt -1
    endloop
    
    set _cnt = GetPlayerTechMaxAllowed(Player(0), 2)
    set _sync_globals_max = _cnt
    loop
    exitwhen _cnt == 0
        set _sync_globals_data[_cnt] = BlzGetAbilityTooltip(_g_ids[_cnt], 0)
        call BlzSetAbilityTooltip(_g_ids[_cnt], _tmp[_cnt + _fn_max], 0)
        set _cnt = _cnt -1
    endloop
    
    call _parse_from_globals()

endfunction

function _i2code takes nothing returns nothing
    set Wrap#_ret = Auto#_i2code(Wrap#_p)
endfunction


function _init takes nothing returns nothing
    local trigger _t
    local integer _i
    if _already_init then
        return
    endif
    set _already_init = true
    
    set _fn_ids[1] = 'Agyv'
    set _fn_ids[2] = 'Aflk'
    set _fn_ids[3] = 'Agyb'
    set _fn_ids[4] = 'Ahea'
    set _fn_ids[5] = 'Ainf'
    set _fn_ids[6] = 'Aslo'
    set _fn_ids[7] = 'Afla'
    set _fn_ids[8] = 'Amls'
    set _fn_ids[9] = 'Adis'
    set _fn_ids[10] = 'Acmg'
    set _fn_ids[11] = 'Amdf'
    set _fn_ids[12] = 'Adts'
    set _fn_ids[13] = 'Aast'
    set _fn_ids[14] = 'Aetf'
    set _fn_ids[15] = 'Absk'
    set _fn_ids[16] = 'Alsh'
    set _fn_ids[17] = 'Aens'
    set _fn_ids[18] = 'Adcn'
    set _fn_ids[19] = 'Aliq'
    set _fn_ids[20] = 'Aspl'
    set _fn_ids[21] = 'Aven'
    set _fn_ids[22] = 'Ablo'
    set _fn_ids[23] = 'Acpf'
    set _fn_ids[24] = 'Awar'
    
    set _g_ids[1] = 'Adec'
    set _g_ids[2] = 'Aeat'
    set _g_ids[3] = 'Aco3'
    set _g_ids[4] = 'Acoh'
    set _g_ids[5] = 'Abrf'
    set _g_ids[6] = 'Aro2'
    set _g_ids[7] = 'Aro1'
    set _g_ids[8] = 'Aegr'
    set _g_ids[9] = 'Aren'
    
    
    
    call TriggerAddCondition(Wrap#_t2, Condition(function _i2code))

    call Types#_init()
    call Wrap#_init()
    call Convert#_init()
    call Ins#_init()
    call Interpreter#_init()
    call Modified#_init()
    
    set _i = 0
    set _t = CreateTrigger()
    loop
    exitwhen _i == GetBJMaxPlayerSlots()
        call BlzTriggerRegisterPlayerSyncEvent(_t, Player(_i), "JHCR-FC", false)
        call BlzTriggerRegisterPlayerSyncEvent(_t, Player(_i), "JHCR-FD", false)
        call BlzTriggerRegisterPlayerSyncEvent(_t, Player(_i), "JHCR-GD", false)
        call BlzTriggerRegisterPlayerSyncEvent(_t, Player(_i), "JHCR-GC", false)
        set _i = _i +1
    endloop
    call TriggerAddAction(_t, function _received_synced)
    
    //call PreloadGenClear()
    //call PreloadGenStart()
endfunction