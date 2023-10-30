// scope Init
// REQUIRES Print Parser Interpreter Wrap Auto Types Convert Ins Modified

globals
    boolean _already_init = false

    integer _seq = 1
    gamecache _GC = InitGameCache("JHCR.w3v")
    trigger _parse_runner = CreateTrigger()
endglobals

function _parse_wrapped takes nothing returns boolean
    local integer _g = 0
    local integer _i

    set API#_last_status = API#_OP_LIMIT

    call Preloader("JHCR-" + Auto#_cookie +"-"+ I2S(_seq)+".txt")
    set _i = GetStoredInteger(_GC, "seq", "0")
    if _i != _seq then
        set API#_last_status = API#_NO_DATA
        return false
    endif
    
    set _seq = _seq + 1

    set _i = GetStoredInteger(_GC, "functions", "count")
    set Parser#_prev_ins = 0
    loop
    exitwhen _i == 0
        call Parser#_parse_and_init(GetStoredString(_GC, "functions", I2S(_i)))
        set _i = _i -1
    endloop

    set Parser#_prev_ins = 0
    set _i = GetStoredInteger(_GC, "globals", "count")
    loop
    exitwhen _i == 0
        set _g = Parser#_parse_globals(GetStoredString(_GC, "globals", I2S(_i)), _g)
        set _i = _i - 1
    endloop

    // execute _g
    if _g != 0 then
        call Interpreter#_exec_globals(_g)
    endif

    set API#_last_status = API#_SUCCESS

    return true
endfunction

function _parse takes nothing returns nothing
    local integer _i = 0
    call TriggerEvaluate(_parse_runner)
    loop
    exitwhen _i >= API#_cnt
        call TriggerExecute(API#_triggers[_i])
        set _i = _i +1
    endloop
endfunction

function _i2code takes nothing returns boolean
    set Wrap#_ret = Auto#_i2code(Wrap#_p)
    return true
endfunction


function _init takes nothing returns nothing
    if _already_init then
        return
    endif
    set _already_init = true
    
    
    call TriggerAddCondition(Wrap#_t2, Condition(function _i2code))
    call TriggerAddCondition(_parse_runner, Condition(function _parse_wrapped))

    call Types#_init()
    call Wrap#_init()
    call Convert#_init()
    call Ins#_init()
    call Interpreter#_init()
    call Modified#_init()
    
    //call PreloadGenClear()
    //call PreloadGenStart()
endfunction
