// scope Init
// REQUIRES Print Parser Interpreter Wrap Auto Types Convert Ins Modified

globals
    boolean _already_init = false

    integer _seq = 1
    gamecache _GC = InitGameCache("JHCR.w3v")
    trigger _parse_runner = CreateTrigger()

    string _param1
    integer _param2
    integer _ret
    force _force
endglobals

function _parse_and_init takes nothing returns nothing
    call Parser#_parse_and_init(_param1)
endfunction
function _parse_and_init_wrapped takes string _s returns nothing
    set _param1 = _s
    call ForForce(_force, function _parse_and_init)
endfunction

function _parse_globals takes nothing returns nothing
    set _ret = Parser#_parse_globals(_param1, _param2)
endfunction
function _parse_globals_wrapped takes string _s, integer _g returns integer
    set _param1 = _s
    set _param2 = _g
    call ForForce(_force, function _parse_globals)
    return _ret
endfunction

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
        call _parse_and_init_wrapped(GetStoredString(_GC, "functions", I2S(_i)))
        set _i = _i -1
    endloop

    set Parser#_prev_ins = 0
    set _i = GetStoredInteger(_GC, "globals", "count")
    loop
    exitwhen _i == 0
        set _g = _parse_globals_wrapped(GetStoredString(_GC, "globals", I2S(_i)), _g)
        // set _g = Parser#_parse_globals(GetStoredString(_GC, "globals", I2S(_i)), _g)
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

    set _force = CreateForce()
    call ForceAddPlayer(_force, Player(0))
    
    //call PreloadGenClear()
    //call PreloadGenStart()
endfunction
