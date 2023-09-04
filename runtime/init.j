// scope Init
// REQUIRES Print Parser Interpreter Wrap Auto Types Convert Ins Modified

globals
    boolean _already_init = false

    integer _seq = 0
    gamecache _GC = InitGameCache("JHCR.w3v")
endglobals

function _parse takes nothing returns nothing
    local integer _g = 0
    local integer _i = 1
    local integer _cnt

#if PATCH_LVL >= 133
    call Preloader("JHCR-"+I2S(_seq)+".txt")
    set _seq = _seq + 1
#else
    call Preloader("JHCR.txt")
#endif

    set _cnt = GetStoredInteger(_GC, "functions", "count")

    set Parser#_prev_ins = 0
    loop
    exitwhen _i > _cnt
        call Parser#_parse_and_init(GetStoredString(_GC, "functions", I2S(_i)))
        set _i = _i +1
    endloop

    set _i = 1
    set Parser#_prev_ins = 0
    set _cnt = GetStoredInteger(_GC, "globals", "count")
    loop
    exitwhen _i > _cnt
        set _g = Parser#_parse_globals(GetStoredString(_GC, "globals", I2S(_i)), _g)
        set _i = _i + 1
    endloop

    // execute _g
    if _g != 0 then
        call Interpreter#_exec_globals(_g)
    endif

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

    call Types#_init()
    call Wrap#_init()
    call Convert#_init()
    call Ins#_init()
    call Interpreter#_init()
    call Modified#_init()
    
    //call PreloadGenClear()
    //call PreloadGenStart()
endfunction
