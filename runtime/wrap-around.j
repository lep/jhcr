// scope Wrap

globals
    integer _p = 0
    trigger _t1 = CreateTrigger()
    trigger _t2 = CreateTrigger()
    code _ret = null
    integer _args = 0
endglobals

function _i2code takes integer _i returns code
    set _p = _i
    call TriggerEvaluate(_t2)
    return _ret
endfunction

function _call_anything_around takes integer _i returns nothing
    set _p = _i
    call TriggerEvaluate(_t1)
endfunction

function _init takes nothing returns nothing
    set _args = Table#_alloc()
endfunction