// scope Wrap

globals
    integer _p = 0
    trigger _t1 = CreateTrigger()
    trigger _t2 = CreateTrigger()
    code _ret = null
    
    // _args :: Table
    integer _args = 0
    
    // _name2id :: StringTable
    integer _name2id = 0
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

function _ExecuteFunc takes string _s returns nothing
    local integer _id = StringTable#_get(_name2id, _s)
    if _id < 0 then
        call ExecuteFunc("Auto#_dummyFunction_" + I2S(-_id))
    else
        call ExecuteFunc(_s)
    endif
endfunction

function _init takes nothing returns nothing
    set _args = Table#_alloc()
    set _name2id = Table#_alloc()
endfunction