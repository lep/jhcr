// scope Wrap

globals
    integer _p = 0
    trigger _t = CreateTrigger()
    integer _args = 0
endglobals

function _call_anything_around takes integer i returns nothing
    set _p = i
    call TriggerEvaluate(_t)
endfunction

function _init takes nothing returns nothing
    set _args = Table#_alloc()
endfunction