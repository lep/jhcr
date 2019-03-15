// scope Wrap

globals
    integer _p = 0
    trigger _t = CreateTrigger()
endglobals

function _call_anything_around takes integer i returns nothing
    set _p = i
    call TriggerEvaluate(_t)
endfunction

