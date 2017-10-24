globals
    code _call_anything_ref
    force _call_anything_f
    integer _call_anything_p
endglobals

function _call_anything_around takes integer i returns nothing
    set _call_anything_p = i
    call ForForce(_call_anything_f, _call_anything_ref)
endfunction
