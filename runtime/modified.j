// scope Modified

globals
    integer _m
endglobals

function _modified takes integer uid returns boolean
    return Table#_get_boolean(_m, uid)
endfunction

function _set_modified takes integer uid returns nothing
    call Table#_set_boolean(_m, uid, true)
endfunction

function _init takes nothing returns nothing
    set _m = Table#_alloc()
endfunction
