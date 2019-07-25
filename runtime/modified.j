// scope Modified
// REQUIRES Table

globals
    integer _m = 0
    integer _globals = 0
endglobals

function _modified takes integer _uid returns boolean
    return Table#_get_boolean(_m, _uid)
endfunction

function _set_modified takes integer _uid returns nothing
    call Table#_set_boolean(_m, _uid, true)
endfunction

function _init takes nothing returns nothing
    set _m = Table#_alloc()
    set _globals = Table#_alloc()
endfunction
