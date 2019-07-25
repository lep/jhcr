// scope List
// REQUIRES 

globals
    #include "alloc-globals.j"
    
    integer array _next

endglobals

#include "alloc.j"

function _cons takes integer _tl returns integer
    local integer _new = _alloc()
    set _next[_new] = _tl
    return _new
endfunction

function _destroy takes integer _l returns nothing
    loop
    exitwhen _l == 0
        call _free(_l)
        set _l = _next[_l]
    endloop
endfunction