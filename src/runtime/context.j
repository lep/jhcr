// scope Context
// REQUIRES Table

globals
    #include "alloc-globals.j"
    
    // struct Context
    //   Table locals
    //   Table bindings
    //   Table labels
    //   Context parent
    //   Instruction pc
    integer array _locals
    integer array _bindings
    integer array _labels
    integer array _parent
    integer array _pc
    
endglobals

#include "alloc.j"

function _destroy takes integer _ctx returns nothing
    call Table#_destroy(_locals[_ctx])
    call Table#_destroy(_bindings[_ctx])
    call _free(_ctx)
endfunction
