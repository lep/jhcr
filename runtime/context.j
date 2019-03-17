// scope Context

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

function _destroy takes integer ctx returns nothing
    call Table#_destroy(_locals[ctx])
    call Table#_destroy(_bindings[ctx]) 
    call _free(ctx)
endfunction