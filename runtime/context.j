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