// scope Interpreter

globals
    integer _fresh = 0
endglobals

// interp :: Context -> IO Context
function _step takes integer ctx returns integer
    local integer op = Context#_pc[ctx]
    local integer t = Ins#_op[op]
    local integer tmp
    local integer fn
    //call BJDebugMsg("Interpreting: ")
    //call Ins#_print(op)
    if t == Ins#_Set then
    
        #define macro(ty) Table@_set_##ty(Context@_locals[ctx], Ins@_a1[op], Table@_get_##ty(Context@_locals[ctx], Ins@_a2[op]))
        #define ty Ins#_type[op]
        #include "g-type-bin.j"
        #undef ty
        #undef macro
        
    elseif t == Ins#_SetLocalArray then
        #define macro(ty) Table@_set_##ty(Context@_locals[ctx], Ins@_a1[op] + Ins@_a2[op], Table@_get_##ty(Context@_locals[ctx], Ins@_a3[op]))
        #define ty Ins#_type[op]
        #include "g-type-bin.j"
        #undef ty
        #undef macro
    
    elseif t == Ins#_GetLocalArray then
        #define macro(ty) Table@_set_##ty(Context@_locals[ctx], Ins@_a1[op], Table@_get_##ty(Context@_locals[ctx], Ins@_a2[op] + Ins@_a3[op]))
        #define ty Ins#_type[op]
        #include "g-type-bin.j"
        #undef ty
        #undef macro

    elseif t == Ins#_Lt then
        if Ins#_type[op] == Types#_integer then
            call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op], Table#_get_integer(Context#_locals[ctx], Ins#_a2[op]) < Table#_get_integer(Context#_locals[ctx], Ins#_a2[op]))
        else
            call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op], Table#_get_real(Context#_locals[ctx], Ins#_a2[op]) < Table#_get_real(Context#_locals[ctx], Ins#_a3[op]))
        endif
        
    elseif t == Ins#_Le then
        if Ins#_type[op] == Types#_integer then
            call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op], Table#_get_integer(Context#_locals[ctx], Ins#_a2[op]) <= Table#_get_integer(Context#_locals[ctx], Ins#_a3[op]))
        else
            call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op], Table#_get_real(Context#_locals[ctx], Ins#_a2[op]) <= Table#_get_real(Context#_locals[ctx], Ins#_a3[op]))
        endif
        
    elseif t == Ins#_Gt then
        if Ins#_type[op] == Types#_integer then
            call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op], Table#_get_integer(Context#_locals[ctx], Ins#_a2[op]) > Table#_get_integer(Context#_locals[ctx], Ins#_a3[op]))
        else
            call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op], Table#_get_real(Context#_locals[ctx], Ins#_a2[op]) > Table#_get_real(Context#_locals[ctx], Ins#_a3[op]))
        endif
        
    elseif t == Ins#_Ge then
        if Ins#_type[op] == Types#_integer then
            call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op], Table#_get_integer(Context#_locals[ctx], Ins#_a2[op]) >= Table#_get_integer(Context#_locals[ctx], Ins#_a3[op]))
        else
            call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op], Table#_get_real(Context#_locals[ctx], Ins#_a2[op]) >= Table#_get_real(Context#_locals[ctx], Ins#_a3[op]))
        endif
        
    elseif t == Ins#_Eq then
        if Ins#_type[op] == Types#_integer then
            call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op], Table#_get_integer(Context#_locals[ctx], Ins#_a2[op]) == Table#_get_integer(Context#_locals[ctx], Ins#_a3[op]))
            
        elseif Ins#_type[op] == Types#_real then
            call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op], Table#_get_real(Context#_locals[ctx], Ins#_a2[op]) == Table#_get_real(Context#_locals[ctx], Ins#_a3[op]))
        
        elseif Ins#_type[op] == Types#_string then
            call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op], Table#_get_string(Context#_locals[ctx], Ins#_a2[op]) == Table#_get_string(Context#_locals[ctx], Ins#_a3[op]))
            
        elseif Ins#_type[op] == Types#_boolean then
            call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op], Table#_get_boolean(Context#_locals[ctx], Ins#_a2[op]) == Table#_get_boolean(Context#_locals[ctx], Ins#_a3[op]))

        else
            // handle derived type (except the other few types)
            call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op], Table#_get_fogstate(Context#_locals[ctx], Ins#_a2[op]) == Table#_get_fogstate(Context#_locals[ctx], Ins#_a3[op]))
        endif
        
    elseif t == Ins#_Neq then
        if Ins#_type[op] == Types#_integer then
            call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op], Table#_get_integer(Context#_locals[ctx], Ins#_a2[op]) != Table#_get_integer(Context#_locals[ctx], Ins#_a3[op]))
            
        elseif Ins#_type[op] == Types#_real then
            call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op], Table#_get_real(Context#_locals[ctx], Ins#_a2[op]) != Table#_get_real(Context#_locals[ctx], Ins#_a3[op]))
        
        elseif Ins#_type[op] == Types#_string then
            call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op], Table#_get_string(Context#_locals[ctx], Ins#_a2[op]) != Table#_get_string(Context#_locals[ctx], Ins#_a3[op]))
            
        elseif Ins#_type[op] == Types#_boolean then
            call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op], Table#_get_boolean(Context#_locals[ctx], Ins#_a2[op]) != Table#_get_boolean(Context#_locals[ctx], Ins#_a3[op]))

        else
            // handle derived type (except the other few types)
            call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op], Table#_get_fogstate(Context#_locals[ctx], Ins#_a2[op]) != Table#_get_fogstate(Context#_locals[ctx], Ins#_a3[op]))
        endif
        
    elseif t == Ins#_Add then
        if Ins#_type[op] == Types#_integer then
            call Table#_set_integer(Context#_locals[ctx], Ins#_a1[op], Table#_get_integer(Context#_locals[ctx], Ins#_a2[op]) + Table#_get_integer(Context#_locals[ctx], Ins#_a3[op]))
        else
            call Table#_set_real(Context#_locals[ctx], Ins#_a1[op], Table#_get_real(Context#_locals[ctx], Ins#_a2[op]) + Table#_get_real(Context#_locals[ctx], Ins#_a3[op]))
        endif
        
    elseif t == Ins#_Sub then
        if Ins#_type[op] == Types#_integer then
            call Table#_set_integer(Context#_locals[ctx], Ins#_a1[op], Table#_get_integer(Context#_locals[ctx], Ins#_a2[op]) - Table#_get_integer(Context#_locals[ctx], Ins#_a3[op]))
        else
            call Table#_set_real(Context#_locals[ctx], Ins#_a1[op], Table#_get_real(Context#_locals[ctx], Ins#_a2[op]) - Table#_get_real(Context#_locals[ctx], Ins#_a3[op]))
        endif
        
    elseif t == Ins#_Mul then
        if Ins#_type[op] == Types#_integer then
            call Table#_set_integer(Context#_locals[ctx], Ins#_a1[op], Table#_get_integer(Context#_locals[ctx], Ins#_a2[op]) * Table#_get_integer(Context#_locals[ctx], Ins#_a3[op]))
        else
            call Table#_set_real(Context#_locals[ctx], Ins#_a1[op], Table#_get_real(Context#_locals[ctx], Ins#_a2[op]) * Table#_get_real(Context#_locals[ctx], Ins#_a3[op]))
        endif
        
    elseif t == Ins#_Div then
        if Ins#_type[op] == Types#_integer then
            call Table#_set_integer(Context#_locals[ctx], Ins#_a1[op], Table#_get_integer(Context#_locals[ctx], Ins#_a2[op]) / Table#_get_integer(Context#_locals[ctx], Ins#_a3[op]))
        else
            call Table#_set_real(Context#_locals[ctx], Ins#_a1[op], Table#_get_real(Context#_locals[ctx], Ins#_a2[op]) / Table#_get_real(Context#_locals[ctx], Ins#_a3[op]))
        endif
    elseif t == Ins#_Mod then
        call Table#_set_integer(Context#_locals[ctx], Ins#_a1[op], ModuloInteger(Table#_get_integer(Context#_locals[ctx], Ins#_a2[op]) , Table#_get_integer(Context#_locals[ctx], Ins#_a3[op])))
        
    elseif t == Ins#_Not then
        call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op], not Table#_get_boolean(Context#_locals[ctx], Ins#_a2[op]))
        
    elseif t == Ins#_Label then
        // do nothing
        
    elseif t == Ins#_Jmp then
        set Context#_pc[ctx] = Ins#_next[Table#_get_integer(Context#_labels[ctx], Ins#_a1[op])]
        return ctx
        
    elseif t == Ins#_JmpT then
        if Table#_get_boolean(Context#_locals[ctx], Ins#_a1[op]) then
            set Context#_pc[ctx] = Table#_get_integer(Context#_labels[ctx], Ins#_a2[op])
            return ctx
        endif
        
    elseif t == Ins#_Ret then
        #define macro(ty) Table@_set_##ty(Context@_locals[Context@_parent[ctx]], Ins@_a1[Context@_pc[Context@_parent[ctx]]], Table@_get_##ty(Context@_locals[ctx], 0))

        #define ty Ins#_type[op]
        #include "g-type-bin.j"
        #undef ty
        #undef macro
        
        set tmp = ctx
        set ctx = Context#_parent[ctx]
        call Context#_destroy(tmp)
    elseif t == Ins#_Call then
        set fn = Ins#_a2[op]
        if fn < 0 or Modified#_modified(fn) then
            // user-defined or reloaded function
            
            set tmp = Context#_alloc()
            
            set Context#_parent[tmp]   = ctx
            set Context#_pc[tmp]       = Parser#_fn_entry[fn + 100]
            set Context#_labels[tmp]   = Parser#_fn_labels[fn + 100]
            set Context#_locals[tmp]   = Context#_bindings[ctx]
            set Context#_bindings[tmp] = Table#_alloc()
            
            return tmp
        elseif fn > 0 then
            // auto generated call for pre-defined functions
            call Auto#_call_predefined(Ins#_a1[op], fn, ctx)
            
        endif

    elseif t == Ins#_Bind then // should be same as Set except different target table
        #define macro(ty) Table@_set_##ty(Context@_bindings[ctx], Ins@_a1[op], Table@_get_##ty(Context@_locals[ctx], Ins@_a2[op]))
        #define ty Ins#_type[op]
        #include "g-type-bin.j"
        #undef ty
        #undef macro
 
    elseif t == Ins#_SetGlobal then
        #define macro(ty) Auto@_set_global_##ty(Ins@_a1[op], Table@_get_##ty(Context@_locals[ctx], Ins@_a2[op]))
        #define ty Ins#_type[op]
        #include "g-type-bin.j"
        #undef ty
        #undef macro

    elseif t == Ins#_GetGlobal then
        #define macro(ty) Table@_set_##ty(Context@_locals[ctx], Ins@_a1[op], Auto@_get_global_##ty(Ins@_a2[op]))
        #define ty Ins#_type[op]
        #include "g-type-bin.j"
        #undef macro

    elseif t == Ins#_SetGlobalArray then
        #define macro(ty) Auto@_array_set_global_##ty(Ins@_a1[op], Ins@_a2[op], Table@_get_##ty(Context@_locals[ctx], Ins@_a3[op]))
        #define ty Ins#_type[op]
        #include "g-type-bin.j"
        #undef ty
        #undef macro

    elseif t == Ins#_GetGlobalArray then
        #define macro(ty) Table@_set_##ty(Context@_locals[ctx], Ins@_a1[op], Auto@_array_get_global_##ty(Ins@_a2[op], Ins@_a3[op]))
        #define ty Ins#_type[op]
        #include "g-type-bin.j"
        #undef ty
        #undef macro

    elseif t == Ins#_Lit then
        if Ins#_type[op] == Types#_integer then
            call Table#_set_integer(Context#_locals[ctx], Ins#_a1[op], Ins#_integer[op])
            
        elseif Ins#_type[op] == Types#_real then
            call Table#_set_real(Context#_locals[ctx], Ins#_a1[op], Ins#_real[op])
            
        elseif Ins#_type[op] == Types#_string then
            call Table#_set_string(Context#_locals[ctx], Ins#_a1[op], Ins#_string[op])
        
        elseif Ins#_type[op] == Types#_boolean then
            call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op], Ins#_boolean[op])
            
        elseif Ins#_type[op] == Types#_code then
            call Table#_set_integer(Context#_locals[ctx], Ins#_a1[op], Ins#_integer[op])
        else // null // TODO?
            
        endif
    elseif t == Ins#_Convert then
        call Convert#_convert(Ins#_type[op], Ins#_a1[op], Ins#_a2[op], Ins#_a3[op], ctx)
    endif
    
    set Context#_pc[ctx] = Ins#_next[Context#_pc[ctx]]
    return ctx
endfunction


//function _start_interpreter takes integer fn returns nothing
//    local integer ctx = Context#_alloc()
//    set Context#_pc[ctx]     = Parser#_fn_entry[fn + 100]
//    set Context#_parent[ctx] = 0
//    set Context#_labels[ctx] = Table#_get_integer(Parser#_fn_labels, fn)
//    set Context#_locals[ctx] = _fresh
//    set _fresh = Context#_alloc()
//    
//    loop
//    exitwhen ctx == 0
//        set ctx = _step(ctx)
//    endloop
//endfunction

function _start_interpreter_wrap takes nothing returns boolean
    local integer ctx = Context#_alloc()
    local integer parent = Context#_alloc()
    
    set Context#_pc[ctx]       = Parser#_fn_entry[Wrap#_p + 100]
    set Context#_parent[ctx]   = parent
    set Context#_labels[ctx]   = Parser#_fn_labels[Wrap#_p + 100]
    set Context#_locals[ctx]   = Table#_alloc()
    set Context#_bindings[ctx] = Table#_alloc()
    
    set Context#_pc[parent] = 0
    set Context#_labels[parent] = 0
    set Context#_locals[parent] = Wrap#_args
    set Context#_parent[parent] = 0

    
    loop
    exitwhen ctx == parent
        set ctx = _step(ctx)
    endloop

    call Context#_free(parent)
    return true
endfunction

function _init takes nothing returns nothing
    call TriggerAddCondition(Wrap#_t1, Condition(function _start_interpreter_wrap))
endfunction
