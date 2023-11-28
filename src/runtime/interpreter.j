// scope Interpreter
// REQUIRES Types Context Ins Table Parser Auto Wrap Convert

// older patches didn't have the %-operator
// we don't use Blizzard.j-ModuloInteger since its behaviour is different
// from the %-operator.
#if PATCH_LVL<129
function _mod128 takes integer _dividend, integer _divisor returns integer
    local integer _modulus = _dividend - (_dividend / _divisor) * _divisor
    if _modulus < 0 then
        if _divisor < 0 then
            set _divisor = - _divisor
        endif
        set _modulus = _modulus + _divisor
    endif
    return _modulus
endfunction
#endif

// _step :: Context -> IO Context
function _step takes integer _ctx returns integer
    local integer _op = Context#_pc[_ctx]
    local integer _t = Ins#_op[_op]
    local integer _tmp
    local integer _fn

    // call Print#_print(" - ins: "+ Ins#_toString(_op))
    
    if _t == Ins#_Set then
        #define macro(ty) Table@_set_##ty(Context@_locals[_ctx], Ins@_a1[_op], Table@_get_##ty(Context@_locals[_ctx], Ins@_a2[_op]))
        #define ty Ins#_type[_op]
        #include "g-type-bin.j"
        #undef ty
        #undef macro
        
    elseif _t == Ins#_SetLocalArray then
        #define macro(ty) Table@_set_##ty(Context@_locals[_ctx], Ins@_a1[_op] + Table@_get_integer(Context@_locals[_ctx], Ins@_a2[_op]), Table@_get_##ty(Context@_locals[_ctx], Ins@_a3[_op]))
        #define ty Ins#_type[_op]
        #include "g-type-bin.j"
        #undef ty
        #undef macro
    
    elseif _t == Ins#_GetLocalArray then
        #define macro(ty) Table@_set_##ty(Context@_locals[_ctx], Ins@_a1[_op], Table@_get_##ty(Context@_locals[_ctx], Ins@_a2[_op] + Table@_get_integer(Context@_locals[_ctx], Ins@_a3[_op])))
        #define ty Ins#_type[_op]
        #include "g-type-bin.j"
        #undef ty
        #undef macro

    elseif _t == Ins#_Lt then
        if Ins#_type[_op] == Types#_integer then
            call Table#_set_boolean(Context#_locals[_ctx], Ins#_a1[_op], Table#_get_integer(Context#_locals[_ctx], Ins#_a2[_op]) < Table#_get_integer(Context#_locals[_ctx], Ins#_a3[_op]))
        else
            call Table#_set_boolean(Context#_locals[_ctx], Ins#_a1[_op], Table#_get_real   (Context#_locals[_ctx], Ins#_a2[_op]) < Table#_get_real   (Context#_locals[_ctx], Ins#_a3[_op]))
        endif
        
    elseif _t == Ins#_Le then
        if Ins#_type[_op] == Types#_integer then
            call Table#_set_boolean(Context#_locals[_ctx], Ins#_a1[_op], Table#_get_integer(Context#_locals[_ctx], Ins#_a2[_op]) <= Table#_get_integer(Context#_locals[_ctx], Ins#_a3[_op]))
        else
            call Table#_set_boolean(Context#_locals[_ctx], Ins#_a1[_op], Table#_get_real   (Context#_locals[_ctx], Ins#_a2[_op]) <= Table#_get_real   (Context#_locals[_ctx], Ins#_a3[_op]))
        endif
        
    elseif _t == Ins#_Gt then
        if Ins#_type[_op] == Types#_integer then
            call Table#_set_boolean(Context#_locals[_ctx], Ins#_a1[_op], Table#_get_integer(Context#_locals[_ctx], Ins#_a2[_op]) > Table#_get_integer(Context#_locals[_ctx], Ins#_a3[_op]))
        else
            call Table#_set_boolean(Context#_locals[_ctx], Ins#_a1[_op], Table#_get_real   (Context#_locals[_ctx], Ins#_a2[_op]) > Table#_get_real   (Context#_locals[_ctx], Ins#_a3[_op]))
        endif
        
    elseif _t == Ins#_Ge then
        if Ins#_type[_op] == Types#_integer then
            call Table#_set_boolean(Context#_locals[_ctx], Ins#_a1[_op], Table#_get_integer(Context#_locals[_ctx], Ins#_a2[_op]) >= Table#_get_integer(Context#_locals[_ctx], Ins#_a3[_op]))
        else
            call Table#_set_boolean(Context#_locals[_ctx], Ins#_a1[_op], Table#_get_real   (Context#_locals[_ctx], Ins#_a2[_op]) >= Table#_get_real   (Context#_locals[_ctx], Ins#_a3[_op]))
        endif
        
    elseif _t == Ins#_Eq then
        #define macro(ty) Table@_set_boolean(Context@_locals[_ctx], Ins@_a1[_op], Table@_get_##ty(Context@_locals[_ctx], Ins@_a2[_op]) == Table@_get_##ty(Context@_locals[_ctx], Ins@_a3[_op]))
        #define ty Ins@_type[_op]
        #include "g-type-bin.j"
        #undef ty
        #undef macro
        
    elseif _t == Ins#_Neq then
        #define macro(ty) Table@_set_boolean(Context@_locals[_ctx], Ins@_a1[_op], Table@_get_##ty(Context@_locals[_ctx], Ins@_a2[_op]) != Table@_get_##ty(Context@_locals[_ctx], Ins@_a3[_op]))
        #define ty Ins@_type[_op]
        #include "g-type-bin.j"
        #undef ty
        #undef macro
        
    elseif _t == Ins#_Add then
        if Ins#_type[_op] == Types#_integer then
            call Table#_set_integer(Context#_locals[_ctx], Ins#_a1[_op], Table#_get_integer(Context#_locals[_ctx], Ins#_a2[_op]) + Table#_get_integer(Context#_locals[_ctx], Ins#_a3[_op]))
        elseif Ins#_type[_op] == Types#_real then
            call Table#_set_real   (Context#_locals[_ctx], Ins#_a1[_op], Table#_get_real   (Context#_locals[_ctx], Ins#_a2[_op]) + Table#_get_real   (Context#_locals[_ctx], Ins#_a3[_op]))
        else
            call Table#_set_string (Context#_locals[_ctx], Ins#_a1[_op], Table#_get_string (Context#_locals[_ctx], Ins#_a2[_op]) + Table#_get_string (Context#_locals[_ctx], Ins#_a3[_op]))
        endif
        
    elseif _t == Ins#_Sub then
        if Ins#_type[_op] == Types#_integer then
            call Table#_set_integer (Context#_locals[_ctx], Ins#_a1[_op], Table#_get_integer(Context#_locals[_ctx], Ins#_a2[_op]) - Table#_get_integer(Context#_locals[_ctx], Ins#_a3[_op]))
        else
            call Table#_set_real    (Context#_locals[_ctx], Ins#_a1[_op], Table#_get_real   (Context#_locals[_ctx], Ins#_a2[_op]) - Table#_get_real   (Context#_locals[_ctx], Ins#_a3[_op]))
        endif
        
    elseif _t == Ins#_Mul then
        if Ins#_type[_op] == Types#_integer then
            call Table#_set_integer (Context#_locals[_ctx], Ins#_a1[_op], Table#_get_integer  (Context#_locals[_ctx], Ins#_a2[_op]) * Table#_get_integer(Context#_locals[_ctx], Ins#_a3[_op]))
        else
            call Table#_set_real    (Context#_locals[_ctx], Ins#_a1[_op], Table#_get_real     (Context#_locals[_ctx], Ins#_a2[_op]) * Table#_get_real   (Context#_locals[_ctx], Ins#_a3[_op]))
        endif
        
    elseif _t == Ins#_Div then
        if Ins#_type[_op] == Types#_integer then
            call Table#_set_integer (Context#_locals[_ctx], Ins#_a1[_op], Table#_get_integer  (Context#_locals[_ctx], Ins#_a2[_op]) / Table#_get_integer(Context#_locals[_ctx], Ins#_a3[_op]))
        else
            call Table#_set_real    (Context#_locals[_ctx], Ins#_a1[_op], Table#_get_real     (Context#_locals[_ctx], Ins#_a2[_op]) / Table#_get_real   (Context#_locals[_ctx], Ins#_a3[_op]))
        endif
    elseif _t == Ins#_Mod then
#if PATCH_LVL<129
        call Table#_set_integer(Context#_locals[_ctx], Ins#_a1[_op], _mod128(Table#_get_integer(Context#_locals[_ctx], Ins#_a2[_op]), Table#_get_integer(Context#_locals[_ctx], Ins#_a3[_op])))
#else
        call Table#_set_integer(Context#_locals[_ctx], Ins#_a1[_op], Table#_get_integer(Context#_locals[_ctx], Ins#_a2[_op]) % Table#_get_integer(Context#_locals[_ctx], Ins#_a3[_op]))
#endif
        
    elseif _t == Ins#_Not then
        call Table#_set_boolean(Context#_locals[_ctx], Ins#_a1[_op], not Table#_get_boolean(Context#_locals[_ctx], Ins#_a2[_op]))
        
    elseif _t == Ins#_Negate then
        if Ins#_type[_op] == Types#_integer then
            call Table#_set_integer (Context#_locals[_ctx], Ins#_a1[_op], -Table#_get_integer (Context#_locals[_ctx], Ins#_a2[_op]))
        else
            call Table#_set_real    (Context#_locals[_ctx], Ins#_a1[_op], -Table#_get_real    (Context#_locals[_ctx], Ins#_a2[_op]))
        endif
        
    elseif _t == Ins#_Label then
        // do nothing
        
    elseif _t == Ins#_Jmp then
        set Context#_pc[_ctx] = Ins#_next[Table#_get_integer(Context#_labels[_ctx], Ins#_a1[_op])]
        return _ctx
        
    elseif _t == Ins#_JmpT then
        if Table#_get_boolean(Context#_locals[_ctx], Ins#_a2[_op]) then
            set Context#_pc[_ctx] = Table#_get_integer(Context#_labels[_ctx], Ins#_a1[_op])
            return _ctx
        endif
        
    elseif _t == Ins#_Ret then
        #define macro(ty) Table@_set_##ty(Context@_locals[Context@_parent[_ctx]], Ins@_a1[Context@_pc[Context@_parent[_ctx]]], Table@_get_##ty(Context@_locals[_ctx], 0))
        #define ty Ins@_type[_op]
        #include "g-type-bin.j"
        #undef ty
        #undef macro
        
        set _tmp = _ctx
        set _ctx = Context#_parent[_ctx]
        call Table#_destroy(Context#_bindings[_tmp])
        call Context#_free(_tmp)
        
    elseif _t == Ins#_Call then
        set _fn = Ins#_a2[_op]
        if _fn < 0 or Modified@_modified(_fn) then
            // newly defined or reloaded function
            
            set _tmp = Context#_alloc()
            
            set Context#_parent[_tmp]   = _ctx
            set Context#_pc[_tmp]       = Parser#_fn_entry[_fn + 100]
            set Context#_labels[_tmp]   = Parser#_fn_labels[_fn + 100]
            set Context#_locals[_tmp]   = Context#_bindings[_ctx]
            set Context#_bindings[_tmp] = Table#_alloc()
            
            return _tmp
        elseif _fn > 0 then
            // auto generated call for pre-defined functions
            call Auto#_call_predefined(Ins#_a1[_op], _fn, _ctx)
            
        endif

    elseif _t == Ins#_Bind then // should be same as Set except different target table
        #define macro(ty) Table@_set_##ty(Context@_bindings[_ctx], Ins@_a1[_op], Table@_get_##ty(Context@_locals[_ctx], Ins@_a2[_op]))
        #define ty Ins#_type[_op]
        #include "g-type-bin.j"
        #undef ty
        #undef macro
 
    elseif _t == Ins#_SetGlobal then
        if Ins#_a1[_op] > 0 then
            #define macro(ty) Auto@_set_global_##ty(Ins@_a1[_op], Table@_get_##ty(Context@_locals[_ctx], Ins@_a2[_op]))
            #define ty Ins#_type[_op]
            #include "g-type-bin.j"
            #undef ty
            #undef macro
        else
            #define macro(ty) Table@_set_##ty(Modified@_globals, Ins@_a1[_op], Table@_get_##ty(Context@_locals[_ctx], Ins@_a2[_op]))
            #define ty Ins#_type[_op]
            #include "g-type-bin.j"
            #undef ty
            #undef macro
        endif

    elseif _t == Ins#_GetGlobal then
        if Ins#_a2[_op] > 0 then
            #define macro(ty) Table@_set_##ty(Context@_locals[_ctx], Ins@_a1[_op], Auto@_get_global_##ty(Ins@_a2[_op]))
            #define ty Ins#_type[_op]
            #include "g-type-bin.j"
            #undef macro
        else
            #define macro(ty) Table@_set_##ty(Context@_locals[_ctx], Ins@_a1[_op], Table@_get_##ty(Modified@_globals, Ins@_a2[_op]))
            #define ty Ins#_type[_op]
            #include "g-type-bin.j"
            #undef ty
            #undef macro
        endif

    elseif _t == Ins#_SetGlobalArray then
        if Ins#_a1[_op] > 0 then
            #define macro(ty) Auto@_array_set_global_##ty(Ins@_a1[_op], Table@_get_integer(Context@_locals[_ctx], Ins@_a2[_op]), Table@_get_##ty(Context@_locals[_ctx], Ins@_a3[_op]))
            #define ty Ins#_type[_op]
            #include "g-type-bin.j"
            #undef ty
            #undef macro
        else
            #define macro(ty) Table@_set_##ty(Modified@_globals, Ins@_a1[_op] - Table@_get_integer(Context@_locals[_ctx], Ins@_a2[_op]), Table@_get_##ty(Context@_locals[_ctx], Ins@_a3[_op]))
            #define ty Ins#_type[_op]
            #include "g-type-bin.j"
            #undef ty
            #undef macro
        endif

    elseif _t == Ins#_GetGlobalArray then
        if Ins#_a2[_op] > 0 then
            #define macro(ty) Table@_set_##ty(Context@_locals[_ctx], Ins@_a1[_op], Auto@_array_get_global_##ty(Ins@_a2[_op], Table@_get_integer(Context@_locals[_ctx], Ins@_a3[_op])))
            #define ty Ins#_type[_op]
            #include "g-type-bin.j"
            #undef ty
            #undef macro
        else
            #define macro(ty) Table@_set_##ty(Context@_locals[_ctx], Ins@_a1[_op], Table@_get_##ty(Modified@_globals, Ins@_a2[_op] - Table@_get_integer(Context@_locals[_ctx], Ins@_a3[_op])))
            #define ty Ins#_type[_op]
            #include "g-type-bin.j"
            #undef ty
            #undef macro
        endif

    elseif _t == Ins#_Lit then
        if Ins#_type[_op] == Types#_integer then
            call Table#_set_integer(Context#_locals[_ctx], Ins#_a1[_op], Ins#_integer[_op])
            
        elseif Ins#_type[_op] == Types#_real then
            call Table#_set_real(Context#_locals[_ctx], Ins#_a1[_op], Ins#_real[_op])
            
        elseif Ins#_type[_op] == Types#_string then
            call Table#_set_string(Context#_locals[_ctx], Ins#_a1[_op], Ins#_string[_op])
        
        elseif Ins#_type[_op] == Types#_boolean then
            call Table#_set_boolean(Context#_locals[_ctx], Ins#_a1[_op], Ins#_boolean[_op])
            
        elseif Ins#_type[_op] == Types#_code then
            call Table#_set_integer(Context#_locals[_ctx], Ins#_a1[_op], Ins#_integer[_op])
        else // null
            // dont like how we use tables global, but whatev
            call RemoveSavedHandle(Table#_ht, Context#_locals[_ctx], Ins#_a1[_op])
        endif
    elseif _t == Ins#_Convert then
        call Convert#_convert(Ins#_type[_op], Ins#_a1[_op], Ins#_a2[_op], Ins#_a3[_op], _ctx)
    endif
    
    set Context#_pc[_ctx] = Ins#_next[Context#_pc[_ctx]]
    return _ctx
endfunction


// This function is called via Wrap#_call_anything_around
// which is called from two "places", that is if the interpreter detects
// a reloaded files or from a dummy function.

// We return a boolean anyways as it is good form for boolexpr's (which this
// is used as, due to cycles), so in the past we always returned true.

// But to be able to use dummy functions as boolexprs // TODO test boolexprs
// we have to somehow get the return value of the function, which of course being
// called from a dummy function is entirely interpreted. To do that we simply
// return the boolean value of the return table (slot 0 in locals).

// Now in case of a function not returning boolean or not being a dummy function
// this does no harm, as we handle the return code in every other case in
// the interpreter *and* we don't do any conditional stuff based on the return
// value of Wrap#_call_anything_around. We just returned a value before because
// it was good taste.

// And this whole spiel is only really needed for the exact case of using a
// dummy function as a boolexpr, because that is a unique boundary between
// basically the call instruction and the semantic of basically `TriggerEvaluate`.
function _start_interpreter_wrap takes nothing returns boolean
    local integer _ctx = Context#_alloc()
    local integer _parent = Context#_alloc()

    set Context#_pc[_ctx]       = Parser#_fn_entry[Wrap#_p + 100]
    set Context#_parent[_ctx]   = _parent
    set Context#_labels[_ctx]   = Parser#_fn_labels[Wrap#_p + 100]
    set Context#_locals[_ctx]   = Wrap#_args
    set Context#_bindings[_ctx] = Table#_alloc()
    
    set Context#_pc[_parent]     = 0
    set Context#_labels[_parent] = 0
    set Context#_locals[_parent] = Wrap#_args
    set Context#_parent[_parent] = 0

    
    loop
    exitwhen _ctx == _parent
        set _ctx = _step(_ctx)
    endloop

    call Context#_free(_parent)
    return Table#_get_boolean(Wrap#_args, 0)
endfunction

function _exec_globals takes integer _g returns nothing
    local integer _ctx = Context#_alloc()
    set Context#_pc[_ctx]       = _g
    set Context#_labels[_ctx]   = 0
    set Context#_locals[_ctx]   = Table#_alloc()
    set Context#_bindings[_ctx] = Table#_alloc()
    set Context#_parent[_ctx]   = 0
    loop
    exitwhen Context#_pc[_ctx] == 0
        set _ctx = _step(_ctx)
    endloop
    call Context#_destroy(_ctx)
    call Ins#_destroy(_g)
endfunction

function _init takes nothing returns nothing
    call TriggerAddCondition(Wrap#_t1, Condition(function _start_interpreter_wrap))
endfunction
