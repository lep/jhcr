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
    // TODO: binsearch
    if t == Ins#_Set then
    
        // TODO: binsearch
        if Ins#_type[op] == Ins#_type_Integer then
            call Table#_set_integer(Context#_locals[ctx], Ins#_a1[op], Table#_get_integer(Context#_locals[ctx], Ins#_a2[op]))
        elseif Ins#_type[op] == Ins#_type_Real then
            call Table#_set_real(Context#_locals[ctx], Ins#_a1[op], Table#_get_real(Context#_locals[ctx], Ins#_a2[op]))
        elseif Ins#_type[op] == Ins#_type_Boolean then
            call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op], Table#_get_boolean(Context#_locals[ctx], Ins#_a2[op]))
        elseif Ins#_type[op] == Ins#_type_String then
            call Table#_set_string(Context#_locals[ctx], Ins#_a1[op], Table#_get_string(Context#_locals[ctx], Ins#_a2[op]))
            
        elseif Ins#_type[op] == Ins#_type_Code then
            call Table#_set_integer(Context#_locals[ctx], Ins#_a1[op], Table#_get_integer(Context#_locals[ctx], Ins#_a2[op]))
        endif
        
    elseif t == Ins#_SetLocalArray then
        if Ins#_type[op] == Ins#_type_Integer then
            call Table#_set_integer(Context#_locals[ctx], Ins#_a1[op] + Ins#_a2[op], Table#_get_integer(Context#_locals[ctx], Ins#_a3[op]))
            
        elseif Ins#_type[op] == Ins#_type_Real then
            call Table#_set_real(Context#_locals[ctx], Ins#_a1[op] + Ins#_a2[op], Table#_get_real(Context#_locals[ctx], Ins#_a3[op]))
            
        elseif Ins#_type[op] == Ins#_type_String then
            call Table#_set_string(Context#_locals[ctx], Ins#_a1[op] + Ins#_a2[op], Table#_get_string(Context#_locals[ctx], Ins#_a3[op]))
            
        elseif Ins#_type[op] == Ins#_type_Boolean then
            call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op] + Ins#_a2[op], Table#_get_boolean(Context#_locals[ctx], Ins#_a3[op]))
            
        elseif Ins#_type[op] == Ins#_type_Code then
            call Table#_set_integer(Context#_locals[ctx], Ins#_a1[op] + Ins#_a2[op], Table#_get_integer(Context#_locals[ctx], Ins#_a3[op]))
        endif
        
    elseif t == Ins#_GetLocalArray then
        if Ins#_type[op] == Ins#_type_Integer then
            call Table#_set_integer(Context#_locals[ctx], Ins#_a1[op], Table#_get_integer(Context#_locals[ctx], Ins#_a2[op] + Ins#_a3[op]))
        
        elseif Ins#_type[op] == Ins#_type_Real then
            call Table#_set_real(Context#_locals[ctx], Ins#_a1[op], Table#_get_real(Context#_locals[ctx], Ins#_a2[op] + Ins#_a3[op]))
            
        elseif Ins#_type[op] == Ins#_type_String then
            call Table#_set_string(Context#_locals[ctx], Ins#_a1[op], Table#_get_string(Context#_locals[ctx], Ins#_a2[op] + Ins#_a3[op]))
            
        elseif Ins#_type[op] == Ins#_type_Boolean then
            call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op], Table#_get_boolean(Context#_locals[ctx], Ins#_a2[op] + Ins#_a3[op]))
            
        elseif Ins#_type[op] == Ins#_type_Code then
            call Table#_set_integer(Context#_locals[ctx], Ins#_a1[op], Table#_get_integer(Context#_locals[ctx], Ins#_a2[op] + Ins#_a3[op]))
        endif
        
    elseif t == Ins#_Lt then
        if Ins#_type[op] == Ins#_type_Integer then
            call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op], Table#_get_integer(Context#_locals[ctx], Ins#_a2[op]) < Table#_get_integer(Context#_locals[ctx], Ins#_a2[op]))
        else
            call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op], Table#_get_real(Context#_locals[ctx], Ins#_a2[op]) < Table#_get_real(Context#_locals[ctx], Ins#_a3[op]))
        endif
        
    elseif t == Ins#_Le then
        if Ins#_type[op] == Ins#_type_Integer then
            call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op], Table#_get_integer(Context#_locals[ctx], Ins#_a2[op]) <= Table#_get_integer(Context#_locals[ctx], Ins#_a3[op]))
        else
            call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op], Table#_get_real(Context#_locals[ctx], Ins#_a2[op]) <= Table#_get_real(Context#_locals[ctx], Ins#_a3[op]))
        endif
        
    elseif t == Ins#_Gt then
        if Ins#_type[op] == Ins#_type_Integer then
            call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op], Table#_get_integer(Context#_locals[ctx], Ins#_a2[op]) > Table#_get_integer(Context#_locals[ctx], Ins#_a3[op]))
        else
            call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op], Table#_get_real(Context#_locals[ctx], Ins#_a2[op]) > Table#_get_real(Context#_locals[ctx], Ins#_a3[op]))
        endif
        
    elseif t == Ins#_Ge then
        if Ins#_type[op] == Ins#_type_Integer then
            call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op], Table#_get_integer(Context#_locals[ctx], Ins#_a2[op]) >= Table#_get_integer(Context#_locals[ctx], Ins#_a3[op]))
        else
            call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op], Table#_get_real(Context#_locals[ctx], Ins#_a2[op]) >= Table#_get_real(Context#_locals[ctx], Ins#_a3[op]))
        endif
        
    elseif t == Ins#_Eq then
        if Ins#_type[op] == Ins#_type_Integer then
            call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op], Table#_get_integer(Context#_locals[ctx], Ins#_a2[op]) == Table#_get_integer(Context#_locals[ctx], Ins#_a3[op]))
            
        elseif Ins#_type[op] == Ins#_type_Real then
            call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op], Table#_get_real(Context#_locals[ctx], Ins#_a2[op]) == Table#_get_real(Context#_locals[ctx], Ins#_a3[op]))
        
        elseif Ins#_type[op] == Ins#_type_String then
            call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op], Table#_get_string(Context#_locals[ctx], Ins#_a2[op]) == Table#_get_string(Context#_locals[ctx], Ins#_a3[op]))
            
        elseif Ins#_type[op] == Ins#_type_Boolean then
            call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op], Table#_get_boolean(Context#_locals[ctx], Ins#_a2[op]) == Table#_get_boolean(Context#_locals[ctx], Ins#_a3[op]))

        else
            // handle derived type (except the other few types)
            call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op], Table#_get_fogstate(Context#_locals[ctx], Ins#_a2[op]) == Table#_get_fogstate(Context#_locals[ctx], Ins#_a3[op]))
        endif
        
    elseif t == Ins#_Neq then
        if Ins#_type[op] == Ins#_type_Integer then
            call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op], Table#_get_integer(Context#_locals[ctx], Ins#_a2[op]) != Table#_get_integer(Context#_locals[ctx], Ins#_a3[op]))
            
        elseif Ins#_type[op] == Ins#_type_Real then
            call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op], Table#_get_real(Context#_locals[ctx], Ins#_a2[op]) != Table#_get_real(Context#_locals[ctx], Ins#_a3[op]))
        
        elseif Ins#_type[op] == Ins#_type_String then
            call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op], Table#_get_string(Context#_locals[ctx], Ins#_a2[op]) != Table#_get_string(Context#_locals[ctx], Ins#_a3[op]))
            
        elseif Ins#_type[op] == Ins#_type_Boolean then
            call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op], Table#_get_boolean(Context#_locals[ctx], Ins#_a2[op]) != Table#_get_boolean(Context#_locals[ctx], Ins#_a3[op]))

        else
            // handle derived type (except the other few types)
            call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op], Table#_get_fogstate(Context#_locals[ctx], Ins#_a2[op]) != Table#_get_fogstate(Context#_locals[ctx], Ins#_a3[op]))
        endif
        
    elseif t == Ins#_Add then
        if Ins#_type[op] == Ins#_type_Integer then
            call Table#_set_integer(Context#_locals[ctx], Ins#_a1[op], Table#_get_integer(Context#_locals[ctx], Ins#_a2[op]) + Table#_get_integer(Context#_locals[ctx], Ins#_a3[op]))
        else
            call Table#_set_real(Context#_locals[ctx], Ins#_a1[op], Table#_get_real(Context#_locals[ctx], Ins#_a2[op]) + Table#_get_real(Context#_locals[ctx], Ins#_a3[op]))
        endif
        
    elseif t == Ins#_Sub then
        if Ins#_type[op] == Ins#_type_Integer then
            call Table#_set_integer(Context#_locals[ctx], Ins#_a1[op], Table#_get_integer(Context#_locals[ctx], Ins#_a2[op]) - Table#_get_integer(Context#_locals[ctx], Ins#_a3[op]))
        else
            call Table#_set_real(Context#_locals[ctx], Ins#_a1[op], Table#_get_real(Context#_locals[ctx], Ins#_a2[op]) - Table#_get_real(Context#_locals[ctx], Ins#_a3[op]))
        endif
        
    elseif t == Ins#_Mul then
        if Ins#_type[op] == Ins#_type_Integer then
            call Table#_set_integer(Context#_locals[ctx], Ins#_a1[op], Table#_get_integer(Context#_locals[ctx], Ins#_a2[op]) * Table#_get_integer(Context#_locals[ctx], Ins#_a3[op]))
        else
            call Table#_set_real(Context#_locals[ctx], Ins#_a1[op], Table#_get_real(Context#_locals[ctx], Ins#_a2[op]) * Table#_get_real(Context#_locals[ctx], Ins#_a3[op]))
        endif
        
    elseif t == Ins#_Div then
        if Ins#_type[op] == Ins#_type_Integer then
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
            set Context#_pc[ctx] = Ins#_next[Table#_get_integer(Context#_labels[ctx], Ins#_a1[op])]
            return ctx
        endif
        
    elseif t == Ins#_Ret then
        //call ctx.tbl.clean()
        //call ctx.destroy()
        if Ins#_type[Context#_pc[Context#_parent[ctx]]] == Ins#_type_Integer then
            call Table#_set_integer(Context#_locals[Context#_parent[ctx]], Ins#_a1[Context#_pc[Context#_parent[ctx]]], Table#_get_integer(Context#_locals[ctx], 0))

        // etc.
        endif
        return Ins#_next[Context#_parent[ctx]]
        
    elseif t == Ins#_Call then
        //set tmp = Names#_get_function(Ins#_literal[op])
        set tmp = Ins#_a2[op]
        if tmp < 0 or Modified#_modified(tmp) then
            // user-defined or reloaded function
            //set fn = Names#_get_function(Ins#_literal[op])
            set fn = tmp
            set Context#_parent[_fresh] = ctx
            set Context#_pc[_fresh] = Parser#_fn_entry[fn + 100]
            set Context#_labels[_fresh] = Table#_get_integer(Parser#_fn_labels, fn)
            
            set tmp = _fresh
            set _fresh = Context#_alloc()
            set Scopes#_binding = _fresh
            set Scopes#_scope = tmp
            return tmp
        elseif tmp > 0 then
            // auto generated call for pre-defined functions
            call Auto#_call_predefined(Ins#_a1[op], tmp)
            
        endif

    elseif t == Ins#_Bind then // should be same as Set except different target table
        if Ins#_type[op] == Ins#_type_Integer then
            call Table#_set_integer(Context#_locals[_fresh], Ins#_a1[op], Table#_get_integer(Context#_locals[ctx], Ins#_a2[op]))
        elseif Ins#_type[op] == Ins#_type_Real then
            call Table#_set_real(Context#_locals[_fresh], Ins#_a1[op], Table#_get_real(Context#_locals[ctx], Ins#_a2[op]))
        // ...
        endif
    elseif t == Ins#_SetGlobal then
        if Ins#_type[op] == Ins#_type_Integer then
            //call Auto#_set_global_integer(Names#_get_global(Ins#_type[op], Ins#_literal[op]), Ins#_a1[op])
            call Auto#_set_global_integer(Ins#_a1[op], Ins#_a2[op])
        endif
    elseif t == Ins#_GetGlobal then
        if Ins#_type[op] == Ins#_type_Integer then
            //call Table#_set_integer(Context#_locals[ctx], Ins#_a1[op], Auto#_get_global_integer(Names#_get_global(Ins#_type[op], Ins#_literal[op])))
            call Table#_set_integer(Context#_locals[ctx], Ins#_a1[op], Auto#_get_global_integer(Ins#_a2[op]))
        endif
    elseif t == Ins#_SetGlobalArray then
        if Ins#_type[op] == Ins#_type_Integer then
            //call Auto#_array_set_global_integer(Names#_get_global(Ins#_type[op], Ins#_literal[op]), Ins#_a1[op], Ins#_a2[op])
            call Auto#_array_set_global_integer(Ins#_a1[op], Ins#_a2[op], Ins#_a3[op])
        endif
    elseif t == Ins#_GetGlobalArray then
        if Ins#_type[op] == Ins#_type_Integer then
            //call Table#_set_integer(Context#_locals[ctx], Ins#_a1[op], Auto#_array_get_global_integer(Names#_get_global(Ins#_type[op], Ins#_literal[op]), Ins#_a2[op]))
            call Table#_set_integer(Context#_locals[ctx], Ins#_a1[op], Auto#_array_get_global_integer(Ins#_a2[op], Ins#_a3[op]))
        endif
    elseif t == Ins#_Lit then
        if Ins#_type[op] == Ins#_type_Integer then
            call Table#_set_integer(Context#_locals[ctx], Ins#_a1[op], Ins#_integer[op])
            
        elseif Ins#_type[op] == Ins#_type_Real then
            call Table#_set_real(Context#_locals[ctx], Ins#_a1[op], Ins#_real[op])
            
        elseif Ins#_type[op] == Ins#_type_String then
            call Table#_set_string(Context#_locals[ctx], Ins#_a1[op], Ins#_string[op])
        
        elseif Ins#_type[op] == Ins#_type_Boolean then
            call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op], Ins#_boolean[op])
            
        elseif Ins#_type[op] == Ins#_type_Code then
            call Table#_set_integer(Context#_locals[ctx], Ins#_a1[op], Ins#_integer[op])
        else // null // TODO?
            
        endif
    elseif t == Ins#_Convert then
        call Convert#_convert(Ins#_type[op], Ins#_a1[op], Ins#_a2[op], Ins#_a3[op])
    endif
    
    set Context#_pc[ctx] = Ins#_next[Context#_pc[ctx]]
    return ctx
endfunction


function _start_interpreter takes integer fn returns nothing
    local integer ctx = Context#_alloc()
    set Context#_pc[ctx]     = Parser#_fn_entry[fn + 100]
    set Context#_parent[ctx] = 0
    set Context#_labels[ctx] = Table#_get_integer(Parser#_fn_labels, fn)
    set Context#_locals[ctx] = _fresh
    set _fresh = Context#_alloc()
    set Scopes#_binding = _fresh
    
    loop
    exitwhen ctx == 0
        set ctx = _step(ctx)
    endloop
endfunction

function _start_interpreter_wrap takes nothing returns boolean
    local integer ctx = Context#_alloc()
    set Context#_pc[ctx]     = Parser#_fn_entry[Wrap#_p + 100]
    set Context#_parent[ctx] = 0
    set Context#_labels[ctx] = Table#_get_integer(Parser#_fn_labels, Wrap#_p)
    set Context#_locals[ctx] = Scopes#_scope
    
    loop
    exitwhen ctx == 0
        set ctx = _step(ctx)
    endloop
    
    return true
endfunction

function _init takes nothing returns nothing
    set _fresh = Context#_alloc()
    call TriggerAddCondition(Wrap#_t, Condition(function _start_interpreter_wrap))
endfunction
