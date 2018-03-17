// scope Interpreter

globals
    integer _fresh = 0
endglobals

// interp :: Context -> IO Context
function interp takes integer ctx returns integer
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
        
    elseif t == Ins#_SetArray then
    elseif t == Ins#_GetArray then
    
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
        // ...
        else
            // handle derived type (except the other few types)
            call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op], Table#_get_fogstate(Context#_locals[ctx], Ins#_a2[op]) == Table#_get_fogstate(Context#_locals[ctx], Ins#_a3[op]))
        endif
        
    elseif t == Ins#_Neq then
        if Ins#_type[op] == Ins#_type_Integer then
            call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op], Table#_get_integer(Context#_locals[ctx], Ins#_a2[op]) != Table#_get_integer(Context#_locals[ctx], Ins#_a3[op]))
        elseif Ins#_type[op] == Ins#_type_Real then
            call Table#_set_boolean(Context#_locals[ctx], Ins#_a1[op], Table#_get_real(Context#_locals[ctx], Ins#_a2[op]) != Table#_get_real(Context#_locals[ctx], Ins#_a3[op]))
                // ...
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
        if Ins#_type[Context#_pc[Context#_parent[ctx]]] == Ins#_type_Integer then
            call Table#_set_integer(Context#_locals[Context#_parent[ctx]], Ins#_a1[Context#_pc[Context#_parent[ctx]]], Table#_get_integer(Context#_locals[ctx], 0))

        // etc.
        endif

        //call ctx.tbl.clean()
        //call ctx.destroy()
        return Ins#_next[Context#_parent[ctx]]
        
    elseif t == Ins#_Call then
        if Ins#_a2[op] < 0 then
            // auto generated call for natives/BJ-functions
        else
            // user-defined function
            set fn = Names#_get_function(Ins#_literal[op])
            set Context#_parent[_fresh] = ctx
            set Context#_pc[_fresh] = Parser#_fn_entry[fn]
            set Context#_labels[_fresh] = Parser#_fn_labels[fn]
            
            set tmp = _fresh
            set _fresh = Context#_alloc()
            return tmp
        endif

    elseif t == Ins#_Bind then // should be same as Set except different target table
        if Ins#_type[op] == Ins#_type_Integer then
            call Table#_set_integer(Context#_locals[_fresh], Ins#_a1[op], Table#_get_integer(Context#_locals[ctx], Ins#_a2[op]))
        elseif Ins#_type[op] == Ins#_type_Real then
            call Table#_set_real(Context#_locals[_fresh], Ins#_a1[op], Table#_get_real(Context#_locals[ctx], Ins#_a2[op]))
        // ...
        endif
    elseif t == Ins#_SetGlobal then
    elseif t == Ins#_GetGlobal then
    elseif t == Ins#_SetGlobalArray then
    elseif t == Ins#_GetGlobalArray then
    elseif t == Ins#_Convert then
        call Convert#_convert(Ins#_type[op], Ins#_a1[op], Ins#_a2[op], Ins#_a3[op])
    endif
    
    set Context#_pc[ctx] = Ins#_next[Context#_pc[ctx]]
    return ctx
endfunction
