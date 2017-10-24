

globals

    constant integer _type_Integer = 0
    constant integer _type_Real = 0
    constant integer _type_Code = 0
    constant integer _type_String = 0
    constant integer _type_Boolean = 0

    constant integer _NOT = 0
    constant integer _Neq = 0
    constant integer _JmpT = 0
    constant integer _Jmp = 0
    constant integer _Literal = 0
    constant integer _Bind = 0
    constant integer _Set = 0
    constant integer _Call = 0
    constant integer _Add = 0
    constant integer _Mul = 0
    constant integer _Div = 0
    constant integer _Sub = 0
    constant integer _Negate = 0
    constant integer _SetArr = 0
    constant integer _GetArr = 0
    constant integer _SetGlobalArr = 0
    constant integer _GetGlobalArr = 0
    constant integer _SetGlobal = 0
    constant integer _GetGlobal = 0
    constant integer _Ret = 0
    constant integer _Label = 0
    constant integer _Eq = 0
    constant integer _Lt = 0
    constant integer _Le = 0
    constant integer _Ge = 0
    constant integer _Gt = 0




    //  

    integer _fresh

    // struct Instruction
    integer array _ins_next
    integer array _ins_op
    integer array _ins_type
    boolean array _ins_lit

    integer array _ins_a1
    integer array _ins_a2
    integer array _ins_a3

    real array _ins_real_lit
    boolean array _ins_bool_lit
    string array _ins_string_lit

    // struct Context
    integer array _ctx_locals
    integer array _ctx_labels
    integer array _ctx_parent

    integer array _ctx_pc


endglobals

function _context_alloc takes nothing returns integer
    return 0
endfunction

function interp takes integer ctx returns integer
    local integer op = _ctx_pc[ctx]
    local integer t = _ins_op[op]
    local boolean l = _ins_lit[op]
    local integer tmp
    // TODO: binsearch
    if t == _Set then
    
        // TODO: binsearch
        if _ins_type[op] == _type_Integer then
            call _set_integer(_ctx_locals[ctx], _ins_a1[op], _get_integer(_ctx_locals[ctx], _ins_a2[op]))
        elseif _ins_type[op] == _type_Real then
            call _set_real(_ctx_locals[ctx], _ins_a1[op], _get_real(_ctx_locals[ctx], _ins_a2[op]))
        elseif _ins_type[op] == _type_Boolean then
            call _set_boolean(_ctx_locals[ctx], _ins_a1[op], _get_boolean(_ctx_locals[ctx], _ins_a2[op]))
        elseif _ins_type[op] == _type_String then
            call _set_string(_ctx_locals[ctx], _ins_a1[op], _get_string(_ctx_locals[ctx], _ins_a2[op]))
        elseif _ins_type[op] == _type_Code then
            call _set_integer(_ctx_locals[ctx], _ins_a1[op], _get_integer(_ctx_locals[ctx], _ins_a2[op]))
            
            // remember: handle literal can only be null
        endif
        
    elseif t == _SetArr then
    elseif t == _GetArr then
    
    elseif t == _Lt then
        if _ins_type[op] == _type_Integer then
            call _set_boolean(_ctx_locals[ctx], _ins_a1[op], _get_integer(_ctx_locals[ctx], _ins_a2[op]) < _get_integer(_ctx_locals[ctx], _ins_a2[op]))
        else
            call _set_boolean(_ctx_locals[ctx], _ins_a1[op], _get_real(_ctx_locals[ctx], _ins_a2[op]) < _get_real(_ctx_locals[ctx], _ins_a3[op]))
        endif
        
    elseif t == _Le then
        if _ins_type[op] == _type_Integer then
            call _set_boolean(_ctx_locals[ctx], _ins_a1[op], _get_integer(_ctx_locals[ctx], _ins_a2[op]) <= _get_integer(_ctx_locals[ctx], _ins_a3[op]))
        else
            call _set_boolean(_ctx_locals[ctx], _ins_a1[op], _get_real(_ctx_locals[ctx], _ins_a2[op]) <= _get_real(_ctx_locals[ctx], _ins_a3[op]))
        endif
        
    elseif t == _Gt then
        if _ins_type[op] == _type_Integer then
            call _set_boolean(_ctx_locals[ctx], _ins_a1[op], _get_integer(_ctx_locals[ctx], _ins_a2[op]) > _get_integer(_ctx_locals[ctx], _ins_a3[op]))
        else
            call _set_boolean(_ctx_locals[ctx], _ins_a1[op], _get_real(_ctx_locals[ctx], _ins_a2[op]) > _get_real(_ctx_locals[ctx], _ins_a3[op]))
        endif
        
    elseif t == _Ge then
        if _ins_type[op] == _type_Integer then
            call _set_boolean(_ctx_locals[ctx], _ins_a1[op], _get_integer(_ctx_locals[ctx], _ins_a2[op]) >= _get_integer(_ctx_locals[ctx], _ins_a3[op]))
        else
            call _set_boolean(_ctx_locals[ctx], _ins_a1[op], _get_real(_ctx_locals[ctx], _ins_a2[op]) >= _get_real(_ctx_locals[ctx], _ins_a3[op]))
        endif
        
    elseif t == _Eq then
        if _ins_type[op] == _type_Integer then
            call _set_boolean(_ctx_locals[ctx], _ins_a1[op], _get_integer(_ctx_locals[ctx], _ins_a2[op]) == _get_integer(_ctx_locals[ctx], _ins_a3[op]))
        elseif _ins_type[op] == _type_Real then
            call _set_boolean(_ctx_locals[ctx], _ins_a1[op], _get_real(_ctx_locals[ctx], _ins_a2[op]) == _get_real(_ctx_locals[ctx], _ins_a3[op]))
        // ...
        else
            // handle derived type (except the other few types)
            call _set_boolean(_ctx_locals[ctx], _ins_a1[op], _get_fogstate(_ctx_locals[ctx], _ins_a2[op]) == _get_fogstate(_ctx_locals[ctx], _ins_a3[op]))
        endif
        
    elseif t == _Neq then
        if _ins_type[op] == _type_Integer then
            call _set_boolean(_ctx_locals[ctx], _ins_a1[op], _get_integer(_ctx_locals[ctx], _ins_a2[op]) != _get_integer(_ctx_locals[ctx], _ins_a3[op]))
        elseif _ins_type[op] == _type_Real then
            call _set_boolean(_ctx_locals[ctx], _ins_a1[op], _get_real(_ctx_locals[ctx], _ins_a2[op]) != _get_real(_ctx_locals[ctx], _ins_a3[op]))
                // ...
        else
            // handle derived type (except the other few types)
            call _set_boolean(_ctx_locals[ctx], _ins_a1[op], _get_fogstate(_ctx_locals[ctx], _ins_a2[op]) != _get_fogstate(_ctx_locals[ctx], _ins_a3[op]))
        endif
        
    elseif t == _Add then
        if _ins_type[op] == _type_Integer then
            call _set_integer(_ctx_locals[ctx], _ins_a1[op], _get_integer(_ctx_locals[ctx], _ins_a2[op]) + _get_integer(_ctx_locals[ctx], _ins_a3[op]))
        else
            call _set_real(_ctx_locals[ctx], _ins_a1[op], _get_real(_ctx_locals[ctx], _ins_a2[op]) + _get_real(_ctx_locals[ctx], _ins_a3[op]))
        endif
        
    elseif t == _Sub then
        if _ins_type[op] == _type_Integer then
            call _set_integer(_ctx_locals[ctx], _ins_a1[op], _get_integer(_ctx_locals[ctx], _ins_a2[op]) - _get_integer(_ctx_locals[ctx], _ins_a3[op]))
        else
            call _set_real(_ctx_locals[ctx], _ins_a1[op], _get_real(_ctx_locals[ctx], _ins_a2[op]) - _get_real(_ctx_locals[ctx], _ins_a3[op]))
        endif
        
    elseif t == _Mul then
        if _ins_type[op] == _type_Integer then
            call _set_integer(_ctx_locals[ctx], _ins_a1[op], _get_integer(_ctx_locals[ctx], _ins_a2[op]) * _get_integer(_ctx_locals[ctx], _ins_a3[op]))
        else
            call _set_real(_ctx_locals[ctx], _ins_a1[op], _get_real(_ctx_locals[ctx], _ins_a2[op]) * _get_real(_ctx_locals[ctx], _ins_a3[op]))
        endif
        
    elseif t == _Div then
        if _ins_type[op] == _type_Integer then
            call _set_integer(_ctx_locals[ctx], _ins_a1[op], _get_integer(_ctx_locals[ctx], _ins_a2[op]) / _get_integer(_ctx_locals[ctx], _ins_a3[op]))
        else
            call _set_real(_ctx_locals[ctx], _ins_a1[op], _get_real(_ctx_locals[ctx], _ins_a2[op]) / _get_real(_ctx_locals[ctx], _ins_a3[op]))
        endif
        
    elseif t == _NOT then
        call _set_boolean(_ctx_locals[ctx], _ins_a1[op], not _get_boolean(_ctx_locals[ctx], _ins_a2[op]))
        
    elseif t == _Label then
        // do nothing
        
    elseif t == _Jmp then
        set _ctx_pc[ctx] = _ins_next[_get_integer(_ctx_labels[ctx], _ins_a1[op])]
        return ctx
        
    elseif t == _JmpT then
        if _get_boolean(_ctx_locals[ctx], _ins_a1[op]) then
            set _ctx_pc[ctx] = _ins_next[_get_integer(_ctx_labels[ctx], _ins_a1[op])]
            return ctx
        endif
        
    elseif t == _Ret then
        if _ins_type[_ctx_pc[_ctx_parent[ctx]]] == _type_Integer then
            call _set_integer(_ctx_locals[_ctx_parent[ctx]], _ins_a1[_ctx_pc[_ctx_parent[ctx]]], _get_integer(_ctx_locals[ctx], 0))

        // etc.
        endif

        //call ctx.tbl.clean()
        //call ctx.destroy()
        return _ins_next[_ctx_parent[ctx]]
        
    elseif t == _Call then
        if _ins_a2[op] > 0 then
            // auto generated call for natives/BJ-functions
        else
            // user-defined function
            set _ctx_parent[_fresh] = ctx
            set _ctx_pc[_fresh] = 0 //calls[_ins_a2[op]]
            set tmp = _fresh
            set _fresh = _context_alloc()
            return tmp
        endif

    elseif t == _Bind then // should be same as Set except different target table
        if _ins_type[op] == _type_Integer then
            call _set_integer(_ctx_locals[_fresh], _ins_a1[op], _get_integer(_ctx_locals[ctx], _ins_a2[op]))
        elseif _ins_type[op] == _type_Real then
            call _set_real(_ctx_locals[_fresh], _ins_a1[op], _get_real(_ctx_locals[ctx], _ins_a2[op]))
        // ...
        endif
    elseif t == _SetGlobal then
    elseif t == _GetGlobal then
    elseif t == _SetGlobalArr then
    elseif t == _GetGlobalArr then
    endif
    
    set _ctx_pc[ctx] = _ins_next[_ctx_pc[ctx]]
    return ctx
endfunction
