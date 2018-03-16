// scope Ins

globals
    #include "alloc-globals.j"
    
    integer _current_fn
    integer _prev_ins = 0
    
    // struct Fn
    //   Table labels
    //   Instruction entry
    integer array _fn_labels
    integer array _fn_entry
endglobals

#include "alloc.j"

function _parse_line takes string s returns integer
    local integer i = 0
    local integer len = StringLength(s)
    local integer array st
    local integer sl = 0
    local integer ins = _alloc()
    local string bla
    
    loop
    exitwhen i >= len
        if SubString(s, i, i+1) == " " then
            set st[sl] = i
            set sl = sl +1
        endif
        set i = i +1
    endloop
    
    set bla = SubString(s, 0, st[0])
    
    if bla == "lt" then
        set _ins_op[ins] = _Lt
        set _ins_type[ins] = S2I(SubString(st[0], st[1]))
        set _ins_a1[ins] = S2I(SubString(st[1], st[2]))
        set _ins_a2[ins] = S2I(SubString(st[2], st[3]))
        set _ins_a3[ins] = S2I(SubString(st[3], st[4]))
    elseif bla == "le" then
        set _ins_op[ins] = _Le
        set _ins_type[ins] = S2I(SubString(st[0], st[1]))
        set _ins_a1[ins] = S2I(SubString(st[1], st[2]))
        set _ins_a2[ins] = S2I(SubString(st[2], st[3]))
        set _ins_a3[ins] = S2I(SubString(st[3], st[4]))
    elseif bla == "gt" then
        set _ins_op[ins] = _Gt
        set _ins_type[ins] = S2I(SubString(st[0], st[1]))
        set _ins_a1[ins] = S2I(SubString(st[1], st[2]))
        set _ins_a2[ins] = S2I(SubString(st[2], st[3]))
        set _ins_a3[ins] = S2I(SubString(st[3], st[4]))
    elseif bla == "ge" then
        set _ins_op[ins] = _Ge
        set _ins_type[ins] = S2I(SubString(st[0], st[1]))
        set _ins_a1[ins] = S2I(SubString(st[1], st[2]))
        set _ins_a2[ins] = S2I(SubString(st[2], st[3]))
        set _ins_a3[ins] = S2I(SubString(st[3], st[4]))
    elseif bla == "eq" then
        set _ins_op[ins] = _Eq
        set _ins_type[ins] = S2I(SubString(st[0], st[1]))
        set _ins_a1[ins] = S2I(SubString(st[1], st[2]))
        set _ins_a2[ins] = S2I(SubString(st[2], st[3]))
        set _ins_a3[ins] = S2I(SubString(st[3], st[4]))
    elseif bla == "neq" then
        set _ins_op[ins] = _Neq
        set _ins_type[ins] = S2I(SubString(st[0], st[1]))
        set _ins_a1[ins] = S2I(SubString(st[1], st[2]))
        set _ins_a2[ins] = S2I(SubString(st[2], st[3]))
        set _ins_a3[ins] = S2I(SubString(st[3], st[4]))
    elseif bla == "add" then
        set _ins_op[ins] = _Add
        set _ins_type[ins] = S2I(SubString(st[0], st[1]))
        set _ins_a1[ins] = S2I(SubString(st[1], st[2]))
        set _ins_a2[ins] = S2I(SubString(st[2], st[3]))
        set _ins_a3[ins] = S2I(SubString(st[3], st[4]))
    elseif bla == "sub" then
        set _ins_op[ins] = _Sub
        set _ins_type[ins] = S2I(SubString(st[0], st[1]))
        set _ins_a1[ins] = S2I(SubString(st[1], st[2]))
        set _ins_a2[ins] = S2I(SubString(st[2], st[3]))
        set _ins_a3[ins] = S2I(SubString(st[3], st[4]))
    elseif bla == "mul" then
        set _ins_op[ins] = _Mul
        set _ins_type[ins] = S2I(SubString(st[0], st[1]))
        set _ins_a1[ins] = S2I(SubString(st[1], st[2]))
        set _ins_a2[ins] = S2I(SubString(st[2], st[3]))
        set _ins_a3[ins] = S2I(SubString(st[3], st[4]))
    elseif bla == "div" then
        set _ins_op[ins] = _Div
        set _ins_type[ins] = S2I(SubString(st[0], st[1]))
        set _ins_a1[ins] = S2I(SubString(st[1], st[2]))
        set _ins_a2[ins] = S2I(SubString(st[2], st[3]))
        set _ins_a3[ins] = S2I(SubString(st[3], st[4]))
    elseif bla == "neg" then
        set _ins_op[ins] = _Negate
        sset _ins_type[ins] = S2I(SubString(st[0], st[1]))
        set _ins_a1[ins] = S2I(SubString(st[1], st[2]))
        set _ins_a2[ins] = S2I(SubString(st[2], st[3]))
    elseif bla == "set" then
        set _ins_op[ins] = _Set
        set _ins_type[ins] = S2I(SubString(st[0], st[1]))
        set _ins_a1[ins] = S2I(SubString(st[1], st[2]))
        set _ins_a2[ins] = S2I(SubString(st[2], st[3]))
    elseif bla == "bind" then
        set _ins_op[ins] = _Bind
        set _ins_type[ins] = S2I(SubString(st[0], st[1]))
        set _ins_a1[ins] = S2I(SubString(st[1], st[2]))
        set _ins_a2[ins] = S2I(SubString(st[2], st[3]))
    elseif bla == "literal" then
        set _ins_op[ins] = _Lit
        set _ins_type[ins] = S2I(SubString(st[0], st[1]))
        set _ins_a1[ins] = S2I(SubString(st[1], st[2]))
        
        if _ins_type[ins] == _type_integer then
            set _ins_integer[ins] = S2I(SubString(st[2], len))
        elseif _ins_type[ins] == _type_real then
            set _ins_real[ins] = S2R(SubString(st[2], len))
        endif
    elseif bla == "call" then
        set _ins_op[ins] = _Call
        set _ins_type[ins] = S2I(SubString(st[0], st[1]))
        set _ins_a1[ins] = S2I(SubString(st[1], st[2]))
        //set _ins_a2[ins] = S2I(SubString(st[2], st[3]))
        set _ins_literal[ins] = SubString(st[2], len)
    elseif bla == "label" then
        set _ins_op[ins] = _Label
        set _ins_a1[ins] = S2I(SubString(st[0], st[1]))
    elseif bla == "jmp" then
        set _ins_op[ins] = _Jmp
        set _ins_a1[ins] = S2I(SubString(st[0], st[1]))
    elseif bla == "fun" then
        set _ins_op[ins] = _Fun
        set _ins_literal[ins] = SubString(st[0], len)
        //set _ins_a1[ins] = S2I(SubString(st[0], st[1]))
    elseif bla == "jmpt" then
        set _ins_op[ins] = _JmpT
        set _ins_a1[ins] = S2I(SubString(st[0], st[1]))
        set _ins_a2[ins] = S2I(SubString(st[1], st[2]))
    elseif bla == "ret" then
        set _ins_op[ins] = _Ret
    elseif bla == "not" then
        set _ins_op[ins] = _Not
        set _ins_a1[ins] = S2I(SubString(st[0], st[1]))
        set _ins_a2[ins] = S2I(SubString(st[1], st[2]))
    elseif bla == "setglobal" then
        set _ins_op[ins] = _SetGlobal
        set _ins_type[ins] = S2I(SubString(st[0], st[1]))
        set _ins_a1[ins] = S2I(SubString(st[1], st[2]))
        set _ins_a2[ins] = S2I(SubString(st[2], st[3]))
    elseif bla == "setglobalarray" then
        set _ins_op[ins] = _SetGlobalArray
        set _ins_type[ins] = S2I(SubString(st[0], st[1]))
        set _ins_a1[ins] = S2I(SubString(st[1], st[2]))
        set _ins_a2[ins] = S2I(SubString(st[2], st[3]))
        set _ins_a3[ins] = S2I(SubString(st[3], st[4]))
    elseif bla == "getglobal" then
        set _ins_op[ins] = _GetGlobal
        set _ins_type[ins] = S2I(SubString(st[0], st[1]))
        set _ins_a1[ins] = S2I(SubString(st[1], st[2]))
        set _ins_a2[ins] = S2I(SubString(st[2], st[3]))
    elseif bla == "getglobalarray" then
        set _ins_op[ins] = _GetGlobalArray
    elseif bla == "setarray" then
        set _ins_op[ins] = _SetArray
        set _ins_type[ins] = S2I(SubString(st[0], st[1]))
        set _ins_a1[ins] = S2I(SubString(st[1], st[2]))
        set _ins_a2[ins] = S2I(SubString(st[2], st[3]))
        set _ins_a3[ins] = S2I(SubString(st[3], st[4]))
    elseif bla == "getarray" then
        set _ins_op[ins] = _GetArray
        set _ins_type[ins] = S2I(SubString(st[0], st[1]))
        set _ins_a1[ins] = S2I(SubString(st[1], st[2]))
        set _ins_a2[ins] = S2I(SubString(st[2], st[3]))
        set _ins_a3[ins] = S2I(SubString(st[3], st[4]))
    elseif bla == "convert" then
        set _ins_op[ins] = _Convert
        set _ins_type[ins] = S2I(SubString(st[0], st[1]))
        set _ins_a1[ins] = S2I(SubString(st[1], st[2]))
        set _ins_a2[ins] = S2I(SubString(st[2], st[3]))
        set _ins_a3[ins] = S2I(SubString(st[3], st[4]))
    endif

endfunction

function _parse_with_context takes string instruction returns nothing
    local integer ins = _parse_line(instruction)
    set _ins_next[_prev_ins] = ins
    set _prev_ins = ins
    
    if _ins_op[ins] == _Fun then
        set _prev_ins = 0
        set _current_fn = _fn_alloc()
        set _fn_entry[_current_fn] = ins
        call Table#_set_integer(_functions, _ins_a1[ins], _current_fn)
    elseif _ins_op[ins] == _Label then
        call Table#_set_integer(_fn_labels[_current_fn], _ins_a1[ins], ins)
    endif
endfunction

