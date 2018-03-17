// scope Parser

globals
    #include "alloc-globals.j"
    
    integer _current_fn
    integer _previns = 0
    
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
        set Ins#_op[ins] = Ins#_Lt
        set Ins#_type[ins] = S2I(SubString(s, st[0], st[1]))
        set Ins#_a1[ins] = S2I(SubString(s, st[1], st[2]))
        set Ins#_a2[ins] = S2I(SubString(s, st[2], st[3]))
        set Ins#_a3[ins] = S2I(SubString(s, st[3], st[4]))
    elseif bla == "le" then
        set Ins#_op[ins] = Ins#_Le
        set Ins#_type[ins] = S2I(SubString(s, st[0], st[1]))
        set Ins#_a1[ins] = S2I(SubString(s, st[1], st[2]))
        set Ins#_a2[ins] = S2I(SubString(s, st[2], st[3]))
        set Ins#_a3[ins] = S2I(SubString(s, st[3], st[4]))
    elseif bla == "gt" then
        set Ins#_op[ins] = Ins#_Gt
        set Ins#_type[ins] = S2I(SubString(s, st[0], st[1]))
        set Ins#_a1[ins] = S2I(SubString(s, st[1], st[2]))
        set Ins#_a2[ins] = S2I(SubString(s, st[2], st[3]))
        set Ins#_a3[ins] = S2I(SubString(s, st[3], st[4]))
    elseif bla == "ge" then
        set Ins#_op[ins] = Ins#_Ge
        set Ins#_type[ins] = S2I(SubString(s, st[0], st[1]))
        set Ins#_a1[ins] = S2I(SubString(s, st[1], st[2]))
        set Ins#_a2[ins] = S2I(SubString(s, st[2], st[3]))
        set Ins#_a3[ins] = S2I(SubString(s, st[3], st[4]))
    elseif bla == "eq" then
        set Ins#_op[ins] = Ins#_Eq
        set Ins#_type[ins] = S2I(SubString(s, st[0], st[1]))
        set Ins#_a1[ins] = S2I(SubString(s, st[1], st[2]))
        set Ins#_a2[ins] = S2I(SubString(s, st[2], st[3]))
        set Ins#_a3[ins] = S2I(SubString(s, st[3], st[4]))
    elseif bla == "neq" then
        set Ins#_op[ins] = Ins#_Neq
        set Ins#_type[ins] = S2I(SubString(s, st[0], st[1]))
        set Ins#_a1[ins] = S2I(SubString(s, st[1], st[2]))
        set Ins#_a2[ins] = S2I(SubString(s, st[2], st[3]))
        set Ins#_a3[ins] = S2I(SubString(s, st[3], st[4]))
    elseif bla == "add" then
        set Ins#_op[ins] = Ins#_Add
        set Ins#_type[ins] = S2I(SubString(s, st[0], st[1]))
        set Ins#_a1[ins] = S2I(SubString(s, st[1], st[2]))
        set Ins#_a2[ins] = S2I(SubString(s, st[2], st[3]))
        set Ins#_a3[ins] = S2I(SubString(s, st[3], st[4]))
    elseif bla == "sub" then
        set Ins#_op[ins] = Ins#_Sub
        set Ins#_type[ins] = S2I(SubString(s, st[0], st[1]))
        set Ins#_a1[ins] = S2I(SubString(s, st[1], st[2]))
        set Ins#_a2[ins] = S2I(SubString(s, st[2], st[3]))
        set Ins#_a3[ins] = S2I(SubString(s, st[3], st[4]))
    elseif bla == "mul" then
        set Ins#_op[ins] = Ins#_Mul
        set Ins#_type[ins] = S2I(SubString(s, st[0], st[1]))
        set Ins#_a1[ins] = S2I(SubString(s, st[1], st[2]))
        set Ins#_a2[ins] = S2I(SubString(s, st[2], st[3]))
        set Ins#_a3[ins] = S2I(SubString(s, st[3], st[4]))
    elseif bla == "div" then
        set Ins#_op[ins] = Ins#_Div
        set Ins#_type[ins] = S2I(SubString(s, st[0], st[1]))
        set Ins#_a1[ins] = S2I(SubString(s, st[1], st[2]))
        set Ins#_a2[ins] = S2I(SubString(s, st[2], st[3]))
        set Ins#_a3[ins] = S2I(SubString(s, st[3], st[4]))
    elseif bla == "neg" then
        set Ins#_op[ins] = Ins#_Negate
        set Ins#_type[ins] = S2I(SubString(s, st[0], st[1]))
        set Ins#_a1[ins] = S2I(SubString(s, st[1], st[2]))
        set Ins#_a2[ins] = S2I(SubString(s, st[2], st[3]))
    elseif bla == "set" then
        set Ins#_op[ins] = Ins#_Set
        set Ins#_type[ins] = S2I(SubString(s, st[0], st[1]))
        set Ins#_a1[ins] = S2I(SubString(s, st[1], st[2]))
        set Ins#_a2[ins] = S2I(SubString(s, st[2], st[3]))
    elseif bla == "bind" then
        set Ins#_op[ins] = Ins#_Bind
        set Ins#_type[ins] = S2I(SubString(s, st[0], st[1]))
        set Ins#_a1[ins] = S2I(SubString(s, st[1], st[2]))
        set Ins#_a2[ins] = S2I(SubString(s, st[2], st[3]))
    elseif bla == "literal" then
        set Ins#_op[ins] = Ins#_Lit
        set Ins#_type[ins] = S2I(SubString(s, st[0], st[1]))
        set Ins#_a1[ins] = S2I(SubString(s, st[1], st[2]))
        
        if Ins#_type[ins] == Ins#_type_Integer then
            set Ins#_integer[ins] = S2I(SubString(s, st[2], len))
        elseif Ins#_type[ins] == Ins#_type_Real then
            set Ins#_real[ins] = S2R(SubString(s, st[2], len))
        endif
    elseif bla == "call" then
        set Ins#_op[ins] = Ins#_Call
        set Ins#_type[ins] = S2I(SubString(s, st[0], st[1]))
        set Ins#_a1[ins] = S2I(SubString(s, st[1], st[2]))
        //set Ins#_a2[ins] = S2I(SubString(s, st[2], st[3]))
        set Ins#_literal[ins] = SubString(s, st[2], len)
    elseif bla == "label" then
        set Ins#_op[ins] = Ins#_Label
        set Ins#_a1[ins] = S2I(SubString(s, st[0], st[1]))
    elseif bla == "jmp" then
        set Ins#_op[ins] = Ins#_Jmp
        set Ins#_a1[ins] = S2I(SubString(s, st[0], st[1]))
    elseif bla == "fun" then
        set Ins#_op[ins] = Ins#_Fun
        set Ins#_literal[ins] = SubString(s, st[0], len)
        //set Ins#_a1[ins] = S2I(SubString(s, st[0], st[1]))
    elseif bla == "jmpt" then
        set Ins#_op[ins] = Ins#_JmpT
        set Ins#_a1[ins] = S2I(SubString(s, st[0], st[1]))
        set Ins#_a2[ins] = S2I(SubString(s, st[1], st[2]))
    elseif bla == "ret" then
        set Ins#_op[ins] = Ins#_Ret
    elseif bla == "not" then
        set Ins#_op[ins] = Ins#_Not
        set Ins#_a1[ins] = S2I(SubString(s, st[0], st[1]))
        set Ins#_a2[ins] = S2I(SubString(s, st[1], st[2]))
    elseif bla == "setglobal" then
        set Ins#_op[ins] = Ins#_SetGlobal
        set Ins#_type[ins] = S2I(SubString(s, st[0], st[1]))
        set Ins#_a1[ins] = S2I(SubString(s, st[1], st[2]))
        set Ins#_a2[ins] = S2I(SubString(s, st[2], st[3]))
    elseif bla == "setglobalarray" then
        set Ins#_op[ins] = Ins#_SetGlobalArray
        set Ins#_type[ins] = S2I(SubString(s, st[0], st[1]))
        set Ins#_a1[ins] = S2I(SubString(s, st[1], st[2]))
        set Ins#_a2[ins] = S2I(SubString(s, st[2], st[3]))
        set Ins#_a3[ins] = S2I(SubString(s, st[3], st[4]))
    elseif bla == "getglobal" then
        set Ins#_op[ins] = Ins#_GetGlobal
        set Ins#_type[ins] = S2I(SubString(s, st[0], st[1]))
        set Ins#_a1[ins] = S2I(SubString(s, st[1], st[2]))
        set Ins#_a2[ins] = S2I(SubString(s, st[2], st[3]))
    elseif bla == "getglobalarray" then
        set Ins#_op[ins] = Ins#_GetGlobalArray
    elseif bla == "setarray" then
        set Ins#_op[ins] = Ins#_SetArray
        set Ins#_type[ins] = S2I(SubString(s, st[0], st[1]))
        set Ins#_a1[ins] = S2I(SubString(s, st[1], st[2]))
        set Ins#_a2[ins] = S2I(SubString(s, st[2], st[3]))
        set Ins#_a3[ins] = S2I(SubString(s, st[3], st[4]))
    elseif bla == "getarray" then
        set Ins#_op[ins] = Ins#_GetArray
        set Ins#_type[ins] = S2I(SubString(s, st[0], st[1]))
        set Ins#_a1[ins] = S2I(SubString(s, st[1], st[2]))
        set Ins#_a2[ins] = S2I(SubString(s, st[2], st[3]))
        set Ins#_a3[ins] = S2I(SubString(s, st[3], st[4]))
    elseif bla == "convert" then
        set Ins#_op[ins] = Ins#_Convert
        set Ins#_type[ins] = S2I(SubString(s, st[0], st[1]))
        set Ins#_a1[ins] = S2I(SubString(s, st[1], st[2]))
        set Ins#_a2[ins] = S2I(SubString(s, st[2], st[3]))
        set Ins#_a3[ins] = S2I(SubString(s, st[3], st[4]))
    endif
    return ins
endfunction

function _parse_with_context takes string instruction returns nothing
    local integer ins = _parse_line(instruction)
    set Ins#_next[_previns] = ins
    set _previns = ins
    
    if Ins#_op[ins] == Ins#_Fun then
        set _previns = 0
        set _current_fn = _alloc()
        set _fn_entry[_current_fn] = ins
        call Names#_insert_function(Ins#_literal[ins], _current_fn)
    elseif Ins#_op[ins] == Ins#_Label then
        call Table#_set_integer(_fn_labels[_current_fn], Ins#_a1[ins], ins)
    endif
endfunction

