// scope Parser

globals
    #include "alloc-globals.j"
    
    constant integer _OpWidth = 2
    constant integer _TypeWidth = 3
    constant integer _RegWidth = 11
    constant integer _LabelWidth = 6
    
    integer _S = 0
    integer _current_fn = 0
    integer _previns = 0
    
    // table from (fn, label) -> ins
    integer _fn_labels = 0 
    // use (fid + 100) as index where 100 is the number of dummy functions
    integer array _fn_entry
endglobals

#include "alloc.j"

function _parse_ins takes string s returns integer
    local integer ins = S2I(SubString(s, _S, _S+2))
    local integer new = _alloc()
    local integer b
    
    set Ins#_op[new] = ins
    
    if ins <= Ins#_GetLocalArray then
        set Ins#_type[new] = S2I(SubString(s, _S+2,  _S+ 5))
        set Ins#_a1[new]   = S2I(SubString(s, _S+5,  _S+16))
        set Ins#_a2[new]   = S2I(SubString(s, _S+16, _S+27))
        set Ins#_a3[new]   = S2I(SubString(s, _S+27, _S+38))
        set _S = _S + 38
    elseif ins <= Ins#_Bind then
        set Ins#_type[new] = S2I(SubString(s, _S+2,  _S+ 5))
        set Ins#_a1[new]   = S2I(SubString(s, _S+5,  _S+16))
        set Ins#_a2[new]   = S2I(SubString(s, _S+16, _S+27))
        set _S = _S + 27
    elseif ins == Ins#_Lit then
        set Ins#_type[new] = S2I(SubString(s, _S+2,  _S+ 5))
        set Ins#_a1[new]   = S2I(SubString(s, _S+5,  _S+16))
        set b              = S2I(SubString(s, _S+16, _S+22))
        if Ins#_type[new] == Types#_string then
            set Ins#_string[new] = SubString(s, _S+22, _S+22+b)
        elseif Ins#_type[new] == Types#_integer then
            set Ins#_integer[new] = S2I(SubString(s, _S+22, _S+22+b))
        elseif Ins#_type[new] == Types#_real then
            set Ins#_real[new] = S2R(SubString(s, _S+22, _S+22+b))
        elseif Ins#_type[new] == Types#_boolean then
            set Ins#_boolean[new] = SubString(s, _S+22, _S+22+b)=="True"
        endif
        set _S = _S + 22 + b
    elseif ins == Ins#_Call then
        set Ins#_type[new] = S2I(SubString(s, _S+2,  _S+ 5))
        set Ins#_a1[new]   = S2I(SubString(s, _S+5,  _S+16))
        set Ins#_a2[new]   = S2I(SubString(s, _S+16, _S+22))
        set _S = _S + 22
    elseif ins == Ins#_Convert then
        set Ins#_type[new] = S2I(SubString(s, _S+2,  _S+ 5))
        set Ins#_a1[new]   = S2I(SubString(s, _S+5,  _S+16))
        set Ins#_a2[new]   = S2I(SubString(s, _S+16, _S+19))
        set Ins#_a3[new]   = S2I(SubString(s, _S+19, _S+30))
        set _S = _S + 30
    elseif ins == Ins#_Label then
        set Ins#_a1[new] = S2I(SubString(s, _S+2, _S+8))
        set _S = _S + 8
    elseif ins == Ins#_Jmp then
        set Ins#_a1[new] = S2I(SubString(s, _S+2, _S+8))
        set _S = _S + 8
    elseif ins == Ins#_Fun then
        set Ins#_a1[new] = S2I(SubString(s, _S+2, _S+8))
        set _S = _S + 8
    elseif ins == Ins#_JmpT then
        set Ins#_a1[new] = S2I(SubString(s, _S+2,  _S+13))
        set Ins#_a2[new] = S2I(SubString(s, _S+13, _S+19))
        set _S = _S + 19
    elseif ins == Ins#_Not then
        set Ins#_a1[new] = S2I(SubString(s, _S+2,  _S+13))
        set Ins#_a2[new] = S2I(SubString(s, _S+13, _S+24))
        set _S = _S + 24
    elseif ins == Ins#_Ret then
        set _S = _S + 2
    endif
    return new
endfunction

function _parse_and_print takes string instruction returns nothing
    local integer len = StringLength(instruction)
    local integer ins
    set _S = 0
    loop
    exitwhen _S >= len
        set ins = _parse_ins(instruction)
        call Ins#_print(ins)
    endloop
endfunction

function _parse_and_init takes string instruction returns nothing
    local integer len = StringLength(instruction)
    local integer ins
    set _S = 0
    loop
    exitwhen _S >= len
        set ins = _parse_ins(instruction)
        //call Ins#_print(ins)
        
        if Ins#_op[ins] == Ins#_Fun then
            set _current_fn = Ins#_a1[ins]
            set _fn_entry[_current_fn + 100] = ins
            call Modified#_set_modified(_current_fn)
            set _previns = 0

        elseif Ins#_op[ins] == Ins#_Label then
            call Table#_set_integer(_current_fn, Ins#_a1[ins], ins)
        endif
        
        if _previns != 0 then
            set Ins#_next[_previns] = ins
        endif
        set _previns = ins

    endloop
endfunction
