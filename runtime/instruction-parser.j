// scope Parser

globals
    #include "alloc-globals.j"
    
    constant integer _OpWidth = 2
    constant integer _TypeWidth = 3
    constant integer _RegWidth = 11
    constant integer _LabelWidth = 6
    
    integer _S = 0
    integer _current_fn = 0
    integer _prev_ins = 0
    
    // Table<lbl, _ins> array with idx = fid + 100
    integer array _fn_labels
    // use (fid + 100) as index where 100 is the number of dummy functions
    integer array _fn_entry
endglobals

#include "alloc.j"

function _parse__ins takes string _s returns integer
    local integer _ins = S2I(SubString(_s, _S, _S+2))
    local integer _new = _alloc()
    local integer _b
    
    set Ins#_op[_new] = _ins
    
    if _ins <= Ins#_GetLocalArray then
        set Ins#_type[_new] = S2I(SubString(_s, _S+2,  _S+ 5))
        set Ins#_a1[_new]   = S2I(SubString(_s, _S+5,  _S+16))
        set Ins#_a2[_new]   = S2I(SubString(_s, _S+16, _S+27))
        set Ins#_a3[_new]   = S2I(SubString(_s, _S+27, _S+38))
        set _S = _S + 38
    elseif _ins <= Ins#_Bind then
        set Ins#_type[_new] = S2I(SubString(_s, _S+2,  _S+ 5))
        set Ins#_a1[_new]   = S2I(SubString(_s, _S+5,  _S+16))
        set Ins#_a2[_new]   = S2I(SubString(_s, _S+16, _S+27))
        set _S = _S + 27
    elseif _ins == Ins#_Lit then
        set Ins#_type[_new] = S2I(SubString(_s, _S+2,  _S+ 5))
        set Ins#_a1[_new]   = S2I(SubString(_s, _S+5,  _S+16))
        set _b              = S2I(SubString(_s, _S+16, _S+22))
        if Ins#_type[_new] == Types#_string then
            set Ins#_string[_new] = SubString(_s, _S+22, _S+22+_b)
        elseif Ins#_type[_new] == Types#_integer then
            set Ins#_integer[_new] = S2I(SubString(_s, _S+22, _S+22+_b))
        elseif Ins#_type[_new] == Types#_real then
            set Ins#_real[_new] = S2R(SubString(_s, _S+22, _S+22+_b))
        elseif Ins#_type[_new] == Types#_boolean then
            set Ins#_boolean[_new] = SubString(_s, _S+22, _S+22+_b)=="True"
        endif
        set _S = _S + 22 + _b
    elseif _ins == Ins#_Call then
        set Ins#_type[_new] = S2I(SubString(_s, _S+2,  _S+ 5))
        set Ins#_a1[_new]   = S2I(SubString(_s, _S+5,  _S+16))
        set Ins#_a2[_new]   = S2I(SubString(_s, _S+16, _S+22))
        set _S = _S + 22
    elseif _ins == Ins#_Convert then
        set Ins#_type[_new] = S2I(SubString(_s, _S+2,  _S+ 5))
        set Ins#_a1[_new]   = S2I(SubString(_s, _S+5,  _S+16))
        set Ins#_a2[_new]   = S2I(SubString(_s, _S+16, _S+19))
        set Ins#_a3[_new]   = S2I(SubString(_s, _S+19, _S+30))
        set _S = _S + 30
    elseif _ins == Ins#_Label then
        set Ins#_a1[_new] = S2I(SubString(_s, _S+2, _S+8))
        set _S = _S + 8
    elseif _ins == Ins#_Jmp then
        set Ins#_a1[_new] = S2I(SubString(_s, _S+2, _S+8))
        set _S = _S + 8
    elseif _ins == Ins#_Fun then
        set Ins#_a1[_new] = S2I(SubString(_s, _S+2, _S+8))
        set _S = _S + 8
    elseif _ins == Ins#_JmpT then
        set Ins#_a1[_new] = S2I(SubString(_s, _S+2,  _S+13))
        set Ins#_a2[_new] = S2I(SubString(_s, _S+13, _S+19))
        set _S = _S + 19
    elseif _ins == Ins#_Not then
        set Ins#_a1[_new] = S2I(SubString(_s, _S+2,  _S+13))
        set Ins#_a2[_new] = S2I(SubString(_s, _S+13, _S+24))
        set _S = _S + 24
    elseif _ins == Ins#_Ret then
        set _S = _S + 2
    endif
    return _new
endfunction

function _parse_and_print takes string _instruction returns nothing
    local integer _len = StringLength(_instruction)
    local integer _ins
    set _S = 0
    loop
    exitwhen _S >= _len
        set _ins = _parse__ins(_instruction)
        call Ins#_print(_ins)
    endloop
endfunction

function _parse_and_init takes string _instruction returns nothing
    local integer _len = StringLength(_instruction)
    local integer _ins
    set _S = 0
    loop
    exitwhen _S >= _len
        set _ins = _parse__ins(_instruction)
        //call Ins#_print(_ins)
        
        if Ins#_op[_ins] == Ins#_Fun then
            set _current_fn = Ins#_a1[_ins]
            set _fn_entry[_current_fn + 100] = _ins
            if _fn_labels[_current_fn + 100] == 0 then
                set _fn_labels[_current_fn + 100] = Table#_alloc()
            endif
            call Modified#_set_modified(_current_fn)
            set _prev_ins = 0

        elseif Ins#_op[_ins] == Ins#_Label then
            call Table#_set_integer(_fn_labels[_current_fn + 100], Ins#_a1[_ins], _ins)
        endif
        
        if _prev_ins != 0 then
            set Ins#_next[_prev_ins] = _ins
        endif
        set _prev_ins = _ins

    endloop
endfunction
