// scope Parser
// REQUIRES Ins Types StringTable Modified Wrap Table

globals
    constant integer _OpWidth = 2
    constant integer _TypeWidth = 3
    constant integer _RegWidth = 9
    constant integer _LabelWidth = 6
    
    integer _S = 0
    integer _current_fn = 0
    integer _prev_ins = 0
    
    // Table<lbl, _ins> array with idx = fid + 100
    integer array _fn_labels
    // use (fid + 100) as index where 100 is the number of dummy functions
    integer array _fn_entry
endglobals


function _parse_ins takes string _s returns integer
    local integer _ins = S2I(SubString(_s, _S, _S+2))
    local integer _new = Ins#_alloc()
    local integer _b
    
    set Ins#_op[_new] = _ins
    set Ins#_next[_new] = 0
    
    // not the less-or-equal: we handle all two-operand arithmetic instructions
    // here and all array instructions the same here.
    if _ins <= Ins#_GetGlobalArray then
        set Ins#_type[_new] = S2I(SubString(_s, _S+2,  _S+ 5))
        set Ins#_a1[_new]   = S2I(SubString(_s, _S+5,  _S+14))
        set Ins#_a2[_new]   = S2I(SubString(_s, _S+14, _S+23))
        set Ins#_a3[_new]   = S2I(SubString(_s, _S+23, _S+32))
        set _S = _S + 32
    // handles negate, set, {set,get}global and bind
    elseif _ins <= Ins#_Bind then
        set Ins#_type[_new] = S2I(SubString(_s, _S+2,  _S+ 5))
        set Ins#_a1[_new]   = S2I(SubString(_s, _S+5,  _S+14))
        set Ins#_a2[_new]   = S2I(SubString(_s, _S+14, _S+23))
        set _S = _S + 23
    elseif _ins == Ins#_Lit then
        set Ins#_type[_new] = S2I(SubString(_s, _S+2,  _S+ 5))
        set Ins#_a1[_new]   = S2I(SubString(_s, _S+5,  _S+14))
        set _b              = S2I(SubString(_s, _S+14, _S+20))
        if Ins#_type[_new] == Types#_string then
            set Ins#_string[_new] = SubString(_s, _S+20, _S+20+_b)
        elseif Ins#_type[_new] == Types#_integer then
            set Ins#_integer[_new] = S2I(SubString(_s, _S+20, _S+20+_b))
        elseif Ins#_type[_new] == Types#_real then
            set Ins#_real[_new] = S2R(SubString(_s, _S+20, _S+20+_b))
        elseif Ins#_type[_new] == Types#_boolean then
            set Ins#_boolean[_new] = SubString(_s, _S+20, _S+20+_b)=="true"
        endif
        set _S = _S + 20 + _b
    elseif _ins == Ins#_Call then
        set Ins#_a1[_new]   = S2I(SubString(_s, _S+2,  _S+11))
        set Ins#_a2[_new]   = S2I(SubString(_s, _S+11, _S+17))
        set _S = _S + 17
    elseif _ins == Ins#_Convert then
        set Ins#_type[_new] = S2I(SubString(_s, _S+2,  _S+ 5))
        set Ins#_a1[_new]   = S2I(SubString(_s, _S+5,  _S+14))
        set Ins#_a2[_new]   = S2I(SubString(_s, _S+14, _S+17))
        set Ins#_a3[_new]   = S2I(SubString(_s, _S+17, _S+26))
        set _S = _S + 26
    elseif _ins == Ins#_Label then
        set Ins#_a1[_new] = S2I(SubString(_s, _S+2, _S+8))
        set _S = _S + 8
    elseif _ins == Ins#_Jmp then
        set Ins#_a1[_new] = S2I(SubString(_s, _S+2, _S+8))
        set _S = _S + 8
    elseif _ins == Ins#_Fun then
        set Ins#_a1[_new] = S2I(SubString(_s, _S+2, _S+8))
        
        // new function
        if Ins#_a1[_new] < 0 then
            set _b = S2I(SubString(_s, _S+8, _S+14))
            set Ins#_string[_new] = SubString(_s, _S+14, _S+14 +_b)
            set _S = _S + 14 + _b
        else
            set _S = _S + 8
        endif
    elseif _ins == Ins#_JmpT then
        set Ins#_a1[_new] = S2I(SubString(_s, _S+2,  _S+8))
        set Ins#_a2[_new] = S2I(SubString(_s, _S+8, _S+17))
        set _S = _S + 17
    elseif _ins == Ins#_Not then
        set Ins#_a1[_new] = S2I(SubString(_s, _S+2,  _S+11))
        set Ins#_a2[_new] = S2I(SubString(_s, _S+11, _S+20))
        set _S = _S + 20
    elseif _ins == Ins#_Ret then
        set Ins#_type[_new] = S2I(SubString(_s, _S+2,  _S+ 5))
        set _S = _S + 5
    endif
    return _new
endfunction

function _parse_and_print takes string _instruction returns nothing
    local integer _len = StringLength(_instruction)
    local integer _ins
    set _S = 0
    loop
    exitwhen _S >= _len
        set _ins = _parse_ins(_instruction)
        call Ins#_print(_ins)
    endloop
endfunction

function _parse_and_init takes string _instruction returns nothing
    local integer _len = StringLength(_instruction)
    local integer _ins
    set _S = 0
    loop
    exitwhen _S >= _len
        set _ins = _parse_ins(_instruction)
        //call Print#_print("Parsed instruction ("+ I2S(_ins) +") "+ Ins#_toString(_ins))
        
        if Ins#_op[_ins] == Ins#_Fun then
            set _current_fn = Ins#_a1[_ins]
            
            if _fn_labels[_current_fn + 100] == 0 then
                set _fn_labels[_current_fn + 100] = Table#_alloc()
            endif
            
            if _fn_entry[_current_fn + 100] != 0 then
                call Ins#_destroy(_fn_entry[_current_fn + 100])
            endif
            
            set _fn_entry[_current_fn + 100] = _ins
            
            if _current_fn < 0 then
                call StringTable#_set(Wrap#_name2id, Ins#_string[_ins], _current_fn)
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

function _parse_globals takes string _instruction, integer _entry returns integer
    local integer _len = StringLength(_instruction)
    local integer _ins
    set _S = 0
    loop
    exitwhen _S >= _len
        set _ins = _parse_ins(_instruction)
        
        // the very first instruction
        if _entry == 0 then
            set _entry = _ins
        endif
        
        // we know we only have code compiled from globals init here
        // so we don't check a bunch of stuff
        
        if _prev_ins != 0 then
            set Ins#_next[_prev_ins] = _ins
        endif
        set _prev_ins = _ins
    endloop
    return _entry
endfunction
