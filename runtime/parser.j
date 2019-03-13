globals
    Table functions
    Instruction last = 0
    Fn fn
    Instruction prev = 0
endglobals

globals
    boolean array Op_Untyped
    integer array Op_Arity
endglobals

struct Fn
    Table labels
    Instruction entry
endstruct

struct Context
    Fn fn
    Context parent
    Table locals
    Table arrays
    Instruction pc
endstruct


function parse_ins takes string s returns string
    set last = Ins.create()
    set last.op = S2I(SubString(s, 0, 2))
    
    if last.op == Op_Literal then
        set last.ty = S2I(SubString(s, 2, 4))
        set last.a1 = S2I(SubString(s, 4, 15))
        set last.a2 = S2I(SubString(s, 15, 26))
        if last.ty == Type_integer then
            set last.a3 = S2I(SubString(s, 26, 26+a2))
        elseif last.ty == Type_real then
            set last.real_lit = S2R(SubString(s, 26, 26+a2))
        elseif last.ty == Type_string then
            set last.string_lit = SubString(s, 26, 26+a2)
        elseif last.ty == Type_boolean then
            set last.bool_lit = SubString(s, 26, 26+a2) == "true"
        endif

        return SubString(s, 26+a2, 10000)
    elseif last.op == Ret then
        return SubString(s, 2, 10000)
    
    // Fun, Label, Jmp
    elseif Op_Untyped[last.op] then
        set last.a1 = S2I(SubString(s, 2, 13))
        return SubString(s, 13, 10000)
        
    // untyped & arity 2
    elseif last.op == Op_Not or last.op == Op_JmpT then
        set last.a1 = S2I(SubString(s, 2, 13))
        set last.a2 = S2I(SubString(s, 13, 24))
        return SubString(s, 24, 10000)
        
    elseif Op_Arity[last.op] == 2 then
        set last.ty = S2I(SubString(s, 2, 4))
        set last.a1 = S2I(SubString(s, 4, 15))
        set last.a2 = S2I(SubString(s, 15, 26))
        return SubString(s, 26, 10000)
        
    elseif Op_Arity[last.op] == 3 then
        set last.ty = S2I(SubString(s, 2, 4))
        set last.a1 = S2I(SubString(s, 4, 15))
        set last.a2 = S2I(SubString(s, 15, 26))
        set last.a3 = S2I(SubString(s, 26, 37))
        return SubString(s, 37, 10000)
    endif

endfunction

function parse takes string s returns nothing
    loop
    exitwhen s == ""
        set s = parse_ins(s)
        if last.op == Op_Fun then
            set fn = Fn.create()
            set fn.entry = last
            call _set_integer(functions, last.a1, last)
        elseif last.op == Op_Label then
            call _set_integer(fn.labels, last.a1, last)
        endif
        set prev.next = last
        set prev = last
    endloop
endfunction


