// scope Ins
// REQUIRES Types Print

globals
  
    constant integer _Lt = 1
    constant integer _Le = 2
    constant integer _Gt = 3
    constant integer _Ge = 4
    constant integer _Eq = 5
    constant integer _Neq = 6
    
    constant integer _Add = 7
    constant integer _Sub = 8
    constant integer _Mul = 9
    constant integer _Div = 10
    constant integer _Mod = 11
    
    constant integer _SetLocalArray = 12
    constant integer _GetLocalArray = 13
    constant integer _SetGlobalArray = 14
    constant integer _GetGlobalArray = 15
    
    constant integer _Negate = 16
    constant integer _Set = 17
    constant integer _SetGlobal = 18
    constant integer _GetGlobal = 19
    constant integer _Bind = 20
    
    constant integer _Lit = 21
    constant integer _Call = 22
    constant integer _Convert = 23
 

    constant integer _Label = 24
    constant integer _Jmp = 25
    constant integer _Fun = 26
    constant integer _JmpT = 27
    
    constant integer _Not = 28
    
    constant integer _Ret = 29
    
    
    
    string array _OpNames


    #include "alloc-globals.j"
    
    // struct Instruction
    //   Instruction next
    //   integer op
    //   integer type
    //   integer a1
    //   integer a2
    //   integer a3
    // literals
    //   string _string
    //   integer _integer
    //   real _real
    //   boolean _boolean
    integer array _next
    integer array _op
    integer array _type

    integer array _a1
    integer array _a2
    integer array _a3

    // literals
    string array _string
    integer array _integer
    real array _real
    boolean array _boolean

endglobals

#include "alloc.j"

function _repr takes integer _ins returns string
    return I2S(_op[_ins]) +" "+ I2S(_type[_ins]) +" "+ I2S(_a1[_ins]) +" "+ I2S(_a2[_ins]) +" "+ I2S(_a3[_ins])
endfunction

function _toString takes integer _ins returns string
    if _op[_ins] <= _GetLocalArray then
        return(_OpNames[_op[_ins]] +" "+ Types#_TypeNames[_type[_ins]] +" "+ I2S(_a1[_ins]) +" "+ I2S(_a2[_ins]) +" "+ I2S(_a3[_ins]))
    elseif _op[_ins] <= _Bind then
        return(_OpNames[_op[_ins]] +" "+ Types#_TypeNames[_type[_ins]] +" "+ I2S(_a1[_ins]) +" "+ I2S(_a2[_ins]) )
    elseif _op[_ins] == _Lit then
        if _type[_ins] == Types#_string then
            return(_OpNames[_op[_ins]] +" "+ Types#_TypeNames[_type[_ins]] +" "+ I2S(_a1[_ins]) +" "+ (_string[_ins]) )
        elseif _type[_ins] == Types#_integer then
            return(_OpNames[_op[_ins]] +" "+ Types#_TypeNames[_type[_ins]] +" "+ I2S(_a1[_ins]) +" "+ I2S(_integer[_ins]) )
        elseif _type[_ins] == Types#_real then
            return(_OpNames[_op[_ins]] +" "+ Types#_TypeNames[_type[_ins]] +" "+ I2S(_a1[_ins]) +" "+ R2S(_real[_ins]) )
        elseif _type[_ins] == Types#_boolean then
            return(_OpNames[_op[_ins]] +" "+ Types#_TypeNames[_type[_ins]] +" "+ I2S(_a1[_ins]) +" "+ Print#_b2s(_boolean[_ins]) )
        else
            return _OpNames[_op[_ins]] +" "+ Types#_TypeNames[_type[_ins]] +" "+ I2S(_a1[_ins]) +" null"
        endif
    elseif _op[_ins] == _Call then
        return(_OpNames[_op[_ins]] +" "+ I2S(_a1[_ins]) +" "+ I2S(_a2[_ins]) )
    elseif _op[_ins] == _Convert then
        return(_OpNames[_op[_ins]] +" "+ Types#_TypeNames[_type[_ins]] +" "+ I2S(_a1[_ins]) +" "+ Types#_TypeNames[_a2[_ins]] +" "+ I2S(_a3[_ins]))
    elseif _op[_ins] == _Label then
        return(_OpNames[_op[_ins]] +" "+ I2S(_a1[_ins]) )
    elseif _op[_ins] == _Jmp then
        return(_OpNames[_op[_ins]] +" "+ I2S(_a1[_ins]) )
    elseif _op[_ins] == _Fun then
        if _a1[_ins] < 0 then
            return(_OpNames[_op[_ins]] +" "+ I2S(_a1[_ins]) +" "+ _string[_ins])
        else
            return(_OpNames[_op[_ins]] +" "+ I2S(_a1[_ins]) )
        endif
    elseif _op[_ins] == _JmpT then
        return(_OpNames[_op[_ins]] +" "+ I2S(_a1[_ins]) +" "+ I2S(_a2[_ins]) )
    elseif _op[_ins] == _Not then
        return(_OpNames[_op[_ins]] +" "+ I2S(_a1[_ins]) +" "+ I2S(_a2[_ins]) )
    elseif _op[_ins] == _Ret then
        return(_OpNames[_op[_ins]] +" "+ Types#_TypeNames[_type[_ins]] )
    else
        return("unknown op " +I2S(_op[_ins]))
    endif

endfunction

function _destroy takes integer _ins returns nothing
    loop
    exitwhen _ins == 0
        call _free(_ins)
        set _ins = _next[_ins]
    endloop
endfunction

function _print takes integer _i returns nothing
    //call Print#_print(I2S(_op[_ins]) +" "+ I2S(_type[_ins]) +" "+ I2S(_a1[_ins]) +" "+ I2S(_a2[_ins]) +" "+ I2S(_a3[_ins]))
    call Print#_print(_toString(_i))
endfunction

function _init takes nothing returns nothing
    set _OpNames[_Not]="not"
    set _OpNames[_Neq]="neq"
    set _OpNames[_JmpT]="jmpt"
    set _OpNames[_Jmp]="jmp"
    set _OpNames[_Lit]="lit"
    set _OpNames[_Bind]="bind"
    set _OpNames[_Set]="set"
    set _OpNames[_Call]="call"
    set _OpNames[_Add]="add"
    set _OpNames[_Mul]="mul"
    set _OpNames[_Div]="div"
    set _OpNames[_Sub]="sub"
    set _OpNames[_Mod]="mod"
    set _OpNames[_Negate]="neg"
    set _OpNames[_SetLocalArray]="sla"
    set _OpNames[_GetLocalArray]="gla"
    set _OpNames[_SetGlobalArray]="sga"
    set _OpNames[_GetGlobalArray]="gga"
    set _OpNames[_SetGlobal]="sg"
    set _OpNames[_GetGlobal]="gg"
    set _OpNames[_Ret]="ret"
    set _OpNames[_Label]="lbl"
    set _OpNames[_Eq]="eq"
    set _OpNames[_Lt]="lt"
    set _OpNames[_Le]="le"
    set _OpNames[_Ge]="ge"
    set _OpNames[_Gt]="gt"
    set _OpNames[_Convert]="conv"
    set _OpNames[_Fun]="fun"

endfunction
