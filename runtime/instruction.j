// scope Ins

globals

    constant integer _type_Integer = 94
    constant integer _type_Real = 93
    constant integer _type_Code = 96
    constant integer _type_String = 95
    constant integer _type_Boolean = 97

   
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
    string array _TypeNames


    #include "alloc-globals.j"
    
    // struct Instruction
    //   Instruction next
    //   integer op
    //   integer type
    //   integer a1
    //   integer a2
    //   integer a3
    //   string literal
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

function _B2S takes boolean b returns string
    if b then
        return "true"
    else
        return "false"
    endif
endfunction

function _print takes integer ins returns nothing


    if _op[ins] <= Ins#_GetLocalArray then
        call BJDebugMsg(_OpNames[_op[ins]] +" "+ _TypeNames[_type[ins]] +" "+ I2S(_a1[ins]) +" "+ I2S(_a2[ins]) +" "+ I2S(_a3[ins]))
    elseif _op[ins] <= Ins#_Bind then
        call BJDebugMsg(_OpNames[_op[ins]] +" "+ _TypeNames[_type[ins]] +" "+ I2S(_a1[ins]) +" "+ I2S(_a2[ins]) )
    elseif _op[ins] == Ins#_Lit then
        if Ins#_type[ins] == Ins#_type_String then
            call BJDebugMsg(_OpNames[_op[ins]] +" "+ _TypeNames[_type[ins]] +" "+ (_string[ins]) )
        elseif Ins#_type[ins] == Ins#_type_Integer then
            call BJDebugMsg(_OpNames[_op[ins]] +" "+ _TypeNames[_type[ins]] +" "+ I2S(_integer[ins]) )
        elseif Ins#_type[ins] == Ins#_type_Real then
            call BJDebugMsg(_OpNames[_op[ins]] +" "+ _TypeNames[_type[ins]]  +" "+ R2S(_real[ins]) )
        elseif Ins#_type[ins] == Ins#_type_Boolean then
            call BJDebugMsg(_OpNames[_op[ins]] +" "+ _TypeNames[_type[ins]] +" "+ _B2S(_boolean[ins]) )
        endif
    elseif _op[ins] == Ins#_Call then
        call BJDebugMsg(_OpNames[_op[ins]] +" "+ _TypeNames[_type[ins]] +" "+ I2S(_a1[ins]) +" "+ I2S(_a2[ins]) )
    //elseif _op[ins] == Ins#_Bind then
    //    call BJDebugMsg(_OpNames[_op[ins]] +" "+ _TypeNames[_type[ins]] +" "+ I2S(_a1[ins]) +" "+ I2S(_a2[ins]) )
    elseif _op[ins] == Ins#_Convert then
        call BJDebugMsg(_OpNames[_op[ins]] +" "+ _TypeNames[_type[ins]] +" "+ I2S(_a1[ins]) +" "+ I2S(_a2[ins]) +" "+ I2S(_a3[ins]))
    elseif _op[ins] == Ins#_Label then
        call BJDebugMsg(_OpNames[_op[ins]] +" "+ I2S(_a1[ins]) )
    elseif _op[ins] == Ins#_Jmp then
        call BJDebugMsg(_OpNames[_op[ins]] +" "+ I2S(_a1[ins]) )
    elseif _op[ins] == Ins#_Fun then
        call BJDebugMsg(_OpNames[_op[ins]] +" "+ I2S(_a1[ins]) )
    elseif _op[ins] == Ins#_JmpT then
        call BJDebugMsg(_OpNames[_op[ins]] +" "+ I2S(_a1[ins]) +" "+ I2S(_a2[ins]) )
    elseif _op[ins] == Ins#_Not then
        call BJDebugMsg(_OpNames[_op[ins]] +" "+ I2S(_a1[ins]) +" "+ I2S(_a2[ins]) )
    elseif _op[ins] == Ins#_Ret then
        call BJDebugMsg(_OpNames[_op[ins]] )
    else
        call BJDebugMsg("unknown op " +I2S(Ins#_op[ins]))
    endif

    
endfunction

function _init takes nothing returns nothing
    set _OpNames[_Not]="Not"
    set _OpNames[_Neq]="Neq"
    set _OpNames[_JmpT]="JmpT"
    set _OpNames[_Jmp]="Jmp"
    set _OpNames[_Lit]="Lit"
    set _OpNames[_Bind]="Bind"
    set _OpNames[_Set]="Set"
    set _OpNames[_Call]="Call"
    set _OpNames[_Add]="Add"
    set _OpNames[_Mul]="Mul"
    set _OpNames[_Div]="Div"
    set _OpNames[_Sub]="Sub"
    set _OpNames[_Negate]="Negate"
    set _OpNames[_SetLocalArray]="SetLocalArray"
    set _OpNames[_GetLocalArray]="GetLocalArray"
    set _OpNames[_SetGlobalArray]="SetGlobalArray"
    set _OpNames[_GetGlobalArray]="GetGlobalArray"
    set _OpNames[_SetGlobal]="SetGlobal"
    set _OpNames[_GetGlobal]="GetGlobal"
    set _OpNames[_Ret]="Ret"
    set _OpNames[_Label]="Label"
    set _OpNames[_Eq]="Eq"
    set _OpNames[_Lt]="Lt"
    set _OpNames[_Le]="Le"
    set _OpNames[_Ge]="Ge"
    set _OpNames[_Gt]="Gt"
    set _OpNames[_Convert]="Convert"
    set _OpNames[_Fun]="Fun"
    
    set _TypeNames[_type_Boolean]="boolean"
    set _TypeNames[_type_Integer]="integer"
    set _TypeNames[_type_Real]="real"
    set _TypeNames[_type_Code]="code"
    set _TypeNames[_type_String]="string"
endfunction
