// scope Ins

globals

    constant integer _type_Integer = 94
    constant integer _type_Real = 93
    constant integer _type_Code = 96
    constant integer _type_String = 95
    constant integer _type_Boolean = 97

    constant integer _Not = 1
    constant integer _Neq = 2
    constant integer _JmpT = 3
    constant integer _Jmp = 4
    constant integer _Lit = 5
    constant integer _Bind = 6
    constant integer _Set = 7
    constant integer _Call = 8
    constant integer _Add = 9
    constant integer _Mul = 10
    constant integer _Div = 11
    constant integer _Sub = 12
    constant integer _Negate = 13
    constant integer _SetArray = 14
    constant integer _GetArray = 15
    constant integer _SetGlobalArray = 16
    constant integer _GetGlobalArray = 17
    constant integer _SetGlobal = 18
    constant integer _GetGlobal = 19
    constant integer _Ret = 20
    constant integer _Label = 21
    constant integer _Eq = 22
    constant integer _Lt = 23
    constant integer _Le = 24
    constant integer _Ge = 25
    constant integer _Gt = 26
    constant integer _Convert = 27
    constant integer _Fun = 28
    
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

    string array _literal
    integer array _integer
    real array _real

endglobals

#include "alloc.j"

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
    set _OpNames[_SetArray]="SetArray"
    set _OpNames[_GetArray]="GetArray"
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
