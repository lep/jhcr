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