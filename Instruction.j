
struct Instruction
    Instruction next
    
    integer OP
    integer type
    boolean literal
    
    // always registers or similar
    integer a1
    integer a2
    
    // might be literal
    integer a3_integer
    real a3_real
    boolean a3_boolean
    string a3_string
endstruct

struct AuxTable
    AuxTable next = 0
    integer ident
    
    gamecache gamecacheE
    terraindeformation terraindeformationE
    weathereffect weathereffectE
    camerasetup camerasetupE
endstruct


struct Context
    static hashtable locals = InitHashtable()
    static hashtable labels = InitHashtable()
    
    static Context fresh = 0
    
    AuxTable auxLocals = 0
    
    Context parent = 0
    Instruction pc
endstruct


function interp takes Context ctx returns nothing
    local Instruction op = ctx.pc
    local integer t = op.OP
    local boolean l = lit
    // TODO: binsearch
    if t == Set then
    
        // TODO: binsearch
        if op.type == TypeInteger then
            if lit then
                set ctx.tbl[op.a1].integer = op.a3_integer
            else
                set ctx.tbl[op.a1].integer = ctx.tbl[op.a3_integer].integer
            endif
        elseif op.type == TypeReal then
            if lit then
                set ctx.tbl[op.a1].real = op.a3_real
            else
                set ctx.tbl[op.a1].real = ctx.tbl[op.a3_integer].real
            endif
        elseif op.type == TypeBoolean then
            if lit then
                set ctx.tbl[op.a1].boolean = op.a3_boolean
            else
                set ctx.tbl[op.a1].boolean = ctx.tbl[op.a3_integer].boolean
            endif
        elseif op.type == TypeString then
            if lit then
                set ctx.tbl[op.a1].string = op.a3_string
            else
                set ctx.tbl[op.a1].string = ctx.tbl[op.a3_integer].string
            endif
        elseif op.type == TypeCode then
            if lit then
                set ctx.tbl[op.a1].integer = op.a3_integer
            else
                set ctx.tbl[op.a1].integer = ctx.tbl[op.a3_integer].integer
            endif
            
            // remember: handle literal can only be null
        endif
        
    elseif t == SetArr then
    elseif t == GetArr then
    
    elseif t == Lt then
        if op.type == TypeInteger then
            if lit then
                set ctx.tbl[op.a1].boolean = ctx.tbl[op.a2].integer < op.a3_integer
            else
                set ctx.tbl[op.a1].boolean = ctx.tbl[op.a2].integer < ctx.tbl[op.a3_integer].integer
            endif
        else
            if lit then
                set ctx.tbl[op.a1].boolean = ctx.tbl[op.a2].real < op.a3_real
            else
                set ctx.tbl[op.a1].boolean = ctx.tbl[op.a2].real < ctx.tbl[op.a3_real].real
            endif
        endif
        
    elseif t == Le then
        if op.type == TypeInteger then
            if lit then
                set ctx.tbl[op.a1].boolean = ctx.tbl[op.a2].integer <= op.a3_integer
            else
                set ctx.tbl[op.a1].boolean = ctx.tbl[op.a2].integer <= ctx.tbl[op.a3_integer].integer
            endif
        else
            if lit then
                set ctx.tbl[op.a1].boolean = ctx.tbl[op.a2].real <= op.a3_real
            else
                set ctx.tbl[op.a1].boolean = ctx.tbl[op.a2].real <= ctx.tbl[op.a3_real].real
            endif
        endif
        
    elseif t == Gt then
        if op.type == TypeInteger then
            if lit then
                set ctx.tbl[op.a1].boolean = ctx.tbl[op.a2].integer > op.a3_integer
            else
                set ctx.tbl[op.a1].boolean = ctx.tbl[op.a2].integer > ctx.tbl[op.a3_integer].integer
            endif
        else
            if lit then
                set ctx.tbl[op.a1].boolean = ctx.tbl[op.a2].real > op.a3_real
            else
                set ctx.tbl[op.a1].boolean = ctx.tbl[op.a2].real > ctx.tbl[op.a3_real].real
            endif
        endif
        
    elseif t == Ge then
        if op.type == TypeInteger then
            if lit then
                set ctx.tbl[op.a1].boolean = ctx.tbl[op.a2].integer >= op.a3_integer
            else
                set ctx.tbl[op.a1].boolean = ctx.tbl[op.a2].integer >= ctx.tbl[op.a3_integer].integer
            endif
        else
            if lit then
                set ctx.tbl[op.a1].boolean = ctx.tbl[op.a2].real >= op.a3_real
            else
                set ctx.tbl[op.a1].boolean = ctx.tbl[op.a2].real >= ctx.tbl[op.a3_real].real
            endif
        endif
        
    elseif t == Eq then
        if op.type == TypeInteger then
            if lit then
                set ctx.tbl[op.a1].boolean = ctx.tbl[op.a2].integer == op.a3_integer
            else
                set ctx.tbl[op.a1].boolean = ctx.tbl[op.a2].integer == ctx.tbl[op.a3_integer].integer
            endif
        elseif op.type == TypeReal then
            if lit then
                set ctx.tbl[op.a1].boolean = ctx.tbl[op.a2].real == op.a3_real
            else
                set ctx.tbl[op.a1].boolean = ctx.tbl[op.a2].real == ctx.tbl[op.a3_real].real
            endif
        // ...
        else
            // handle derived type (except the other few types)
            set ctx.tbl[op.a1].boolean = ctx.tbl[op.a2].fogstate == ctx.tbl[op.a3_integer].fogstate
        endif
        
    elseif t == Neq then
        if op.type == TypeInteger then
            if lit then
                set ctx.tbl[op.a1].boolean = ctx.tbl[op.a2].integer != op.a3_integer
            else
                set ctx.tbl[op.a1].boolean = ctx.tbl[op.a2].integer != ctx.tbl[op.a3_integer].integer
            endif
        elseif op.type == TypeReal then
            if lit then
                set ctx.tbl[op.a1].boolean = ctx.tbl[op.a2].real != op.a3_real
            else
                set ctx.tbl[op.a1].boolean = ctx.tbl[op.a2].real != ctx.tbl[op.a3_real].real
            endif
                // ...
        else
            // handle derived type (except the other few types)
            set ctx.tbl[op.a1].boolean = ctx.tbl[op.a2].fogstate != ctx.tbl[op.a3_integer].fogstate
        endif
        endif
        
    elseif t == Add then
        if op.type == TypeInteger then
            if lit then
                set ctx.tbl[op.a1].integer = ctx.tbl[op.a2].integer + op.a3_integer
            else
                set ctx.tbl[op.a1].integer = ctx.tbl[op.a2].integer + ctx.tbl[op.a3_integer].integer
            endif
        else
            if lit then
                set ctx.tbl[op.a1].real = ctx.tbl[op.a2].real + op.a3_real
            else
                set ctx.tbl[op.a1].real = ctx.tbl[op.a2].real + ctx.tbl[op.a3_real].real
            endif
        endif
        
    elseif t == Sub then
        if op.type == TypeInteger then
            if lit then
                set ctx.tbl[op.a1].integer = ctx.tbl[op.a2].integer - op.a3_integer
            else
                set ctx.tbl[op.a1].integer = ctx.tbl[op.a2].integer - ctx.tbl[op.a3_integer].integer
            endif
        else
            if lit then
                set ctx.tbl[op.a1].real = ctx.tbl[op.a2].real - op.a3_real
            else
                set ctx.tbl[op.a1].real = ctx.tbl[op.a2].real - ctx.tbl[op.a3_real].real
            endif
        endif
        
    elseif t == Mul then
        if op.type == TypeInteger then
            if lit then
                set ctx.tbl[op.a1].integer = ctx.tbl[op.a2].integer * op.a3_integer
            else
                set ctx.tbl[op.a1].integer = ctx.tbl[op.a2].integer * ctx.tbl[op.a3_integer].integer
            endif
        else
            if lit then
                set ctx.tbl[op.a1].real = ctx.tbl[op.a2].real * op.a3_real
            else
                set ctx.tbl[op.a1].real = ctx.tbl[op.a2].real * ctx.tbl[op.a3_real].real
            endif
        endif
        
    elseif t == Div then
        if op.type == TypeInteger then
            if lit then
                set ctx.tbl[op.a1].integer = ctx.tbl[op.a2].integer / op.a3_integer
            else
                set ctx.tbl[op.a1].integer = ctx.tbl[op.a2].integer / ctx.tbl[op.a3_integer].integer
            endif
        else
            if lit then
                set ctx.tbl[op.a1].real = ctx.tbl[op.a2].real / op.a3_real
            else
                set ctx.tbl[op.a1].real = ctx.tbl[op.a2].real / ctx.tbl[op.a3_real].real
            endif
        endif
        
    elseif t == NOT then
        set ctx.tbl[op.a1].boolean = not ctx.tbl[op.a2].boolean
        
    elseif t == Label then
        // do nothing
        
    elseif t == Jmp then
        set ctx.pc = ctx.labels[op.a1].next
        return ctx
        
    elseif t == JmpT then
        if ctx.tbl[op.a1].boolean then
            set ctx.pc = ctx.labels[op.a1].next
            return ctx
        endif
        
    elseif t == Ret then
        if ctx.parent.pc.type == TypeInteger then
            set ctx.parent.tbl[ctx.parent.pc.a1].integer = ctx.tbl[0].integer
        // etc.
        endif
        call FlushChildHashtable(tbl, ctx)
        if ctx.auxLocals != 0 then
            call freeAuxLocals(ctx.auxLocals)
        endif
        return ctx.parent.next
        
    elseif t == i2r then
        set ctx.tbl[op.a1].real = ctx.tbl[op.a2].integer
        
    elseif t == Call then
        if op.a2 < 0 then
            // auto generated call for natives/BJ-functions
        else
            // user-defined function
            set Context.fresh.parent = ctx
            set Context.fresh.pc = calls[op.a2]
            set tmp = Context.fresh
            set Context.fresh = Context.create()
            return tmp
        endif

    elseif t == Bind then
        if op.type == TypeInteger then
            if lit then
                set Context.fresh.tbl[op.a1].integer = op.a3_integer
            else
                set Context.fresh.tbl[op.a3_integer].integer = ctx.tbl[op.a3_integer].integer
            endif
        elseif op.type == TypeReal then
            if lit then
                set Context.fresh.tbl[op.a1].real = op.a3_real
            else
                set Context.fresh.tbl[op.a3_integer].real = ctx.tbl[op.a3_integer].real
            endif
        // ...
        endif
    elseif t == SetGlobal then
    elseif t == GetGlobal then
    elseif t == SetGlobalArr then
    elseif t == GetGlobalArr then
    endif
    
    set ctx.pc = ctx.pc.next
    return ctx
endfunction

/*

// typed ops
lt le gt ge neg eq
add sub
mul div
call
bind
set
set[] get[]
setglobal getglobal
setglobal[] getglobal[]


// untyped ops
not
label
jmpt
jmp
i2r
ret

//

// arity of 2
set integer t v1
set integer t literal 3

setglobal integer g1 v1
setglobal integer g1 literal 3

getglobal integer t g1

bind integer a1 v1
bind string a2 literal foo bar

call integer t c1

// arity of 3

lt integer t v1 v2
lt integer t v1 literal 0

mul integer t v1 v2
mul integer t v3 literal 3

set[] integer v1 v2 v3        // v1[v2] = v3
set[] integer v1 v2 literal 3 // v1[v2] = 3

get[] integer t v1 v2         // g1 = v1
get[] integer t v1 literal 3  // g1 = 3


setglobal[] integer g1 v1 v2        // g1[v1] = v2
setglobal[] integer g1 v1 literal 3 // g1[v1] = 3

getglobal[] integer t g1 v1         // t = g1[v1]
getglobal[] integer t g1 literal 3  // t = g1[3]


// untyped

// arity of 1

jmp l1
label l1 // label 3

// arity of 2

jmpt v1 l1 // jmpt 1 3 // jump to label 3 if variable 1 is true
not t v1
i2r t v1


*/
