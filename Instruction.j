
globals
    Context fresh
endglobals

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


struct Context
    Table tbl = Table.create()
    Table lables = Table.create()

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
                set ctx.tbl.integer[op.a1] = op.a3_integer
            else
                set ctx.tbl.integer[op.a1] = ctx.tbl.integer[op.a3_integer]
            endif
        elseif op.type == TypeReal then
            if lit then
                set ctx.tbl.real[op.a1] = op.a3_real
            else
                set ctx.tbl.real[op.a1] = ctx.tbl.real[op.a3_integer]
            endif
        elseif op.type == TypeBoolean then
            if lit then
                set ctx.tbl.boolean[op.a1] = op.a3_boolean
            else
                set ctx.tbl.boolean[op.a1] = ctx.tbl.boolean[op.a3_integer]
            endif
        elseif op.type == TypeString then
            if lit then
                set ctx.tbl.string[op.a1] = op.a3_string
            else
                set ctx.tbl.string[op.a1] = ctx.tbl.string[op.a3_integer]
            endif
        elseif op.type == TypeCode then
            if lit then
                set ctx.tbl.integer[op.a1] = op.a3_integer
            else
                set ctx.tbl.integer[op.a1] = ctx.tbl.integer[op.a3_integer]
            endif
            
            // remember: handle literal can only be null
        endif
        
    elseif t == SetArr then
    elseif t == GetArr then
    
    elseif t == Lt then
        if op.type == TypeInteger then
            if lit then
                set ctx.tbl.boolean[op.a1] = ctx.tbl.integer[op.a2] < op.a3_integer
            else
                set ctx.tbl.boolean[op.a1] = ctx.tbl.integer[op.a2] < ctx.tbl.integer[op.a3_integer]
            endif
        else
            if lit then
                set ctx.tbl.boolean[op.a1] = ctx.tbl.real[op.a2] < op.a3_real
            else
                set ctx.tbl.boolean[op.a1] = ctx.tbl.real[op.a2] < ctx.tbl.real[op.a3_integer]
            endif
        endif
        
    elseif t == Le then
        if op.type == TypeInteger then
            if lit then
                set ctx.tbl.boolean[op.a1] = ctx.tbl.integer[op.a2] <= op.a3_integer
            else
                set ctx.tbl.boolean[op.a1] = ctx.tbl.integer[op.a2] <= ctx.tbl.integer[op.a3_integer]
            endif
        else
            if lit then
                set ctx.tbl.boolean[op.a1] = ctx.tbl.real[op.a2] <= op.a3_real
            else
                set ctx.tbl.boolean[op.a1] = ctx.tbl.real[op.a2] <= ctx.tbl.real[op.a3_integer]
            endif
        endif
        
    elseif t == Gt then
        if op.type == TypeInteger then
            if lit then
                set ctx.tbl.boolean[op.a1] = ctx.tbl.integer[op.a2] > op.a3_integer
            else
                set ctx.tbl.boolean[op.a1] = ctx.tbl.integer[op.a2] > ctx.tbl.integer[op.a3_integer]
            endif
        else
            if lit then
                set ctx.tbl.boolean[op.a1] = ctx.tbl.real[op.a2] > op.a3_real
            else
                set ctx.tbl.boolean[op.a1] = ctx.tbl.real[op.a2] > ctx.tbl.real[op.a3_integer]
            endif
        endif
        
    elseif t == Ge then
        if op.type == TypeInteger then
            if lit then
                set ctx.tbl.boolean[op.a1] = ctx.tbl.integer[op.a2] >= op.a3_integer
            else
                set ctx.tbl.boolean[op.a1] = ctx.tbl.integer[op.a2] >= ctx.tbl.integer[op.a3_integer]
            endif
        else
            if lit then
                set ctx.tbl.boolean[op.a1] = ctx.tbl.real[op.a2] >= op.a3_real
            else
                set ctx.tbl.boolean[op.a1] = ctx.tbl.real[op.a2] >= ctx.tbl.real[op.a3_integer]
            endif
        endif
        
    elseif t == Eq then
        if op.type == TypeInteger then
            if lit then
                set ctx.tbl.boolean[op.a1] = ctx.tbl.integer[op.a2] == op.a3_integer
            else
                set ctx.tbl.boolean[op.a1] = ctx.tbl.integer[op.a2] == ctx.tbl.integer[op.a3_integer]
            endif
        elseif op.type == TypeReal then
            if lit then
                set ctx.tbl.boolean[op.a1] = ctx.tbl.real[op.a2] == op.a3_real
            else
                set ctx.tbl.boolean[op.a1] = ctx.tbl.real[op.a2] == ctx.tbl.real[op.a3_integer]
            endif
        // ...
        else
            // handle derived type (except the other few types)
            set ctx.tbl.boolean[op.a1] = ctx.tbl.fogstate[op.a2] == ctx.tbl.fogstate[op.a3_integer]
        endif
        
    elseif t == Neq then
        if op.type == TypeInteger then
            if lit then
                set ctx.tbl.boolean[op.a1] = ctx.tbl.integer[op.a2] != op.a3_integer
            else
                set ctx.tbl.boolean[op.a1] = ctx.tbl.integer[op.a2] != ctx.tbl.integer[op.a3_integer]
            endif
        elseif op.type == TypeReal then
            if lit then
                set ctx.tbl.boolean[op.a1] = ctx.tbl.real[op.a2] != op.a3_real
            else
                set ctx.tbl.boolean[op.a1] = ctx.tbl.real[op.a2] != ctx.tbl.real[op.a3_integer]
            endif
                // ...
        else
            // handle derived type (except the other few types)
            set ctx.tbl.boolean[op.a1] = ctx.tbl.fogstate[op.a2] != ctx.tbl.fogstate[op.a3_integer]
        endif
        
    elseif t == Add then
        if op.type == TypeInteger then
            if lit then
                set ctx.tbl.integer[op.a1] = ctx.tbl.integer[op.a2] + op.a3_integer
            else
                set ctx.tbl.integer[op.a1] = ctx.tbl.integer[op.a2] + ctx.tbl.integer[op.a3_integer]
            endif
        else
            if lit then
                set ctx.tbl.real[op.a1] = ctx.tbl.real[op.a2] + op.a3_real
            else
                set ctx.tbl.real[op.a1] = ctx.tbl.real[op.a2] + ctx.tbl.real[op.a3_integer]
            endif
        endif
        
    elseif t == Sub then
        if op.type == TypeInteger then
            if lit then
                set ctx.tbl.integer[op.a1] = ctx.tbl.integer[op.a2] - op.a3_integer
            else
                set ctx.tbl.integer[op.a1] = ctx.tbl.integer[op.a2] - ctx.tbl.integer[op.a3_integer]
            endif
        else
            if lit then
                set ctx.tbl.real[op.a1] = ctx.tbl.real[op.a2] - op.a3_real
            else
                set ctx.tbl.real[op.a1] = ctx.tbl.real[op.a2] - ctx.tbl.real[op.a3_real]
            endif
        endif
        
    elseif t == Mul then
        if op.type == TypeInteger then
            if lit then
                set ctx.tbl.integer[op.a1] = ctx.tbl.integer[op.a2] * op.a3_integer
            else
                set ctx.tbl.integer[op.a1] = ctx.tbl.integer[op.a2] * ctx.tbl.integer[op.a3_integer]
            endif
        else
            if lit then
                set ctx.tbl.real[op.a1] = ctx.tbl.real[op.a2] * op.a3_real
            else
                set ctx.tbl.real[op.a1] = ctx.tbl.real[op.a2] * ctx.tbl.real[op.a3_integer]
            endif
        endif
        
    elseif t == Div then
        if op.type == TypeInteger then
            if lit then
                set ctx.tbl.integer[op.a1] = ctx.tbl.integer[op.a2] / op.a3_integer
            else
                set ctx.tbl.integer[op.a1] = ctx.tbl.integer[op.a2] / ctx.tbl.integer[op.a3_integer]
            endif
        else
            if lit then
                set ctx.tbl.real[op.a1] = ctx.tbl.real[op.a2] / op.a3_real
            else
                set ctx.tbl.real[op.a1] = ctx.tbl.real[op.a2] / ctx.tbl.real[op.a3_integer]
            endif
        endif
        
    elseif t == NOT then
        set ctx.tbl.boolean[op.a1] = not ctx.tbl.boolean[op.a2]
        
    elseif t == Label then
        // do nothing
        
    elseif t == Jmp then
        set ctx.pc = ctx.labels[op.a1].next
        return ctx
        
    elseif t == JmpT then
        if ctx.tbl.boolean[op.a1] then
            set ctx.pc = ctx.labels[op.a1].next
            return ctx
        endif
        
    elseif t == Ret then
        if ctx.parent.pc.type == TypeInteger then
            set ctx.parent.tbl.integer[ctx.parent.pc.a1] = ctx.tbl.integer[0]
        // etc.
        endif
        call ctx.tbl.clean()
        call ctx.destroy()
        return ctx.parent.next
        
    elseif t == i2r then
        set ctx.tbl.real[op.a1] = ctx.tbl.integer[op.a2]
        
    elseif t == Call then
        if op.a2 < 0 then
            // auto generated call for natives/BJ-functions
        else
            // user-defined function
            set fresh.parent = ctx
            set fresh.pc = calls[op.a2]
            set tmp = fresh
            set fresh = Context.create()
            return tmp
        endif

    elseif t == Bind then // should be same as Set except different target table
        if op.type == TypeInteger then
            if lit then
                set fresh.tbl.integer[op.a1] = op.a3_integer
            else
                set fresh.tbl.integer[op.a3_integer] = ctx.tbl.integer[op.a3_integer]
            endif
        elseif op.type == TypeReal then
            if lit then
                set fresh.tbl.real[op.a1] = op.a3_real
            else
                set fresh.tbl.real[op.a3_integer] = ctx.tbl.real[op.a3_integer]
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

x2h / cast ?


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
