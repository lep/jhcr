// scope Init

function _init takes nothing returns nothing
    call Convert#_init()
    call Ins#_init()
    call Interpreter#_init()
    call Modified#_init()
    call Scopes#_init()
endfunction