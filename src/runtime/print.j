// scope Print
// REQUIRES 

function _b2s takes boolean _b returns string
    if _b then
        return "True"
    else
        return "False"
    endif
endfunction

function _log takes string _s returns nothing
    call Preload(_s)
endfunction

function _error takes string _s returns nothing
    local integer _i
    call DisplayTimedTextToPlayer(Player(0), 0, 0, 60, "Error: "+_s)
    set _i = 1/0
endfunction

function _print takes string _s returns nothing
    call DisplayTimedTextToPlayer(Player(0), 0, 0, 60, _s)
endfunction