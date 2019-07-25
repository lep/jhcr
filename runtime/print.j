// scope Print
// REQUIRES 

function _b2s takes boolean b returns string
    if b then
        return "True"
    else
        return "False"
    endif
endfunction

function _log takes string s returns nothing
    call Preload(s)
endfunction

function _print takes string s returns nothing
    call DisplayTimedTextToPlayer(Player(0), 0, 0, 60, s)
endfunction