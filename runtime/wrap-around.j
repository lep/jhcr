// scope Wrap

globals
    code _ref
    force _f
    integer _p
endglobals

function _call_anything_around takes integer i returns nothing
    set _p = i
    call ForForce(_f, _ref)
endfunction

function _init takes nothing returns nothing
    set _f = CreateForce()
    call ForceAddPlayer(_f, Player(0))
endfunction
