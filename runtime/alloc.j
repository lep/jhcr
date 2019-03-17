
function _alloc takes nothing returns integer
    local integer this = _F
    if this != 0 then
        set _F = _V[this]
    else
        set _I = _I+1
        set this = _I
    endif
    
    if this >= JASS_MAX_ARRAY_SIZE then
        call BJDebugMsg("no more free instances " + "__BASE_FILE__")
        return 0
    endif

    set _V[this] = -1
    return this
endfunction

function _free takes integer this returns nothing
    if this == 0 then
        call BJDebugMsg("free of nullptr " + "__BASE_FILE__")
        return
    elseif _V[this] != -1 then
        call BJDebugMsg("Double free in " + "__BASE_FILE__")
        return
    endif
    set _V[this] = _F
    set _F = this
endfunction
