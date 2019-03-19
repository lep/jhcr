
function _alloc takes nothing returns integer
    local integer _this = _F
    if _this != 0 then
        set _F = _V[_this]
    else
        set _I = _I+1
        set _this = _I
    endif
    
    if _this >= JASS_MAX_ARRAY_SIZE then
        call Print#_print("no more free instances " + "__BASE_FILE__")
        return 0
    endif

    set _V[_this] = -1
    return _this
endfunction

function _free takes integer _this returns nothing
    if _this == 0 then
        call Print#_print("free of nullptr " + "__BASE_FILE__")
        return
    elseif _V[_this] != -1 then
        call Print#_print("Double free in " + "__BASE_FILE__")
        return
    endif
    set _V[_this] = _F
    set _F = _this
endfunction
