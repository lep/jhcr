// scope StringTable
// REQUIRES Table

globals
    string array _key
    integer array _value
endglobals

function _set takes integer _tbl, string _k, integer _v returns nothing
    local integer _lst = Table#_get_integer(_tbl, StringHash(_k))
    local integer _tmp = _lst
    loop
    exitwhen _tmp == 0
        if _key[_tmp] == _k then
            set _value[_tmp] = _v
            return
        endif
        set _tmp = List#_next[_tmp]
    endloop
    // either _lst was 0 in the first place or no element was found in the list
    set _lst = List#_cons(_lst)
    set _key[_lst] = _k
    set _value[_lst] = _v
    call Table#_set_integer(_tbl, StringHash(_k), _lst)
endfunction

function _get takes integer _tbl, string _k returns integer
    local integer _lst = Table#_get_integer(_tbl, StringHash(_k))
    loop
    exitwhen _lst == 0
        if _key[_lst] == _k then
            return _value[_lst]
        endif
        set _lst = List#_next[_lst]
    endloop
    return 0
endfunction