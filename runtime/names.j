// scope Names

globals
    // Table<Int, StringTable<Int>>
    integer _type_table
    
    // StringTable<Int>
    integer _fun_table
endglobals

function _insert_global takes integer ty, string name, integer uid returns nothing
    local integer tmp = Table#_get_integer(_type_table, ty)
    if tmp == 0 then
        set tmp = StringTable#_alloc()
        call Table#_set_integer(_type_table, ty, tmp)
    endif
    call StringTable#_insert(tmp, name, uid)
endfunction

function _insert_function takes string name, integer uid returns nothing
    call StringTable#_insert(_fun_table, name, uid)
endfunction

function _get_function takes string name returns integer
    return StringTable#_lookup(_fun_table, name)
endfunction

function _init takes nothing returns nothing
    set _type_table = Table#_alloc()
    set _fun_table = StringTable#_alloc()
endfunction
