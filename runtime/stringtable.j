// scope FunTable

globals
    #include "alloc-globals.j"
    
    // struct node
    //   node next
    //   integer value
    //   string key
    integer array _node_next
    integer array _node_value
    string array _node_key

    hashtable _ht
endglobals

#include "alloc.j"

function _create takes string name, integer value, integer next returns integer
    local integer node = _alloc()
    set _node_next[node] = next
    set _node_value[node] = value
    set _node_key[node] = name
    return node
endfunction

function _lookup takes string name returns integer
    local integer node = LoadInteger(_ht, StringHash(name), 0)
    loop
    exitwhen _node_key[node] == name
        if _node_next[node] == 0 then
            return 0
        endif
        set node = _node_next[node]
    endloop
    return _node_value[node]
endfunction

function _insert takes string name, integer value returns nothing
    local integer node = _lookup(name)
    if node == 0 then
        call SaveInteger(_ht, StringHash(name), 0, _create(name, value, node))
    else
        set _node_value[node] = value
    endif
endfunction
