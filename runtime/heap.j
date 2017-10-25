
globals

    // struct node
    real array _node_key
    integer array _node_value
    integer array _node_next
    integer array _node_child

    // struct heap
    integer array _heap_root

endglobals

function _node_allocate takes nothing returns integer
    return 0
endfunction

function _node_free takes integer node returns nothing
endfunction

function _heap_allocate takes nothing returns integer
    return 0
endfunction

function _heap_free takes integer heap returns nothing
endfunction

function _heap_isEmpty takes integer heap returns boolean
    return _heap_root[heap] == 0
endfunction

function _heap_min takes integer heap returns integer
    return _node_value[_heap_root[heap]]
endfunction

function _heap_merge takes integer node1, integer node2 returns integer
    if node1 == 0 then
        return node2
    elseif node2 == 0 then
        return node1
    elseif _node_key[node1] >= _node_key[b] then
        set _node_next[node1] = _node_child[node2]
        set _node_child[node2] = node1
        return node2
    else
        set _node_next[node2] = _node_child[node1]
        set _node_child[node1] = node2
        return node1
    endif
endfunction

function _heap_singleton takes real k, integer v returns integer
    local integer node = _node_allocate()
    set _node_key[node] = k
    set _node_value[node] = v
    set _node_next[node] = 0
    set _node_child[node] = 0
    return node
endfunction

function _heap_insert takes integer heap, real k, integer v returns nothing
    set _heap_root[heap] = _heap_merge(_heap_root[heap], _heap_singleton(k, v))
endfunction

function _heap_merge_pairs takes integer node returns integer
    local integer node2 = _node_next[node]
    local integer node3 = _node_next[node2]

    if node == 0 then
        return 0
    elseif node2 == 0 then
        return node
    else
        return _heap_merge(_heap_merge(node, node2), _heap_merge_pairs(node3))
    endif
endfunction

function _heap_deleteMin takes integer heap returns integer
    local integer v = _node_value[_heap_root[heap]]
    local integer node = _heap_root[heap]
    set _heap_root[heap] = _heap_merge_pairs(_node_child[_heap_root[heap]])
    if node != 0 then
        call _node_free(node)
    endif
    return v
endfunction

function _heap_merge takes integer heap1, integer heap2 returns nothing
    set _heap_root[heap1] = _heap_merge(_heap_root[heap1], _heap_root[heap2])
    call _heap_free(heap2)
endfunction
