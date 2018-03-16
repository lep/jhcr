// scope Heap

globals

    // struct node
    //   real key
    //   node next
    //   node child
    real array _node_key
    integer array _node_value
    integer array _node_next
    integer array _node_child

    //  struct heap
    //    node root
    integer array _root

endglobals

function _isEmpty takes integer heap returns boolean
    return _root[heap] == 0
endfunction

function _min takes integer heap returns integer
    return _node_value[_root[heap]]
endfunction

function _merge takes integer node1, integer node2 returns integer
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

function _singleton takes real k, integer v returns integer
    local integer node = _allocate() // node allocate
    set _node_key[node] = k
    set _node_value[node] = v
    set _node_next[node] = 0
    set _node_child[node] = 0
    return node
endfunction

function _insert takes integer heap, real k, integer v returns nothing
    set _root[heap] = _merge(_root[heap], _singleton(k, v))
endfunction

function _merge_pairs takes integer node returns integer
    local integer node2 = _node_next[node]
    local integer node3 = _node_next[node2]

    if node == 0 then
        return 0
    elseif node2 == 0 then
        return node
    else
        return _merge(_merge(node, node2), _merge_pairs(node3))
    endif
endfunction

function _deleteMin takes integer heap returns integer
    local integer v = _node_value[_root[heap]]
    local integer node = _root[heap]
    set _root[heap] = _merge_pairs(_node_child[_root[heap]])
    if node != 0 then
        call _free(node) // node free
    endif
    return v
endfunction

function _merge takes integer heap1, integer heap2 returns nothing
    set _root[heap1] = _merge(_root[heap1], _root[heap2])
    call _free(heap2) // heap free
endfunction
