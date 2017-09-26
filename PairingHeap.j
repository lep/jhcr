library PairingHeap

private keyword node

private function f_isEmpty takes heap h returns boolean
    return h.root == 0
endfunction

private function f_min takes heap h returns integer
    return h.root.value
endfunction

private function f_merge takes node a, node b returns node
    if a == 0 then
        return b
    elseif b == 0 then
        return a
    elseif a.key >= b.key then
        set a.next = b.child
        set b.child = a
        return b
    else
        set b.next = a.child
        set a.child = b
        return a
    endif
endfunction


private function f_singleton takes real k, integer v returns node
    local node new = node.create()
    set new.key = k
    set new.value = v
    set new.next = 0
    set new.child = 0
    return new
endfunction

private function f_insert takes heap h, real k, integer v returns nothing
    set h.root = f_merge(h.root, f_singleton(k, v))
endfunction


private function f_mergepairs takes node n returns node
    local node e2 = n.next
    local node e3 = e2.next
    
    if n == 0 then
        return 0
    elseif e2 == 0 then
        return n
    else
        return f_merge(f_merge(n, e2), f_mergepairs(e3))
    endif
endfunction

private function f_deleteMin takes heap h returns integer
    local integer v = h.root.value
    local node c = h.root
    set h.root = f_mergepairs(h.root.child)
    if c != 0 then
        call c.destroy()
    endif
    return v
endfunction

private struct node
    real key
    integer value
    node next
    node child
endstruct


struct heap
    node root = 0
    
    method isEmpty takes nothing returns boolean
        return f_isEmpty(this)
    endmethod
    
    method min takes nothing returns integer
        return f_min(this)
    endmethod
    
    method deleteMin takes nothing returns integer
        return f_deleteMin(this)
    endmethod
    
    method add takes real k, integer v returns nothing
        call f_insert(this, k, v)
    endmethod
    
    method merge takes heap other returns nothing
        set this.root = f_merge(this.root, other.root)
        call other.destroy()
    endmethod
endstruct

endlibrary