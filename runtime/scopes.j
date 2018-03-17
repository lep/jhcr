// scope Scopes

globals
    integer _binding
    integer _scope
endglobals

function _init takes nothing returns nothing
    set _binding = Table#_alloc()
    set _scope = Table#_alloc()
endfunction
