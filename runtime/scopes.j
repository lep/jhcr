// scope Scopes

globals
    integer _binding = 0
    integer _scope = 0
endglobals

function _init takes nothing returns nothing
    set _binding = Table#_alloc()
    set _scope = Table#_alloc()
endfunction
