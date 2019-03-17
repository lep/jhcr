// scope Init

globals
    integer array _ids
    constant integer _max = 6
endglobals

function _parse takes nothing returns nothing
    local integer cnt = _max
    local string array tmp
    
    call BJDebugMsg("esc")
    
    loop
    exitwhen cnt == 0
        set tmp[cnt] = BlzGetAbilityTooltip(_ids[cnt], 1)
        set cnt = cnt -1
    endloop

    call Preloader("JHCR.txt")
    set cnt = GetPlayerTechMaxAllowed(Player(0), 1) 
    loop
    exitwhen cnt == 0
        call JHCR_Parser_parse_and_init(BlzGetAbilityTooltip(_ids[cnt], 1))
        call BlzSetAbilityTooltip(_ids[cnt], tmp[cnt], 1)
        set cnt = cnt -1
    endloop
endfunction


function _init takes nothing returns nothing
    local trigger t = CreateTrigger()
    call TriggerRegisterPlayerEventEndCinematic( t, Player(0) )
    call TriggerAddAction( t, function _parse )
    
    // "Agyv", "Aflk", "Agyb", "Ahea", "Ainf", "Aslo", "Afla", "Amls", "Adis", "Acmg", "Amdf", "Adts"
    set _ids[1] = 'Agyv'
    set _ids[2] = 'Aflk'
    set _ids[3] = 'Agyb'
    set _ids[4] = 'Ahea'
    set _ids[5] = 'Ainf'
    set _ids[6] = 'Aslo'
    set _ids[6] = 'Afla'
    set _ids[6] = 'Amls'
    set _ids[6] = 'Adis'
    set _ids[6] = 'Acmg'
    set _ids[6] = 'Amdf'
    set _ids[6] = 'Adts'

    call Wrap#_init()
    call Convert#_init()
    call Ins#_init()
    call Interpreter#_init()
    call Modified#_init()
endfunction