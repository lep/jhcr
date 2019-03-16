// scope Init

function _parse takes nothing returns nothing
    //call BJDebugMsg("esc")
    call Preloader("JHCR.txt")
    //call BJDebugMsg(BlzGetAbilityTooltip('Amls', 1))
    call JHCR_Parser_parse_and_init(BlzGetAbilityTooltip('Amls', 1))
    //call JHCR_Parser_parse_and_init(GetPlayerName(Player(0)))
    //call JHCR_Parser_parse_and_init(GetPlayerName(Player(1)))
    //call JHCR_Parser_parse_and_init(GetPlayerName(Player(2)))
    //call BJDebugMsg("esc2")
endfunction


function _init takes nothing returns nothing
    local trigger t = CreateTrigger()
    call TriggerRegisterPlayerEventEndCinematic( t, Player(0) )
    call TriggerAddAction( t, function _parse )
    
    call Convert#_init()
    call Ins#_init()
    call Interpreter#_init()
    call Modified#_init()
endfunction