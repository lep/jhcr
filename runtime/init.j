// scope Init

globals
    integer array _ids
    constant integer _max = 24
endglobals

function _parse takes nothing returns nothing
    local integer _cnt = _max
    local string array _tmp
    
    loop
    exitwhen _cnt == 0
        set _tmp[_cnt] = BlzGetAbilityTooltip(_ids[_cnt], 1)
        set _cnt = _cnt -1
    endloop

    call Preloader("JHCR.txt")
    set _cnt = GetPlayerTechMaxAllowed(Player(0), 1) 
    loop
    exitwhen _cnt == 0
        call Parser#_parse_and_init(BlzGetAbilityTooltip(_ids[_cnt], 1))
        call BlzSetAbilityTooltip(_ids[_cnt], _tmp[_cnt], 1)
        set _cnt = _cnt -1
    endloop
endfunction

function _i2code takes nothing returns nothing
    set Wrap#_ret = Auto#_i2code(Wrap#_p)
endfunction


function _init takes nothing returns nothing

    // "Agyv", "Aflk", "Agyb", "Ahea", "Ainf", "Aslo", "Afla", "Amls", "Adis", "Acmg", "Amdf", "Adts"
    set _ids[1] = 'Agyv'
    set _ids[2] = 'Aflk'
    set _ids[3] = 'Agyb'
    set _ids[4] = 'Ahea'
    set _ids[5] = 'Ainf'
    set _ids[6] = 'Aslo'
    set _ids[7] = 'Afla'
    set _ids[8] = 'Amls'
    set _ids[9] = 'Adis'
    set _ids[10] = 'Acmg'
    set _ids[11] = 'Amdf'
    set _ids[12] = 'Adts'
    set _ids[13] = 'Aast'
    set _ids[14] = 'Aetf'
    set _ids[15] = 'Absk'
    set _ids[16] = 'Alsh'
    set _ids[17] = 'Aens'
    set _ids[18] = 'Adcn'
    set _ids[19] = 'Aliq'
    set _ids[20] = 'Aspl'
    set _ids[21] = 'Aven'
    set _ids[22] = 'Ablo'
    set _ids[23] = 'Acpf'
    set _ids[24] = 'Awar'
    
    
    call TriggerAddCondition(Wrap#_t2, Condition(function _i2code))

    call Wrap#_init()
    call Convert#_init()
    call Ins#_init()
    call Interpreter#_init()
    call Modified#_init()
endfunction