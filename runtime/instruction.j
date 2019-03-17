// scope Ins

globals
  
    constant integer _Lt = 1
    constant integer _Le = 2
    constant integer _Gt = 3
    constant integer _Ge = 4
    constant integer _Eq = 5
    constant integer _Neq = 6
    
    constant integer _Add = 7
    constant integer _Sub = 8
    constant integer _Mul = 9
    constant integer _Div = 10
    constant integer _Mod = 11
    
    constant integer _SetLocalArray = 12
    constant integer _GetLocalArray = 13
    constant integer _SetGlobalArray = 14
    constant integer _GetGlobalArray = 15
    
    constant integer _Negate = 16
    constant integer _Set = 17
    constant integer _SetGlobal = 18
    constant integer _GetGlobal = 19
    constant integer _Bind = 20
    
    constant integer _Lit = 21
    constant integer _Call = 22
    constant integer _Convert = 23
 

    constant integer _Label = 24
    constant integer _Jmp = 25
    constant integer _Fun = 26
    constant integer _JmpT = 27
    
    constant integer _Not = 28
    
    constant integer _Ret = 29
    
    
    
    string array _OpNames
    string array _TypeNames


    #include "alloc-globals.j"
    
    // struct Instruction
    //   Instruction next
    //   integer op
    //   integer type
    //   integer a1
    //   integer a2
    //   integer a3
    //   string literal
    integer array _next
    integer array _op
    integer array _type

    integer array _a1
    integer array _a2
    integer array _a3

    // literals
    string array _string
    integer array _integer
    real array _real
    boolean array _boolean

endglobals

#include "alloc.j"

function _B2S takes boolean _b returns string
    if _b then
        return "true"
    else
        return "false"
    endif
endfunction

function _print takes integer _ins returns nothing

    call BJDebugMsg(I2S(_op[_ins]) +" "+ I2S(_type[_ins]) +" "+ I2S(_a1[_ins]) +" "+ I2S(_a2[_ins]) +" "+ I2S(_a3[_ins]))


    if _op[_ins] <= Ins#_GetLocalArray then
        call BJDebugMsg(_OpNames[_op[_ins]] +" "+ _TypeNames[_type[_ins]] +" "+ I2S(_a1[_ins]) +" "+ I2S(_a2[_ins]) +" "+ I2S(_a3[_ins]))
    elseif _op[_ins] <= Ins#_Bind then
        call BJDebugMsg(_OpNames[_op[_ins]] +" "+ _TypeNames[_type[_ins]] +" "+ I2S(_a1[_ins]) +" "+ I2S(_a2[_ins]) )
    elseif _op[_ins] == Ins#_Lit then
        if Ins#_type[_ins] == Types#_string then
            call BJDebugMsg(_OpNames[_op[_ins]] +" "+ _TypeNames[_type[_ins]] +" "+ (_string[_ins]) )
        elseif Ins#_type[_ins] == Types#_integer then
            call BJDebugMsg(_OpNames[_op[_ins]] +" "+ _TypeNames[_type[_ins]] +" "+ I2S(_integer[_ins]) )
        elseif Ins#_type[_ins] == Types#_real then
            call BJDebugMsg(_OpNames[_op[_ins]] +" "+ _TypeNames[_type[_ins]]  +" "+ R2S(_real[_ins]) )
        elseif Ins#_type[_ins] == Types#_boolean then
            call BJDebugMsg(_OpNames[_op[_ins]] +" "+ _TypeNames[_type[_ins]] +" "+ _B2S(_boolean[_ins]) )
        endif
    elseif _op[_ins] == Ins#_Call then
        call BJDebugMsg(_OpNames[_op[_ins]] +" "+ _TypeNames[_type[_ins]] +" "+ I2S(_a1[_ins]) +" "+ I2S(_a2[_ins]) )
    elseif _op[_ins] == Ins#_Convert then
        call BJDebugMsg(_OpNames[_op[_ins]] +" "+ _TypeNames[_type[_ins]] +" "+ I2S(_a1[_ins]) +" "+ I2S(_a2[_ins]) +" "+ I2S(_a3[_ins]))
    elseif _op[_ins] == Ins#_Label then
        call BJDebugMsg(_OpNames[_op[_ins]] +" "+ I2S(_a1[_ins]) )
    elseif _op[_ins] == Ins#_Jmp then
        call BJDebugMsg(_OpNames[_op[_ins]] +" "+ I2S(_a1[_ins]) )
    elseif _op[_ins] == Ins#_Fun then
        call BJDebugMsg(_OpNames[_op[_ins]] +" "+ I2S(_a1[_ins]) )
    elseif _op[_ins] == Ins#_JmpT then
        call BJDebugMsg(_OpNames[_op[_ins]] +" "+ I2S(_a1[_ins]) +" "+ I2S(_a2[_ins]) )
    elseif _op[_ins] == Ins#_Not then
        call BJDebugMsg(_OpNames[_op[_ins]] +" "+ I2S(_a1[_ins]) +" "+ I2S(_a2[_ins]) )
    elseif _op[_ins] == Ins#_Ret then
        call BJDebugMsg(_OpNames[_op[_ins]] )
    else
        call BJDebugMsg("unknown op " +I2S(Ins#_op[_ins]))
    endif

    call BJDebugMsg("------------")
    
endfunction

function _init takes nothing returns nothing
    set _OpNames[_Not]="Not"
    set _OpNames[_Neq]="Neq"
    set _OpNames[_JmpT]="JmpT"
    set _OpNames[_Jmp]="Jmp"
    set _OpNames[_Lit]="Lit"
    set _OpNames[_Bind]="Bind"
    set _OpNames[_Set]="Set"
    set _OpNames[_Call]="Call"
    set _OpNames[_Add]="Add"
    set _OpNames[_Mul]="Mul"
    set _OpNames[_Div]="Div"
    set _OpNames[_Sub]="Sub"
    set _OpNames[_Negate]="Negate"
    set _OpNames[_SetLocalArray]="SetLocalArray"
    set _OpNames[_GetLocalArray]="GetLocalArray"
    set _OpNames[_SetGlobalArray]="SetGlobalArray"
    set _OpNames[_GetGlobalArray]="GetGlobalArray"
    set _OpNames[_SetGlobal]="SetGlobal"
    set _OpNames[_GetGlobal]="GetGlobal"
    set _OpNames[_Ret]="Ret"
    set _OpNames[_Label]="Label"
    set _OpNames[_Eq]="Eq"
    set _OpNames[_Lt]="Lt"
    set _OpNames[_Le]="Le"
    set _OpNames[_Ge]="Ge"
    set _OpNames[_Gt]="Gt"
    set _OpNames[_Convert]="Convert"
    set _OpNames[_Fun]="Fun"
    
    set _TypeNames[Types#_handle] = "handle"
    set _TypeNames[Types#_agent] = "agent"
    set _TypeNames[Types#_event] = "event"
    set _TypeNames[Types#_player] = "player"
    set _TypeNames[Types#_widget] = "widget"
    set _TypeNames[Types#_unit] = "unit"
    set _TypeNames[Types#_destructable] = "destructable"
    set _TypeNames[Types#_item] = "item"
    set _TypeNames[Types#_ability] = "ability"
    set _TypeNames[Types#_buff] = "buff"
    set _TypeNames[Types#_force] = "force"
    set _TypeNames[Types#_group] = "group"
    set _TypeNames[Types#_trigger] = "trigger"
    set _TypeNames[Types#_triggercondition] = "triggercondition"
    set _TypeNames[Types#_timer] = "timer"
    set _TypeNames[Types#_location] = "location"
    set _TypeNames[Types#_region] = "region"
    set _TypeNames[Types#_rect] = "rect"
    set _TypeNames[Types#_boolexpr] = "boolexpr"
    set _TypeNames[Types#_conditionfunc] = "conditionfunc"
    set _TypeNames[Types#_filterfunc] = "filterfunc"
    set _TypeNames[Types#_sound] = "sound"
    set _TypeNames[Types#_effect] = "effect"
    set _TypeNames[Types#_fogmodifier] = "fogmodifier"
    set _TypeNames[Types#_dialog] = "dialog"
    set _TypeNames[Types#_button] = "button"
    set _TypeNames[Types#_quest] = "quest"
    set _TypeNames[Types#_questitem] = "questitem"
    set _TypeNames[Types#_defeatcondition] = "defeatcondition"
    set _TypeNames[Types#_timerdialog] = "timerdialog"
    set _TypeNames[Types#_leaderboard] = "leaderboard"
    set _TypeNames[Types#_multiboard] = "multiboard"
    set _TypeNames[Types#_multiboarditem] = "multiboarditem"
    set _TypeNames[Types#_trackable] = "trackable"
    set _TypeNames[Types#_gamecache] = "gamecache"
    set _TypeNames[Types#_hashtable] = "hashtable"
    set _TypeNames[Types#_triggeraction] = "triggeraction"
    set _TypeNames[Types#_unitpool] = "unitpool"
    set _TypeNames[Types#_itempool] = "itempool"
    set _TypeNames[Types#_race] = "race"
    set _TypeNames[Types#_alliancetype] = "alliancetype"
    set _TypeNames[Types#_racepreference] = "racepreference"
    set _TypeNames[Types#_gamestate] = "gamestate"
    set _TypeNames[Types#_igamestate] = "igamestate"
    set _TypeNames[Types#_fgamestate] = "fgamestate"
    set _TypeNames[Types#_playerstate] = "playerstate"
    set _TypeNames[Types#_playerscore] = "playerscore"
    set _TypeNames[Types#_playergameresult] = "playergameresult"
    set _TypeNames[Types#_unitstate] = "unitstate"
    set _TypeNames[Types#_aidifficulty] = "aidifficulty"
    set _TypeNames[Types#_eventid] = "eventid"
    set _TypeNames[Types#_gameevent] = "gameevent"
    set _TypeNames[Types#_playerevent] = "playerevent"
    set _TypeNames[Types#_playerunitevent] = "playerunitevent"
    set _TypeNames[Types#_unitevent] = "unitevent"
    set _TypeNames[Types#_limitop] = "limitop"
    set _TypeNames[Types#_widgetevent] = "widgetevent"
    set _TypeNames[Types#_dialogevent] = "dialogevent"
    set _TypeNames[Types#_unittype] = "unittype"
    set _TypeNames[Types#_gamespeed] = "gamespeed"
    set _TypeNames[Types#_gamedifficulty] = "gamedifficulty"
    set _TypeNames[Types#_gametype] = "gametype"
    set _TypeNames[Types#_mapflag] = "mapflag"
    set _TypeNames[Types#_mapvisibility] = "mapvisibility"
    set _TypeNames[Types#_mapsetting] = "mapsetting"
    set _TypeNames[Types#_mapdensity] = "mapdensity"
    set _TypeNames[Types#_mapcontrol] = "mapcontrol"
    set _TypeNames[Types#_playerslotstate] = "playerslotstate"
    set _TypeNames[Types#_volumegroup] = "volumegroup"
    set _TypeNames[Types#_camerafield] = "camerafield"
    set _TypeNames[Types#_camerasetup] = "camerasetup"
    set _TypeNames[Types#_playercolor] = "playercolor"
    set _TypeNames[Types#_placement] = "placement"
    set _TypeNames[Types#_startlocprio] = "startlocprio"
    set _TypeNames[Types#_raritycontrol] = "raritycontrol"
    set _TypeNames[Types#_blendmode] = "blendmode"
    set _TypeNames[Types#_texmapflags] = "texmapflags"
    set _TypeNames[Types#_effecttype] = "effecttype"
    set _TypeNames[Types#_weathereffect] = "weathereffect"
    set _TypeNames[Types#_terraindeformation] = "terraindeformation"
    set _TypeNames[Types#_fogstate] = "fogstate"
    set _TypeNames[Types#_version] = "version"
    set _TypeNames[Types#_itemtype] = "itemtype"
    set _TypeNames[Types#_texttag] = "texttag"
    set _TypeNames[Types#_attacktype] = "attacktype"
    set _TypeNames[Types#_damagetype] = "damagetype"
    set _TypeNames[Types#_weapontype] = "weapontype"
    set _TypeNames[Types#_soundtype] = "soundtype"
    set _TypeNames[Types#_lightning] = "lightning"
    set _TypeNames[Types#_pathingtype] = "pathingtype"
    set _TypeNames[Types#_mousebuttontype] = "mousebuttontype"
    set _TypeNames[Types#_animtype] = "animtype"
    set _TypeNames[Types#_subanimtype] = "subanimtype"
    set _TypeNames[Types#_image] = "image"
    set _TypeNames[Types#_ubersplat] = "ubersplat"
    set _TypeNames[Types#_real] = "real"
    set _TypeNames[Types#_integer] = "integer"
    set _TypeNames[Types#_string] = "string"
    set _TypeNames[Types#_boolean] = "boolean"
    set _TypeNames[Types#_code] = "code"
    set _TypeNames[Types#_nothing] = "nothing"

endfunction
