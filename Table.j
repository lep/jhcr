library Table

globals
    private hashtable ht = InitHashtable()
endglobals

private struct node
    node next = 0
    
    agent agentE
    buff buffE
    camerasetup camerasetupE
    conditionfunc conditionfuncE
    eventid eventidE
    filterfunc filterfuncE
    gamecache gamecacheE
    gamestate gamestateE
    handle handleE
    terraindeformation terraindeformationE
    weathereffect weathereffectE
endstruct

//! textmacro BASIC_SAVEABLE takes NAME, TYPE
private struct $TYPE$s extends array
    method operator [] takes integer key returns $TYPE$
        return Load$NAME$(ht, this, key)
    endmethod
    method operator []= takes integer key, $TYPE$ value returns nothing
        call Save$NAME$(ht, this, key, value)
    endmethod
endstruct
private module $TYPE$m
    method operator $TYPE$ takes nothing returns $TYPE$s
        return this
    endmethod
endmodule
//! endtextmacro

//! textmacro BUILTIN_SAVEABLE takes NAME, TYPE
private struct $TYPE$s extends array
    method operator [] takes integer key returns $TYPE$
        return Load$NAME$Handle(ht, this, key)
    endmethod
    method operator []= takes integer key, $TYPE$ value returns nothing
        call Save$NAME$Handle(ht, this, key, value)
    endmethod
endstruct
private module $TYPE$m
    method operator $TYPE$ takes nothing returns $TYPE$s
        return this
    endmethod
endmodule
//! endtextmacro

//! textmacro HACK_SAVEABLE takes NAME, TYPE
private struct $TYPE$s extends array
    method operator [] takes integer key returns $TYPE$
        return Convert$NAME$(GetHandleId(LoadFogStateHandle(ht, this, key)))
    endmethod
    method operator []= takes integer key, $TYPE$ value returns nothing
        call SaveFogStateHandle(ht, this, key, ConvertFogState(GetHandleId(value)))
    endmethod
endstruct
private module $TYPE$m
    method operator $TYPE$ takes nothing returns $TYPE$s
        return this
    endmethod
endmodule
//! endtextmacro

//! textmacro DUMB_SAVEABLE takes TYPE
private struct $TYPE$s extends array
    method operator [] takes integer key returns $TYPE$
        return node(LoadInteger(ht, this, key)).$TYPE$E
    endmethod
    method operator []= takes integer key, $TYPE$ value returns nothing
        local node n = node.create()
        set n.$TYPE$E = value
        set n.next = Table(this).aux
        set Table(this).aux = n
        call SaveInteger(ht, this, key, n)
    endmethod
endstruct
private module $TYPE$m
    method operator $TYPE$ takes nothing returns $TYPE$s
        return this
    endmethod
endmodule
//! endtextmacro

//! runtextmacro BASIC_SAVEABLE("Boolean","boolean")
//! runtextmacro BASIC_SAVEABLE("Integer","integer")
//! runtextmacro BASIC_SAVEABLE("Real","real")
//! runtextmacro BASIC_SAVEABLE("Str","string")

//! runtextmacro BUILTIN_SAVEABLE("Ability","ability")
//! runtextmacro BUILTIN_SAVEABLE("BooleanExpr","boolexpr")
//! runtextmacro BUILTIN_SAVEABLE("Button","button")
//! runtextmacro BUILTIN_SAVEABLE("DefeatCondition","defeatcondition")
//! runtextmacro BUILTIN_SAVEABLE("Destructable","destructable")
//! runtextmacro BUILTIN_SAVEABLE("Dialog","dialog")
//! runtextmacro BUILTIN_SAVEABLE("Effect","effect")
//! runtextmacro BUILTIN_SAVEABLE("FogModifier","fogmodifier")
//! runtextmacro BUILTIN_SAVEABLE("FogState","fogstate")
//! runtextmacro BUILTIN_SAVEABLE("Force","force")
//! runtextmacro BUILTIN_SAVEABLE("Group","group")
//! runtextmacro BUILTIN_SAVEABLE("Hashtable","hashtable")
//! runtextmacro BUILTIN_SAVEABLE("Image","image")
//! runtextmacro BUILTIN_SAVEABLE("Item","item")
//! runtextmacro BUILTIN_SAVEABLE("ItemPool","itempool")
//! runtextmacro BUILTIN_SAVEABLE("Leaderboard","leaderboard")
//! runtextmacro BUILTIN_SAVEABLE("Lightning","lightning")
//! runtextmacro BUILTIN_SAVEABLE("Location","location")
//! runtextmacro BUILTIN_SAVEABLE("Multiboard","multiboard")
//! runtextmacro BUILTIN_SAVEABLE("MultiboardItem","multiboarditem")
//! runtextmacro BUILTIN_SAVEABLE("Player","player")
//! runtextmacro BUILTIN_SAVEABLE("Quest","quest")
//! runtextmacro BUILTIN_SAVEABLE("QuestItem","questitem")
//! runtextmacro BUILTIN_SAVEABLE("Rect","rect")
//! runtextmacro BUILTIN_SAVEABLE("Region","region")
//! runtextmacro BUILTIN_SAVEABLE("Sound","sound")
//! runtextmacro BUILTIN_SAVEABLE("TextTag","texttag")
//! runtextmacro BUILTIN_SAVEABLE("Timer","timer")
//! runtextmacro BUILTIN_SAVEABLE("TimerDialog","timerdialog")
//! runtextmacro BUILTIN_SAVEABLE("Trackable","trackable")
//! runtextmacro BUILTIN_SAVEABLE("Trigger","trigger")
//! runtextmacro BUILTIN_SAVEABLE("TriggerAction","triggeraction")
//! runtextmacro BUILTIN_SAVEABLE("TriggerCondition","triggercondition")
//! runtextmacro BUILTIN_SAVEABLE("TriggerEvent","event")
//! runtextmacro BUILTIN_SAVEABLE("Ubersplat","ubersplat")
//! runtextmacro BUILTIN_SAVEABLE("Unit","unit")
//! runtextmacro BUILTIN_SAVEABLE("UnitPool","unitpool")
//! runtextmacro BUILTIN_SAVEABLE("Widget","widget")

//! runtextmacro HACK_SAVEABLE("AIDifficulty","aidifficulty")
//! runtextmacro HACK_SAVEABLE("AllianceType","alliancetype")
//! runtextmacro HACK_SAVEABLE("AttackType","attacktype")
//! runtextmacro HACK_SAVEABLE("BlendMode","blendmode")
//! runtextmacro HACK_SAVEABLE("CameraField","camerafield")
//! runtextmacro HACK_SAVEABLE("DamageType","damagetype")
//! runtextmacro HACK_SAVEABLE("DialogEvent","dialogevent")
//! runtextmacro HACK_SAVEABLE("EffectType","effecttype")
//! runtextmacro HACK_SAVEABLE("FGameState","fgamestate")
//! runtextmacro HACK_SAVEABLE("GameDifficulty","gamedifficulty")
//! runtextmacro HACK_SAVEABLE("GameEvent","gameevent")
//! runtextmacro HACK_SAVEABLE("GameSpeed","gamespeed")
//! runtextmacro HACK_SAVEABLE("GameType","gametype")
//! runtextmacro HACK_SAVEABLE("IGameState","igamestate")
//! runtextmacro HACK_SAVEABLE("ItemType","itemtype")
//! runtextmacro HACK_SAVEABLE("LimitOp","limitop")
//! runtextmacro HACK_SAVEABLE("MapControl","mapcontrol")
//! runtextmacro HACK_SAVEABLE("MapDensity","mapdensity")
//! runtextmacro HACK_SAVEABLE("MapFlag","mapflag")
//! runtextmacro HACK_SAVEABLE("MapSetting","mapsetting")
//! runtextmacro HACK_SAVEABLE("MapVisibility","mapvisibility")
//! runtextmacro HACK_SAVEABLE("PathingType","pathingtype")
//! runtextmacro HACK_SAVEABLE("Placement","placement")
//! runtextmacro HACK_SAVEABLE("PlayerColor","playercolor")
//! runtextmacro HACK_SAVEABLE("PlayerEvent","playerevent")
//! runtextmacro HACK_SAVEABLE("PlayerGameResult","playergameresult")
//! runtextmacro HACK_SAVEABLE("PlayerScore","playerscore")
//! runtextmacro HACK_SAVEABLE("PlayerSlotState","playerslotstate")
//! runtextmacro HACK_SAVEABLE("PlayerState","playerstate")
//! runtextmacro HACK_SAVEABLE("PlayerUnitEvent","playerunitevent")
//! runtextmacro HACK_SAVEABLE("Race","race")
//! runtextmacro HACK_SAVEABLE("RacePref","racepreference")
//! runtextmacro HACK_SAVEABLE("RarityControl","raritycontrol")
//! runtextmacro HACK_SAVEABLE("SoundType","soundtype")
//! runtextmacro HACK_SAVEABLE("StartLocPrio","startlocprio")
//! runtextmacro HACK_SAVEABLE("TexMapFlags","texmapflags")
//! runtextmacro HACK_SAVEABLE("UnitEvent","unitevent")
//! runtextmacro HACK_SAVEABLE("UnitState","unitstate")
//! runtextmacro HACK_SAVEABLE("UnitType","unittype")
//! runtextmacro HACK_SAVEABLE("Version","version")
//! runtextmacro HACK_SAVEABLE("VolumeGroup","volumegroup")
//! runtextmacro HACK_SAVEABLE("WeaponType","weapontype")
//! runtextmacro HACK_SAVEABLE("WidgetEvent","widgetevent")

//! runtextmacro DUMB_SAVEABLE("agent")
//! runtextmacro DUMB_SAVEABLE("buff")
//! runtextmacro DUMB_SAVEABLE("camerasetup")
//! runtextmacro DUMB_SAVEABLE("conditionfunc")
//! runtextmacro DUMB_SAVEABLE("eventid")
//! runtextmacro DUMB_SAVEABLE("filterfunc")
//! runtextmacro DUMB_SAVEABLE("gamecache")
//! runtextmacro DUMB_SAVEABLE("gamestate")
//! runtextmacro DUMB_SAVEABLE("handle")
//! runtextmacro DUMB_SAVEABLE("terraindeformation")
//! runtextmacro DUMB_SAVEABLE("weathereffect")


struct Table
    node aux = 0
    
    implement booleanm
    implement integerm
    implement realm
    implement stringm
    
    implement abilitym
    implement boolexprm
    implement buttonm
    implement defeatconditionm
    implement destructablem
    implement dialogm
    implement effectm
    implement eventm
    implement fogmodifierm
    implement fogstatem
    implement forcem
    implement groupm
    implement hashtablem
    implement imagem
    implement itemm
    implement itempoolm
    implement leaderboardm
    implement lightningm
    implement locationm
    implement multiboarditemm
    implement multiboardm
    implement playerm
    implement questitemm
    implement questm
    implement rectm
    implement regionm
    implement soundm
    implement texttagm
    implement timerdialogm
    implement timerm
    implement trackablem
    implement triggeractionm
    implement triggerconditionm
    implement triggerm
    implement ubersplatm
    implement unitm
    implement unitpoolm
    implement widgetm

    implement aidifficultym
    implement alliancetypem
    implement attacktypem
    implement blendmodem
    implement camerafieldm
    implement damagetypem
    implement dialogeventm
    implement effecttypem
    implement fgamestatem
    implement gamedifficultym
    implement gameeventm
    implement gamespeedm
    implement gametypem
    implement igamestatem
    implement itemtypem
    implement limitopm
    implement mapcontrolm
    implement mapdensitym
    implement mapflagm
    implement mapsettingm
    implement mapvisibilitym
    implement pathingtypem
    implement placementm
    implement playercolorm
    implement playereventm
    implement playergameresultm
    implement playerscorem
    implement playerslotstatem
    implement playerstatem
    implement playeruniteventm
    implement racem
    implement racepreferencem
    implement raritycontrolm
    implement soundtypem
    implement startlocpriom
    implement texmapflagsm
    implement uniteventm
    implement unitstatem
    implement unittypem
    implement versionm
    implement volumegroupm
    implement weapontypem
    implement widgeteventm

    implement agentm
    implement buffm
    implement camerasetupm
    implement conditionfuncm
    implement eventidm
    implement filterfuncm
    implement gamecachem
    implement gamestatem
    implement handlem
    implement terraindeformationm
    implement weathereffectm
    
    method clear takes nothing returns nothing
        call FlushChildHashtable(ht, this)
        loop
        exitwhen aux == 0
            call aux.destroy()
            set aux = aux.next
        endloop
    endmethod
endstruct

endlibrary