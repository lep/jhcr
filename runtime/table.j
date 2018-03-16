// scope Table

globals
    #include "alloc-globals.j"
    
    hashtable _ht = InitHashtable()

    agent array _agent
    buff array _buff
    camerasetup array _camerasetup
    conditionfunc array _conditionfunc
    eventid array _eventid
    filterfunc array _filterfunc
    gamecache array _gamecache
    gamestate array _gamestate
    handle array _handle
    terraindeformation array _terraindeformation
    weathereffect array _weathereffect
    
    integer array _aux
    integer array _next

endglobals

#include "alloc.j"

function _get_boolean takes integer this,integer key returns boolean
    return LoadBoolean(_ht, this, key)
endfunction
function _set_boolean takes integer this,integer key,boolean value returns nothing
    call SaveBoolean(_ht, this, key, value)
endfunction


function _get_integer takes integer this,integer key returns integer
    return LoadInteger(_ht, this, key)
endfunction
function _set_integer takes integer this,integer key,integer value returns nothing
    call SaveInteger(_ht, this, key, value)
endfunction


function _get_real takes integer this,integer key returns real
    return LoadReal(_ht, this, key)
endfunction
function _set_real takes integer this,integer key,real value returns nothing
    call SaveReal(_ht, this, key, value)
endfunction


function _get_string takes integer this,integer key returns string
    return LoadStr(_ht, this, key)
endfunction
function _set_string takes integer this,integer key,string value returns nothing
    call SaveStr(_ht, this, key, value)
endfunction



function _get_ability takes integer this,integer key returns ability
    return LoadAbilityHandle(_ht, this, key)
endfunction
function _set_ability takes integer this,integer key,ability value returns nothing
    call SaveAbilityHandle(_ht, this, key, value)
endfunction


function _get_boolexpr takes integer this,integer key returns boolexpr
    return LoadBooleanExprHandle(_ht, this, key)
endfunction
function _set_boolexpr takes integer this,integer key,boolexpr value returns nothing
    call SaveBooleanExprHandle(_ht, this, key, value)
endfunction


function _get_button takes integer this,integer key returns button
    return LoadButtonHandle(_ht, this, key)
endfunction
function _set_button takes integer this,integer key,button value returns nothing
    call SaveButtonHandle(_ht, this, key, value)
endfunction


function _get_defeatcondition takes integer this,integer key returns defeatcondition
    return LoadDefeatConditionHandle(_ht, this, key)
endfunction
function _set_defeatcondition takes integer this,integer key,defeatcondition value returns nothing
    call SaveDefeatConditionHandle(_ht, this, key, value)
endfunction


function _get_destructable takes integer this,integer key returns destructable
    return LoadDestructableHandle(_ht, this, key)
endfunction
function _set_destructable takes integer this,integer key,destructable value returns nothing
    call SaveDestructableHandle(_ht, this, key, value)
endfunction


function _get_dialog takes integer this,integer key returns dialog
    return LoadDialogHandle(_ht, this, key)
endfunction
function _set_dialog takes integer this,integer key,dialog value returns nothing
    call SaveDialogHandle(_ht, this, key, value)
endfunction


function _get_effect takes integer this,integer key returns effect
    return LoadEffectHandle(_ht, this, key)
endfunction
function _set_effect takes integer this,integer key,effect value returns nothing
    call SaveEffectHandle(_ht, this, key, value)
endfunction


function _get_fogmodifier takes integer this,integer key returns fogmodifier
    return LoadFogModifierHandle(_ht, this, key)
endfunction
function _set_fogmodifier takes integer this,integer key,fogmodifier value returns nothing
    call SaveFogModifierHandle(_ht, this, key, value)
endfunction


function _get_fogstate takes integer this,integer key returns fogstate
    return LoadFogStateHandle(_ht, this, key)
endfunction
function _set_fogstate takes integer this,integer key,fogstate value returns nothing
    call SaveFogStateHandle(_ht, this, key, value)
endfunction


function _get_force takes integer this,integer key returns force
    return LoadForceHandle(_ht, this, key)
endfunction
function _set_force takes integer this,integer key,force value returns nothing
    call SaveForceHandle(_ht, this, key, value)
endfunction


function _get_group takes integer this,integer key returns group
    return LoadGroupHandle(_ht, this, key)
endfunction
function _set_group takes integer this,integer key,group value returns nothing
    call SaveGroupHandle(_ht, this, key, value)
endfunction


function _get_hashtable takes integer this,integer key returns hashtable
    return LoadHashtableHandle(_ht, this, key)
endfunction
function _set_hashtable takes integer this,integer key,hashtable value returns nothing
    call SaveHashtableHandle(_ht, this, key, value)
endfunction


function _get_image takes integer this,integer key returns image
    return LoadImageHandle(_ht, this, key)
endfunction
function _set_image takes integer this,integer key,image value returns nothing
    call SaveImageHandle(_ht, this, key, value)
endfunction


function _get_item takes integer this,integer key returns item
    return LoadItemHandle(_ht, this, key)
endfunction
function _set_item takes integer this,integer key,item value returns nothing
    call SaveItemHandle(_ht, this, key, value)
endfunction


function _get_itempool takes integer this,integer key returns itempool
    return LoadItemPoolHandle(_ht, this, key)
endfunction
function _set_itempool takes integer this,integer key,itempool value returns nothing
    call SaveItemPoolHandle(_ht, this, key, value)
endfunction


function _get_leaderboard takes integer this,integer key returns leaderboard
    return LoadLeaderboardHandle(_ht, this, key)
endfunction
function _set_leaderboard takes integer this,integer key,leaderboard value returns nothing
    call SaveLeaderboardHandle(_ht, this, key, value)
endfunction


function _get_lightning takes integer this,integer key returns lightning
    return LoadLightningHandle(_ht, this, key)
endfunction
function _set_lightning takes integer this,integer key,lightning value returns nothing
    call SaveLightningHandle(_ht, this, key, value)
endfunction


function _get_location takes integer this,integer key returns location
    return LoadLocationHandle(_ht, this, key)
endfunction
function _set_location takes integer this,integer key,location value returns nothing
    call SaveLocationHandle(_ht, this, key, value)
endfunction


function _get_multiboard takes integer this,integer key returns multiboard
    return LoadMultiboardHandle(_ht, this, key)
endfunction
function _set_multiboard takes integer this,integer key,multiboard value returns nothing
    call SaveMultiboardHandle(_ht, this, key, value)
endfunction


function _get_multiboarditem takes integer this,integer key returns multiboarditem
    return LoadMultiboardItemHandle(_ht, this, key)
endfunction
function _set_multiboarditem takes integer this,integer key,multiboarditem value returns nothing
    call SaveMultiboardItemHandle(_ht, this, key, value)
endfunction


function _get_player takes integer this,integer key returns player
    return LoadPlayerHandle(_ht, this, key)
endfunction
function _set_player takes integer this,integer key,player value returns nothing
    call SavePlayerHandle(_ht, this, key, value)
endfunction


function _get_quest takes integer this,integer key returns quest
    return LoadQuestHandle(_ht, this, key)
endfunction
function _set_quest takes integer this,integer key,quest value returns nothing
    call SaveQuestHandle(_ht, this, key, value)
endfunction


function _get_questitem takes integer this,integer key returns questitem
    return LoadQuestItemHandle(_ht, this, key)
endfunction
function _set_questitem takes integer this,integer key,questitem value returns nothing
    call SaveQuestItemHandle(_ht, this, key, value)
endfunction


function _get_rect takes integer this,integer key returns rect
    return LoadRectHandle(_ht, this, key)
endfunction
function _set_rect takes integer this,integer key,rect value returns nothing
    call SaveRectHandle(_ht, this, key, value)
endfunction


function _get_region takes integer this,integer key returns region
    return LoadRegionHandle(_ht, this, key)
endfunction
function _set_region takes integer this,integer key,region value returns nothing
    call SaveRegionHandle(_ht, this, key, value)
endfunction


function _get_sound takes integer this,integer key returns sound
    return LoadSoundHandle(_ht, this, key)
endfunction
function _set_sound takes integer this,integer key,sound value returns nothing
    call SaveSoundHandle(_ht, this, key, value)
endfunction


function _get_texttag takes integer this,integer key returns texttag
    return LoadTextTagHandle(_ht, this, key)
endfunction
function _set_texttag takes integer this,integer key,texttag value returns nothing
    call SaveTextTagHandle(_ht, this, key, value)
endfunction


function _get_timer takes integer this,integer key returns timer
    return LoadTimerHandle(_ht, this, key)
endfunction
function _set_timer takes integer this,integer key,timer value returns nothing
    call SaveTimerHandle(_ht, this, key, value)
endfunction


function _get_timerdialog takes integer this,integer key returns timerdialog
    return LoadTimerDialogHandle(_ht, this, key)
endfunction
function _set_timerdialog takes integer this,integer key,timerdialog value returns nothing
    call SaveTimerDialogHandle(_ht, this, key, value)
endfunction


function _get_trackable takes integer this,integer key returns trackable
    return LoadTrackableHandle(_ht, this, key)
endfunction
function _set_trackable takes integer this,integer key,trackable value returns nothing
    call SaveTrackableHandle(_ht, this, key, value)
endfunction


function _get_trigger takes integer this,integer key returns trigger
    return LoadTriggerHandle(_ht, this, key)
endfunction
function _set_trigger takes integer this,integer key,trigger value returns nothing
    call SaveTriggerHandle(_ht, this, key, value)
endfunction


function _get_triggeraction takes integer this,integer key returns triggeraction
    return LoadTriggerActionHandle(_ht, this, key)
endfunction
function _set_triggeraction takes integer this,integer key,triggeraction value returns nothing
    call SaveTriggerActionHandle(_ht, this, key, value)
endfunction


function _get_triggercondition takes integer this,integer key returns triggercondition
    return LoadTriggerConditionHandle(_ht, this, key)
endfunction
function _set_triggercondition takes integer this,integer key,triggercondition value returns nothing
    call SaveTriggerConditionHandle(_ht, this, key, value)
endfunction


function _get_event takes integer this,integer key returns event
    return LoadTriggerEventHandle(_ht, this, key)
endfunction
function _set_event takes integer this,integer key,event value returns nothing
    call SaveTriggerEventHandle(_ht, this, key, value)
endfunction


function _get_ubersplat takes integer this,integer key returns ubersplat
    return LoadUbersplatHandle(_ht, this, key)
endfunction
function _set_ubersplat takes integer this,integer key,ubersplat value returns nothing
    call SaveUbersplatHandle(_ht, this, key, value)
endfunction


function _get_unit takes integer this,integer key returns unit
    return LoadUnitHandle(_ht, this, key)
endfunction
function _set_unit takes integer this,integer key,unit value returns nothing
    call SaveUnitHandle(_ht, this, key, value)
endfunction


function _get_unitpool takes integer this,integer key returns unitpool
    return LoadUnitPoolHandle(_ht, this, key)
endfunction
function _set_unitpool takes integer this,integer key,unitpool value returns nothing
    call SaveUnitPoolHandle(_ht, this, key, value)
endfunction


function _get_widget takes integer this,integer key returns widget
    return LoadWidgetHandle(_ht, this, key)
endfunction
function _set_widget takes integer this,integer key,widget value returns nothing
    call SaveWidgetHandle(_ht, this, key, value)
endfunction



function _get_aidifficulty takes integer this,integer key returns aidifficulty
    return ConvertAIDifficulty(GetHandleId(LoadFogStateHandle(_ht, this, key)))
endfunction
function _set_aidifficulty takes integer this,integer key,aidifficulty value returns nothing
    call SaveFogStateHandle(_ht, this, key, ConvertFogState(GetHandleId(value)))
endfunction


function _get_alliancetype takes integer this,integer key returns alliancetype
    return ConvertAllianceType(GetHandleId(LoadFogStateHandle(_ht, this, key)))
endfunction
function _set_alliancetype takes integer this,integer key,alliancetype value returns nothing
    call SaveFogStateHandle(_ht, this, key, ConvertFogState(GetHandleId(value)))
endfunction


function _get_attacktype takes integer this,integer key returns attacktype
    return ConvertAttackType(GetHandleId(LoadFogStateHandle(_ht, this, key)))
endfunction
function _set_attacktype takes integer this,integer key,attacktype value returns nothing
    call SaveFogStateHandle(_ht, this, key, ConvertFogState(GetHandleId(value)))
endfunction


function _get_blendmode takes integer this,integer key returns blendmode
    return ConvertBlendMode(GetHandleId(LoadFogStateHandle(_ht, this, key)))
endfunction
function _set_blendmode takes integer this,integer key,blendmode value returns nothing
    call SaveFogStateHandle(_ht, this, key, ConvertFogState(GetHandleId(value)))
endfunction


function _get_camerafield takes integer this,integer key returns camerafield
    return ConvertCameraField(GetHandleId(LoadFogStateHandle(_ht, this, key)))
endfunction
function _set_camerafield takes integer this,integer key,camerafield value returns nothing
    call SaveFogStateHandle(_ht, this, key, ConvertFogState(GetHandleId(value)))
endfunction


function _get_damagetype takes integer this,integer key returns damagetype
    return ConvertDamageType(GetHandleId(LoadFogStateHandle(_ht, this, key)))
endfunction
function _set_damagetype takes integer this,integer key,damagetype value returns nothing
    call SaveFogStateHandle(_ht, this, key, ConvertFogState(GetHandleId(value)))
endfunction


function _get_dialogevent takes integer this,integer key returns dialogevent
    return ConvertDialogEvent(GetHandleId(LoadFogStateHandle(_ht, this, key)))
endfunction
function _set_dialogevent takes integer this,integer key,dialogevent value returns nothing
    call SaveFogStateHandle(_ht, this, key, ConvertFogState(GetHandleId(value)))
endfunction


function _get_effecttype takes integer this,integer key returns effecttype
    return ConvertEffectType(GetHandleId(LoadFogStateHandle(_ht, this, key)))
endfunction
function _set_effecttype takes integer this,integer key,effecttype value returns nothing
    call SaveFogStateHandle(_ht, this, key, ConvertFogState(GetHandleId(value)))
endfunction


function _get_fgamestate takes integer this,integer key returns fgamestate
    return ConvertFGameState(GetHandleId(LoadFogStateHandle(_ht, this, key)))
endfunction
function _set_fgamestate takes integer this,integer key,fgamestate value returns nothing
    call SaveFogStateHandle(_ht, this, key, ConvertFogState(GetHandleId(value)))
endfunction


function _get_gamedifficulty takes integer this,integer key returns gamedifficulty
    return ConvertGameDifficulty(GetHandleId(LoadFogStateHandle(_ht, this, key)))
endfunction
function _set_gamedifficulty takes integer this,integer key,gamedifficulty value returns nothing
    call SaveFogStateHandle(_ht, this, key, ConvertFogState(GetHandleId(value)))
endfunction


function _get_gameevent takes integer this,integer key returns gameevent
    return ConvertGameEvent(GetHandleId(LoadFogStateHandle(_ht, this, key)))
endfunction
function _set_gameevent takes integer this,integer key,gameevent value returns nothing
    call SaveFogStateHandle(_ht, this, key, ConvertFogState(GetHandleId(value)))
endfunction


function _get_gamespeed takes integer this,integer key returns gamespeed
    return ConvertGameSpeed(GetHandleId(LoadFogStateHandle(_ht, this, key)))
endfunction
function _set_gamespeed takes integer this,integer key,gamespeed value returns nothing
    call SaveFogStateHandle(_ht, this, key, ConvertFogState(GetHandleId(value)))
endfunction


function _get_gametype takes integer this,integer key returns gametype
    return ConvertGameType(GetHandleId(LoadFogStateHandle(_ht, this, key)))
endfunction
function _set_gametype takes integer this,integer key,gametype value returns nothing
    call SaveFogStateHandle(_ht, this, key, ConvertFogState(GetHandleId(value)))
endfunction


function _get_igamestate takes integer this,integer key returns igamestate
    return ConvertIGameState(GetHandleId(LoadFogStateHandle(_ht, this, key)))
endfunction
function _set_igamestate takes integer this,integer key,igamestate value returns nothing
    call SaveFogStateHandle(_ht, this, key, ConvertFogState(GetHandleId(value)))
endfunction


function _get_itemtype takes integer this,integer key returns itemtype
    return ConvertItemType(GetHandleId(LoadFogStateHandle(_ht, this, key)))
endfunction
function _set_itemtype takes integer this,integer key,itemtype value returns nothing
    call SaveFogStateHandle(_ht, this, key, ConvertFogState(GetHandleId(value)))
endfunction


function _get_limitop takes integer this,integer key returns limitop
    return ConvertLimitOp(GetHandleId(LoadFogStateHandle(_ht, this, key)))
endfunction
function _set_limitop takes integer this,integer key,limitop value returns nothing
    call SaveFogStateHandle(_ht, this, key, ConvertFogState(GetHandleId(value)))
endfunction


function _get_mapcontrol takes integer this,integer key returns mapcontrol
    return ConvertMapControl(GetHandleId(LoadFogStateHandle(_ht, this, key)))
endfunction
function _set_mapcontrol takes integer this,integer key,mapcontrol value returns nothing
    call SaveFogStateHandle(_ht, this, key, ConvertFogState(GetHandleId(value)))
endfunction


function _get_mapdensity takes integer this,integer key returns mapdensity
    return ConvertMapDensity(GetHandleId(LoadFogStateHandle(_ht, this, key)))
endfunction
function _set_mapdensity takes integer this,integer key,mapdensity value returns nothing
    call SaveFogStateHandle(_ht, this, key, ConvertFogState(GetHandleId(value)))
endfunction


function _get_mapflag takes integer this,integer key returns mapflag
    return ConvertMapFlag(GetHandleId(LoadFogStateHandle(_ht, this, key)))
endfunction
function _set_mapflag takes integer this,integer key,mapflag value returns nothing
    call SaveFogStateHandle(_ht, this, key, ConvertFogState(GetHandleId(value)))
endfunction


function _get_mapsetting takes integer this,integer key returns mapsetting
    return ConvertMapSetting(GetHandleId(LoadFogStateHandle(_ht, this, key)))
endfunction
function _set_mapsetting takes integer this,integer key,mapsetting value returns nothing
    call SaveFogStateHandle(_ht, this, key, ConvertFogState(GetHandleId(value)))
endfunction


function _get_mapvisibility takes integer this,integer key returns mapvisibility
    return ConvertMapVisibility(GetHandleId(LoadFogStateHandle(_ht, this, key)))
endfunction
function _set_mapvisibility takes integer this,integer key,mapvisibility value returns nothing
    call SaveFogStateHandle(_ht, this, key, ConvertFogState(GetHandleId(value)))
endfunction


function _get_pathingtype takes integer this,integer key returns pathingtype
    return ConvertPathingType(GetHandleId(LoadFogStateHandle(_ht, this, key)))
endfunction
function _set_pathingtype takes integer this,integer key,pathingtype value returns nothing
    call SaveFogStateHandle(_ht, this, key, ConvertFogState(GetHandleId(value)))
endfunction


function _get_placement takes integer this,integer key returns placement
    return ConvertPlacement(GetHandleId(LoadFogStateHandle(_ht, this, key)))
endfunction
function _set_placement takes integer this,integer key,placement value returns nothing
    call SaveFogStateHandle(_ht, this, key, ConvertFogState(GetHandleId(value)))
endfunction


function _get_playercolor takes integer this,integer key returns playercolor
    return ConvertPlayerColor(GetHandleId(LoadFogStateHandle(_ht, this, key)))
endfunction
function _set_playercolor takes integer this,integer key,playercolor value returns nothing
    call SaveFogStateHandle(_ht, this, key, ConvertFogState(GetHandleId(value)))
endfunction


function _get_playerevent takes integer this,integer key returns playerevent
    return ConvertPlayerEvent(GetHandleId(LoadFogStateHandle(_ht, this, key)))
endfunction
function _set_playerevent takes integer this,integer key,playerevent value returns nothing
    call SaveFogStateHandle(_ht, this, key, ConvertFogState(GetHandleId(value)))
endfunction


function _get_playergameresult takes integer this,integer key returns playergameresult
    return ConvertPlayerGameResult(GetHandleId(LoadFogStateHandle(_ht, this, key)))
endfunction
function _set_playergameresult takes integer this,integer key,playergameresult value returns nothing
    call SaveFogStateHandle(_ht, this, key, ConvertFogState(GetHandleId(value)))
endfunction


function _get_playerscore takes integer this,integer key returns playerscore
    return ConvertPlayerScore(GetHandleId(LoadFogStateHandle(_ht, this, key)))
endfunction
function _set_playerscore takes integer this,integer key,playerscore value returns nothing
    call SaveFogStateHandle(_ht, this, key, ConvertFogState(GetHandleId(value)))
endfunction


function _get_playerslotstate takes integer this,integer key returns playerslotstate
    return ConvertPlayerSlotState(GetHandleId(LoadFogStateHandle(_ht, this, key)))
endfunction
function _set_playerslotstate takes integer this,integer key,playerslotstate value returns nothing
    call SaveFogStateHandle(_ht, this, key, ConvertFogState(GetHandleId(value)))
endfunction


function _get_playerstate takes integer this,integer key returns playerstate
    return ConvertPlayerState(GetHandleId(LoadFogStateHandle(_ht, this, key)))
endfunction
function _set_playerstate takes integer this,integer key,playerstate value returns nothing
    call SaveFogStateHandle(_ht, this, key, ConvertFogState(GetHandleId(value)))
endfunction


function _get_playerunitevent takes integer this,integer key returns playerunitevent
    return ConvertPlayerUnitEvent(GetHandleId(LoadFogStateHandle(_ht, this, key)))
endfunction
function _set_playerunitevent takes integer this,integer key,playerunitevent value returns nothing
    call SaveFogStateHandle(_ht, this, key, ConvertFogState(GetHandleId(value)))
endfunction


function _get_race takes integer this,integer key returns race
    return ConvertRace(GetHandleId(LoadFogStateHandle(_ht, this, key)))
endfunction
function _set_race takes integer this,integer key,race value returns nothing
    call SaveFogStateHandle(_ht, this, key, ConvertFogState(GetHandleId(value)))
endfunction


function _get_racepreference takes integer this,integer key returns racepreference
    return ConvertRacePref(GetHandleId(LoadFogStateHandle(_ht, this, key)))
endfunction
function _set_racepreference takes integer this,integer key,racepreference value returns nothing
    call SaveFogStateHandle(_ht, this, key, ConvertFogState(GetHandleId(value)))
endfunction


function _get_raritycontrol takes integer this,integer key returns raritycontrol
    return ConvertRarityControl(GetHandleId(LoadFogStateHandle(_ht, this, key)))
endfunction
function _set_raritycontrol takes integer this,integer key,raritycontrol value returns nothing
    call SaveFogStateHandle(_ht, this, key, ConvertFogState(GetHandleId(value)))
endfunction


function _get_soundtype takes integer this,integer key returns soundtype
    return ConvertSoundType(GetHandleId(LoadFogStateHandle(_ht, this, key)))
endfunction
function _set_soundtype takes integer this,integer key,soundtype value returns nothing
    call SaveFogStateHandle(_ht, this, key, ConvertFogState(GetHandleId(value)))
endfunction


function _get_startlocprio takes integer this,integer key returns startlocprio
    return ConvertStartLocPrio(GetHandleId(LoadFogStateHandle(_ht, this, key)))
endfunction
function _set_startlocprio takes integer this,integer key,startlocprio value returns nothing
    call SaveFogStateHandle(_ht, this, key, ConvertFogState(GetHandleId(value)))
endfunction


function _get_texmapflags takes integer this,integer key returns texmapflags
    return ConvertTexMapFlags(GetHandleId(LoadFogStateHandle(_ht, this, key)))
endfunction
function _set_texmapflags takes integer this,integer key,texmapflags value returns nothing
    call SaveFogStateHandle(_ht, this, key, ConvertFogState(GetHandleId(value)))
endfunction


function _get_unitevent takes integer this,integer key returns unitevent
    return ConvertUnitEvent(GetHandleId(LoadFogStateHandle(_ht, this, key)))
endfunction
function _set_unitevent takes integer this,integer key,unitevent value returns nothing
    call SaveFogStateHandle(_ht, this, key, ConvertFogState(GetHandleId(value)))
endfunction


function _get_unitstate takes integer this,integer key returns unitstate
    return ConvertUnitState(GetHandleId(LoadFogStateHandle(_ht, this, key)))
endfunction
function _set_unitstate takes integer this,integer key,unitstate value returns nothing
    call SaveFogStateHandle(_ht, this, key, ConvertFogState(GetHandleId(value)))
endfunction


function _get_unittype takes integer this,integer key returns unittype
    return ConvertUnitType(GetHandleId(LoadFogStateHandle(_ht, this, key)))
endfunction
function _set_unittype takes integer this,integer key,unittype value returns nothing
    call SaveFogStateHandle(_ht, this, key, ConvertFogState(GetHandleId(value)))
endfunction


function _get_version takes integer this,integer key returns version
    return ConvertVersion(GetHandleId(LoadFogStateHandle(_ht, this, key)))
endfunction
function _set_version takes integer this,integer key,version value returns nothing
    call SaveFogStateHandle(_ht, this, key, ConvertFogState(GetHandleId(value)))
endfunction


function _get_volumegroup takes integer this,integer key returns volumegroup
    return ConvertVolumeGroup(GetHandleId(LoadFogStateHandle(_ht, this, key)))
endfunction
function _set_volumegroup takes integer this,integer key,volumegroup value returns nothing
    call SaveFogStateHandle(_ht, this, key, ConvertFogState(GetHandleId(value)))
endfunction


function _get_weapontype takes integer this,integer key returns weapontype
    return ConvertWeaponType(GetHandleId(LoadFogStateHandle(_ht, this, key)))
endfunction
function _set_weapontype takes integer this,integer key,weapontype value returns nothing
    call SaveFogStateHandle(_ht, this, key, ConvertFogState(GetHandleId(value)))
endfunction


function _get_widgetevent takes integer this,integer key returns widgetevent
    return ConvertWidgetEvent(GetHandleId(LoadFogStateHandle(_ht, this, key)))
endfunction
function _set_widgetevent takes integer this,integer key,widgetevent value returns nothing
    call SaveFogStateHandle(_ht, this, key, ConvertFogState(GetHandleId(value)))
endfunction




function _get_agent takes integer this,integer key returns agent
    return _agent[(LoadInteger(_ht, this, key))]
endfunction
function _set_agent takes integer this,integer key,agent value returns nothing
    local integer n= _alloc()
    set _agent[n]=value
    set _next[n]=_aux[this]
    set _aux[this]=n
    call SaveInteger(_ht, this, key, n)
endfunction


function _get_buff takes integer this,integer key returns buff
    return _buff[(LoadInteger(_ht, this, key))]
endfunction
function _set_buff takes integer this,integer key,buff value returns nothing
    local integer n= _alloc()
    set _buff[n]=value
    set _next[n]=_aux[this]
    set _aux[this]=n
    call SaveInteger(_ht, this, key, n)
endfunction


function _get_camerasetup takes integer this,integer key returns camerasetup
    return _camerasetup[(LoadInteger(_ht, this, key))]
endfunction
function _set_camerasetup takes integer this,integer key,camerasetup value returns nothing
    local integer n= _alloc()
    set _camerasetup[n]=value
    set _next[n]=_aux[this]
    set _aux[this]=n
    call SaveInteger(_ht, this, key, n)
endfunction


function _get_conditionfunc takes integer this,integer key returns conditionfunc
    return _conditionfunc[(LoadInteger(_ht, this, key))]
endfunction
function _set_conditionfunc takes integer this,integer key,conditionfunc value returns nothing
    local integer n= _alloc()
    set _conditionfunc[n]=value
    set _next[n]=_aux[this]
    set _aux[this]=n
    call SaveInteger(_ht, this, key, n)
endfunction


function _get_eventid takes integer this,integer key returns eventid
    return _eventid[(LoadInteger(_ht, this, key))]
endfunction
function _set_eventid takes integer this,integer key,eventid value returns nothing
    local integer n= _alloc()
    set _eventid[n]=value
    set _next[n]=_aux[this]
    set _aux[this]=n
    call SaveInteger(_ht, this, key, n)
endfunction


function _get_filterfunc takes integer this,integer key returns filterfunc
    return _filterfunc[(LoadInteger(_ht, this, key))]
endfunction
function _set_filterfunc takes integer this,integer key,filterfunc value returns nothing
    local integer n= _alloc()
    set _filterfunc[n]=value
    set _next[n]=_aux[this]
    set _aux[this]=n
    call SaveInteger(_ht, this, key, n)
endfunction


function _get_gamecache takes integer this,integer key returns gamecache
    return _gamecache[(LoadInteger(_ht, this, key))]
endfunction
function _set_gamecache takes integer this,integer key,gamecache value returns nothing
    local integer n= _alloc()
    set _gamecache[n]=value
    set _next[n]=_aux[this]
    set _aux[this]=n
    call SaveInteger(_ht, this, key, n)
endfunction


function _get_gamestate takes integer this,integer key returns gamestate
    return _gamestate[(LoadInteger(_ht, this, key))]
endfunction
function _set_gamestate takes integer this,integer key,gamestate value returns nothing
    local integer n= _alloc()
    set _gamestate[n]=value
    set _next[n]=_aux[this]
    set _aux[this]=n
    call SaveInteger(_ht, this, key, n)
endfunction


function _get_handle takes integer this,integer key returns handle
    return _handle[(LoadInteger(_ht, this, key))]
endfunction
function _set_handle takes integer this,integer key,handle value returns nothing
    local integer n= _alloc()
    set _handle[n]=value
    set _next[n]=_aux[this]
    set _aux[this]=n
    call SaveInteger(_ht, this, key, n)
endfunction


function _get_terraindeformation takes integer this,integer key returns terraindeformation
    return _terraindeformation[(LoadInteger(_ht, this, key))]
endfunction
function _set_terraindeformation takes integer this,integer key,terraindeformation value returns nothing
    local integer n= _alloc()
    set _terraindeformation[n]=value
    set _next[n]=_aux[this]
    set _aux[this]=n
    call SaveInteger(_ht, this, key, n)
endfunction


function _get_weathereffect takes integer this,integer key returns weathereffect
    return _weathereffect[(LoadInteger(_ht, this, key))]
endfunction
function _set_weathereffect takes integer this,integer key,weathereffect value returns nothing
    local integer n= _alloc()
    set _weathereffect[n]=value
    set _next[n]=_aux[this]
    set _aux[this]=n
    call SaveInteger(_ht, this, key, n)
endfunction
