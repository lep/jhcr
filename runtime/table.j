// scope Table
// REQUIRES List

globals
    #include "alloc-globals.j"
    
    hashtable _ht = InitHashtable()

    integer array _list
    
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
    
#if PATCH_LVL >= 132
    minimapicon array _minimapicon
    commandbuttoneffect array _commandbuttoneffect
#endif

endglobals

#include "alloc.j"

function _flush takes integer _tbl returns nothing
    call List#_destroy(_list[_tbl])
    set _list[_tbl] = 0
    call FlushChildHashtable(_ht, _tbl)
endfunction

function _destroy takes integer _tbl returns nothing
    call List#_destroy(_list[_tbl])
    set _list[_tbl] = 0
    call FlushChildHashtable(_ht, _tbl)
    call _free(_tbl)
endfunction

// native saveable

function _get_boolean takes integer _this,integer _key returns boolean
    return LoadBoolean(_ht, _this, _key)
endfunction
function _set_boolean takes integer _this,integer _key,boolean _value returns nothing
    call SaveBoolean(_ht, _this, _key, _value)
endfunction


function _get_integer takes integer _this,integer _key returns integer
    return LoadInteger(_ht, _this, _key)
endfunction
function _set_integer takes integer _this,integer _key,integer _value returns nothing
    call SaveInteger(_ht, _this, _key, _value)
endfunction


function _get_real takes integer _this,integer _key returns real
    return LoadReal(_ht, _this, _key)
endfunction
function _set_real takes integer _this,integer _key,real _value returns nothing
    call SaveReal(_ht, _this, _key, _value)
endfunction


function _get_string takes integer _this,integer _key returns string
    return LoadStr(_ht, _this, _key)
endfunction
function _set_string takes integer _this,integer _key,string _value returns nothing
    call SaveStr(_ht, _this, _key, _value)
endfunction



function _get_ability takes integer _this,integer _key returns ability
    return LoadAbilityHandle(_ht, _this, _key)
endfunction
function _set_ability takes integer _this,integer _key,ability _value returns nothing
    call SaveAbilityHandle(_ht, _this, _key, _value)
endfunction


function _get_boolexpr takes integer _this,integer _key returns boolexpr
    return LoadBooleanExprHandle(_ht, _this, _key)
endfunction
function _set_boolexpr takes integer _this,integer _key,boolexpr _value returns nothing
    call SaveBooleanExprHandle(_ht, _this, _key, _value)
endfunction


function _get_button takes integer _this,integer _key returns button
    return LoadButtonHandle(_ht, _this, _key)
endfunction
function _set_button takes integer _this,integer _key,button _value returns nothing
    call SaveButtonHandle(_ht, _this, _key, _value)
endfunction


function _get_defeatcondition takes integer _this,integer _key returns defeatcondition
    return LoadDefeatConditionHandle(_ht, _this, _key)
endfunction
function _set_defeatcondition takes integer _this,integer _key,defeatcondition _value returns nothing
    call SaveDefeatConditionHandle(_ht, _this, _key, _value)
endfunction


function _get_destructable takes integer _this,integer _key returns destructable
    return LoadDestructableHandle(_ht, _this, _key)
endfunction
function _set_destructable takes integer _this,integer _key,destructable _value returns nothing
    call SaveDestructableHandle(_ht, _this, _key, _value)
endfunction


function _get_dialog takes integer _this,integer _key returns dialog
    return LoadDialogHandle(_ht, _this, _key)
endfunction
function _set_dialog takes integer _this,integer _key,dialog _value returns nothing
    call SaveDialogHandle(_ht, _this, _key, _value)
endfunction


function _get_effect takes integer _this,integer _key returns effect
    return LoadEffectHandle(_ht, _this, _key)
endfunction
function _set_effect takes integer _this,integer _key,effect _value returns nothing
    call SaveEffectHandle(_ht, _this, _key, _value)
endfunction


function _get_fogmodifier takes integer _this,integer _key returns fogmodifier
    return LoadFogModifierHandle(_ht, _this, _key)
endfunction
function _set_fogmodifier takes integer _this,integer _key,fogmodifier _value returns nothing
    call SaveFogModifierHandle(_ht, _this, _key, _value)
endfunction


function _get_fogstate takes integer _this,integer _key returns fogstate
    return LoadFogStateHandle(_ht, _this, _key)
endfunction
function _set_fogstate takes integer _this,integer _key,fogstate _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, _value)
endfunction


function _get_force takes integer _this,integer _key returns force
    return LoadForceHandle(_ht, _this, _key)
endfunction
function _set_force takes integer _this,integer _key,force _value returns nothing
    call SaveForceHandle(_ht, _this, _key, _value)
endfunction


function _get_group takes integer _this,integer _key returns group
    return LoadGroupHandle(_ht, _this, _key)
endfunction
function _set_group takes integer _this,integer _key,group _value returns nothing
    call SaveGroupHandle(_ht, _this, _key, _value)
endfunction


function _get_hashtable takes integer _this,integer _key returns hashtable
    return LoadHashtableHandle(_ht, _this, _key)
endfunction
function _set_hashtable takes integer _this,integer _key,hashtable _value returns nothing
    call SaveHashtableHandle(_ht, _this, _key, _value)
endfunction


function _get_image takes integer _this,integer _key returns image
    return LoadImageHandle(_ht, _this, _key)
endfunction
function _set_image takes integer _this,integer _key,image _value returns nothing
    call SaveImageHandle(_ht, _this, _key, _value)
endfunction


function _get_item takes integer _this,integer _key returns item
    return LoadItemHandle(_ht, _this, _key)
endfunction
function _set_item takes integer _this,integer _key,item _value returns nothing
    call SaveItemHandle(_ht, _this, _key, _value)
endfunction


function _get_itempool takes integer _this,integer _key returns itempool
    return LoadItemPoolHandle(_ht, _this, _key)
endfunction
function _set_itempool takes integer _this,integer _key,itempool _value returns nothing
    call SaveItemPoolHandle(_ht, _this, _key, _value)
endfunction


function _get_leaderboard takes integer _this,integer _key returns leaderboard
    return LoadLeaderboardHandle(_ht, _this, _key)
endfunction
function _set_leaderboard takes integer _this,integer _key,leaderboard _value returns nothing
    call SaveLeaderboardHandle(_ht, _this, _key, _value)
endfunction


function _get_lightning takes integer _this,integer _key returns lightning
    return LoadLightningHandle(_ht, _this, _key)
endfunction
function _set_lightning takes integer _this,integer _key,lightning _value returns nothing
    call SaveLightningHandle(_ht, _this, _key, _value)
endfunction


function _get_location takes integer _this,integer _key returns location
    return LoadLocationHandle(_ht, _this, _key)
endfunction
function _set_location takes integer _this,integer _key,location _value returns nothing
    call SaveLocationHandle(_ht, _this, _key, _value)
endfunction


function _get_multiboard takes integer _this,integer _key returns multiboard
    return LoadMultiboardHandle(_ht, _this, _key)
endfunction
function _set_multiboard takes integer _this,integer _key,multiboard _value returns nothing
    call SaveMultiboardHandle(_ht, _this, _key, _value)
endfunction


function _get_multiboarditem takes integer _this,integer _key returns multiboarditem
    return LoadMultiboardItemHandle(_ht, _this, _key)
endfunction
function _set_multiboarditem takes integer _this,integer _key,multiboarditem _value returns nothing
    call SaveMultiboardItemHandle(_ht, _this, _key, _value)
endfunction


function _get_player takes integer _this,integer _key returns player
    return LoadPlayerHandle(_ht, _this, _key)
endfunction
function _set_player takes integer _this,integer _key,player _value returns nothing
    call SavePlayerHandle(_ht, _this, _key, _value)
endfunction


function _get_quest takes integer _this,integer _key returns quest
    return LoadQuestHandle(_ht, _this, _key)
endfunction
function _set_quest takes integer _this,integer _key,quest _value returns nothing
    call SaveQuestHandle(_ht, _this, _key, _value)
endfunction


function _get_questitem takes integer _this,integer _key returns questitem
    return LoadQuestItemHandle(_ht, _this, _key)
endfunction
function _set_questitem takes integer _this,integer _key,questitem _value returns nothing
    call SaveQuestItemHandle(_ht, _this, _key, _value)
endfunction


function _get_rect takes integer _this,integer _key returns rect
    return LoadRectHandle(_ht, _this, _key)
endfunction
function _set_rect takes integer _this,integer _key,rect _value returns nothing
    call SaveRectHandle(_ht, _this, _key, _value)
endfunction


function _get_region takes integer _this,integer _key returns region
    return LoadRegionHandle(_ht, _this, _key)
endfunction
function _set_region takes integer _this,integer _key,region _value returns nothing
    call SaveRegionHandle(_ht, _this, _key, _value)
endfunction


function _get_sound takes integer _this,integer _key returns sound
    return LoadSoundHandle(_ht, _this, _key)
endfunction
function _set_sound takes integer _this,integer _key,sound _value returns nothing
    call SaveSoundHandle(_ht, _this, _key, _value)
endfunction


function _get_texttag takes integer _this,integer _key returns texttag
    return LoadTextTagHandle(_ht, _this, _key)
endfunction
function _set_texttag takes integer _this,integer _key,texttag _value returns nothing
    call SaveTextTagHandle(_ht, _this, _key, _value)
endfunction


function _get_timer takes integer _this,integer _key returns timer
    return LoadTimerHandle(_ht, _this, _key)
endfunction
function _set_timer takes integer _this,integer _key,timer _value returns nothing
    call SaveTimerHandle(_ht, _this, _key, _value)
endfunction


function _get_timerdialog takes integer _this,integer _key returns timerdialog
    return LoadTimerDialogHandle(_ht, _this, _key)
endfunction
function _set_timerdialog takes integer _this,integer _key,timerdialog _value returns nothing
    call SaveTimerDialogHandle(_ht, _this, _key, _value)
endfunction


function _get_trackable takes integer _this,integer _key returns trackable
    return LoadTrackableHandle(_ht, _this, _key)
endfunction
function _set_trackable takes integer _this,integer _key,trackable _value returns nothing
    call SaveTrackableHandle(_ht, _this, _key, _value)
endfunction


function _get_trigger takes integer _this,integer _key returns trigger
    return LoadTriggerHandle(_ht, _this, _key)
endfunction
function _set_trigger takes integer _this,integer _key,trigger _value returns nothing
    call SaveTriggerHandle(_ht, _this, _key, _value)
endfunction


function _get_triggeraction takes integer _this,integer _key returns triggeraction
    return LoadTriggerActionHandle(_ht, _this, _key)
endfunction
function _set_triggeraction takes integer _this,integer _key,triggeraction _value returns nothing
    call SaveTriggerActionHandle(_ht, _this, _key, _value)
endfunction


function _get_triggercondition takes integer _this,integer _key returns triggercondition
    return LoadTriggerConditionHandle(_ht, _this, _key)
endfunction
function _set_triggercondition takes integer _this,integer _key,triggercondition _value returns nothing
    call SaveTriggerConditionHandle(_ht, _this, _key, _value)
endfunction


function _get_event takes integer _this,integer _key returns event
    return LoadTriggerEventHandle(_ht, _this, _key)
endfunction
function _set_event takes integer _this,integer _key,event _value returns nothing
    call SaveTriggerEventHandle(_ht, _this, _key, _value)
endfunction


function _get_ubersplat takes integer _this,integer _key returns ubersplat
    return LoadUbersplatHandle(_ht, _this, _key)
endfunction
function _set_ubersplat takes integer _this,integer _key,ubersplat _value returns nothing
    call SaveUbersplatHandle(_ht, _this, _key, _value)
endfunction


function _get_unit takes integer _this,integer _key returns unit
    return LoadUnitHandle(_ht, _this, _key)
endfunction
function _set_unit takes integer _this,integer _key,unit _value returns nothing
    call SaveUnitHandle(_ht, _this, _key, _value)
endfunction


function _get_unitpool takes integer _this,integer _key returns unitpool
    return LoadUnitPoolHandle(_ht, _this, _key)
endfunction
function _set_unitpool takes integer _this,integer _key,unitpool _value returns nothing
    call SaveUnitPoolHandle(_ht, _this, _key, _value)
endfunction


function _get_widget takes integer _this,integer _key returns widget
    return LoadWidgetHandle(_ht, _this, _key)
endfunction
function _set_widget takes integer _this,integer _key,widget _value returns nothing
    call SaveWidgetHandle(_ht, _this, _key, _value)
endfunction

// saveable via ConvertFogState

function _get_aidifficulty takes integer _this,integer _key returns aidifficulty
    return ConvertAIDifficulty(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_aidifficulty takes integer _this,integer _key,aidifficulty _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction


function _get_alliancetype takes integer _this,integer _key returns alliancetype
    return ConvertAllianceType(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_alliancetype takes integer _this,integer _key,alliancetype _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction



function _get_attacktype takes integer _this,integer _key returns attacktype
    return ConvertAttackType(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_attacktype takes integer _this,integer _key,attacktype _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction


function _get_blendmode takes integer _this,integer _key returns blendmode
    return ConvertBlendMode(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_blendmode takes integer _this,integer _key,blendmode _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction


function _get_camerafield takes integer _this,integer _key returns camerafield
    return ConvertCameraField(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_camerafield takes integer _this,integer _key,camerafield _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction


function _get_damagetype takes integer _this,integer _key returns damagetype
    return ConvertDamageType(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_damagetype takes integer _this,integer _key,damagetype _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction


function _get_dialogevent takes integer _this,integer _key returns dialogevent
    return ConvertDialogEvent(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_dialogevent takes integer _this,integer _key,dialogevent _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction


function _get_effecttype takes integer _this,integer _key returns effecttype
    return ConvertEffectType(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_effecttype takes integer _this,integer _key,effecttype _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction


function _get_fgamestate takes integer _this,integer _key returns fgamestate
    return ConvertFGameState(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_fgamestate takes integer _this,integer _key,fgamestate _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction


function _get_gamedifficulty takes integer _this,integer _key returns gamedifficulty
    return ConvertGameDifficulty(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_gamedifficulty takes integer _this,integer _key,gamedifficulty _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction


function _get_gameevent takes integer _this,integer _key returns gameevent
    return ConvertGameEvent(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_gameevent takes integer _this,integer _key,gameevent _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction


function _get_gamespeed takes integer _this,integer _key returns gamespeed
    return ConvertGameSpeed(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_gamespeed takes integer _this,integer _key,gamespeed _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction


function _get_gametype takes integer _this,integer _key returns gametype
    return ConvertGameType(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_gametype takes integer _this,integer _key,gametype _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction


function _get_igamestate takes integer _this,integer _key returns igamestate
    return ConvertIGameState(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_igamestate takes integer _this,integer _key,igamestate _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction


function _get_itemtype takes integer _this,integer _key returns itemtype
    return ConvertItemType(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_itemtype takes integer _this,integer _key,itemtype _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction


function _get_limitop takes integer _this,integer _key returns limitop
    return ConvertLimitOp(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_limitop takes integer _this,integer _key,limitop _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction


function _get_mapcontrol takes integer _this,integer _key returns mapcontrol
    return ConvertMapControl(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_mapcontrol takes integer _this,integer _key,mapcontrol _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction


function _get_mapdensity takes integer _this,integer _key returns mapdensity
    return ConvertMapDensity(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_mapdensity takes integer _this,integer _key,mapdensity _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction


function _get_mapflag takes integer _this,integer _key returns mapflag
    return ConvertMapFlag(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_mapflag takes integer _this,integer _key,mapflag _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction


function _get_mapsetting takes integer _this,integer _key returns mapsetting
    return ConvertMapSetting(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_mapsetting takes integer _this,integer _key,mapsetting _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction


function _get_mapvisibility takes integer _this,integer _key returns mapvisibility
    return ConvertMapVisibility(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_mapvisibility takes integer _this,integer _key,mapvisibility _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction


function _get_pathingtype takes integer _this,integer _key returns pathingtype
    return ConvertPathingType(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_pathingtype takes integer _this,integer _key,pathingtype _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction


function _get_placement takes integer _this,integer _key returns placement
    return ConvertPlacement(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_placement takes integer _this,integer _key,placement _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction


function _get_playercolor takes integer _this,integer _key returns playercolor
    return ConvertPlayerColor(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_playercolor takes integer _this,integer _key,playercolor _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction


function _get_playerevent takes integer _this,integer _key returns playerevent
    return ConvertPlayerEvent(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_playerevent takes integer _this,integer _key,playerevent _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction


function _get_playergameresult takes integer _this,integer _key returns playergameresult
    return ConvertPlayerGameResult(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_playergameresult takes integer _this,integer _key,playergameresult _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction


function _get_playerscore takes integer _this,integer _key returns playerscore
    return ConvertPlayerScore(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_playerscore takes integer _this,integer _key,playerscore _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction


function _get_playerslotstate takes integer _this,integer _key returns playerslotstate
    return ConvertPlayerSlotState(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_playerslotstate takes integer _this,integer _key,playerslotstate _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction


function _get_playerstate takes integer _this,integer _key returns playerstate
    return ConvertPlayerState(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_playerstate takes integer _this,integer _key,playerstate _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction


function _get_playerunitevent takes integer _this,integer _key returns playerunitevent
    return ConvertPlayerUnitEvent(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_playerunitevent takes integer _this,integer _key,playerunitevent _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction


function _get_race takes integer _this,integer _key returns race
    return ConvertRace(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_race takes integer _this,integer _key,race _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction


function _get_racepreference takes integer _this,integer _key returns racepreference
    return ConvertRacePref(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_racepreference takes integer _this,integer _key,racepreference _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction


function _get_raritycontrol takes integer _this,integer _key returns raritycontrol
    return ConvertRarityControl(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_raritycontrol takes integer _this,integer _key,raritycontrol _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction


function _get_soundtype takes integer _this,integer _key returns soundtype
    return ConvertSoundType(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_soundtype takes integer _this,integer _key,soundtype _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction


function _get_startlocprio takes integer _this,integer _key returns startlocprio
    return ConvertStartLocPrio(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_startlocprio takes integer _this,integer _key,startlocprio _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction


function _get_texmapflags takes integer _this,integer _key returns texmapflags
    return ConvertTexMapFlags(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_texmapflags takes integer _this,integer _key,texmapflags _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction


function _get_unitevent takes integer _this,integer _key returns unitevent
    return ConvertUnitEvent(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_unitevent takes integer _this,integer _key,unitevent _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction


function _get_unitstate takes integer _this,integer _key returns unitstate
    return ConvertUnitState(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_unitstate takes integer _this,integer _key,unitstate _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction


function _get_unittype takes integer _this,integer _key returns unittype
    return ConvertUnitType(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_unittype takes integer _this,integer _key,unittype _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction


function _get_version takes integer _this,integer _key returns version
    return ConvertVersion(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_version takes integer _this,integer _key,version _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction


function _get_volumegroup takes integer _this,integer _key returns volumegroup
    return ConvertVolumeGroup(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_volumegroup takes integer _this,integer _key,volumegroup _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction


function _get_weapontype takes integer _this,integer _key returns weapontype
    return ConvertWeaponType(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_weapontype takes integer _this,integer _key,weapontype _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction


function _get_widgetevent takes integer _this,integer _key returns widgetevent
    return ConvertWidgetEvent(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_widgetevent takes integer _this,integer _key,widgetevent _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction



// neither native saveable nor convertfogstate-hackable

function _get_agent takes integer _this,integer _key returns agent
    return _agent[(LoadInteger(_ht, _this, _key))]
endfunction
function _set_agent takes integer _this,integer _key,agent _value returns nothing
    set _list[_this] = List#_cons(_list[_this])
    set _agent[_list[_this]]=_value
    call SaveInteger(_ht, _this, _key, _list[_this])
endfunction


function _get_buff takes integer _this,integer _key returns buff
    return _buff[(LoadInteger(_ht, _this, _key))]
endfunction
function _set_buff takes integer _this,integer _key,buff _value returns nothing
    set _list[_this] = List#_cons(_list[_this])
    set _buff[_list[_this]]=_value
    call SaveInteger(_ht, _this, _key, _list[_this])
endfunction


function _get_camerasetup takes integer _this,integer _key returns camerasetup
    return _camerasetup[(LoadInteger(_ht, _this, _key))]
endfunction
function _set_camerasetup takes integer _this,integer _key,camerasetup _value returns nothing
    set _list[_this] = List#_cons(_list[_this])
    set _camerasetup[_list[_this]]=_value
    call SaveInteger(_ht, _this, _key, _list[_this])
endfunction


function _get_conditionfunc takes integer _this,integer _key returns conditionfunc
    return _conditionfunc[(LoadInteger(_ht, _this, _key))]
endfunction
function _set_conditionfunc takes integer _this,integer _key,conditionfunc _value returns nothing
    set _list[_this] = List#_cons(_list[_this])
    set _conditionfunc[_list[_this]]=_value
    call SaveInteger(_ht, _this, _key, _list[_this])
endfunction


function _get_eventid takes integer _this,integer _key returns eventid
    return _eventid[(LoadInteger(_ht, _this, _key))]
endfunction
function _set_eventid takes integer _this,integer _key,eventid _value returns nothing
    set _list[_this] = List#_cons(_list[_this])
    set _eventid[_list[_this]]=_value
    call SaveInteger(_ht, _this, _key, _list[_this])
endfunction


function _get_filterfunc takes integer _this,integer _key returns filterfunc
    return _filterfunc[(LoadInteger(_ht, _this, _key))]
endfunction
function _set_filterfunc takes integer _this,integer _key,filterfunc _value returns nothing
    set _list[_this] = List#_cons(_list[_this])
    set _filterfunc[_list[_this]]=_value
    call SaveInteger(_ht, _this, _key, _list[_this])
endfunction


function _get_gamecache takes integer _this,integer _key returns gamecache
    return _gamecache[(LoadInteger(_ht, _this, _key))]
endfunction
function _set_gamecache takes integer _this,integer _key,gamecache _value returns nothing
    set _list[_this] = List#_cons(_list[_this])
    set _gamecache[_list[_this]]=_value
    call SaveInteger(_ht, _this, _key, _list[_this])
endfunction


function _get_gamestate takes integer _this,integer _key returns gamestate
    return _gamestate[(LoadInteger(_ht, _this, _key))]
endfunction
function _set_gamestate takes integer _this,integer _key,gamestate _value returns nothing
    set _list[_this] = List#_cons(_list[_this])
    set _gamestate[_list[_this]]=_value
    call SaveInteger(_ht, _this, _key, _list[_this])
endfunction


function _get_handle takes integer _this,integer _key returns handle
    return _handle[(LoadInteger(_ht, _this, _key))]
endfunction
function _set_handle takes integer _this,integer _key,handle _value returns nothing
    set _list[_this] = List#_cons(_list[_this])
    set _handle[_list[_this]]=_value
    call SaveInteger(_ht, _this, _key, _list[_this])
endfunction


function _get_terraindeformation takes integer _this,integer _key returns terraindeformation
    return _terraindeformation[(LoadInteger(_ht, _this, _key))]
endfunction
function _set_terraindeformation takes integer _this,integer _key,terraindeformation _value returns nothing
    set _list[_this] = List#_cons(_list[_this])
    set _terraindeformation[_list[_this]]=_value
    call SaveInteger(_ht, _this, _key, _list[_this])
endfunction


function _get_weathereffect takes integer _this,integer _key returns weathereffect
    return _weathereffect[(LoadInteger(_ht, _this, _key))]
endfunction
function _set_weathereffect takes integer _this,integer _key,weathereffect _value returns nothing
    set _list[_this] = List#_cons(_list[_this])
    set _weathereffect[_list[_this]]=_value
    call SaveInteger(_ht, _this, _key, _list[_this])
endfunction

#if PATCH_LVL >= 129

// patch 1.29
function _get_mousebuttontype takes integer _this,integer _key returns mousebuttontype
    return ConvertMouseButtonType(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_mousebuttontype takes integer _this,integer _key,mousebuttontype _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction

#endif

#if PATCH_LVL >= 130

// patch 1.30
function _get_animtype takes integer _this,integer _key returns animtype
    return ConvertAnimType(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_animtype takes integer _this,integer _key,animtype _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction

function _get_subanimtype takes integer _this,integer _key returns subanimtype
    return ConvertSubAnimType(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_subanimtype takes integer _this,integer _key,subanimtype _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction

#endif

#if PATCH_LVL >= 131
// 1.31


function _get_framehandle takes integer _this,integer _key returns framehandle
    return LoadFrameHandle(_ht, _this, _key)
endfunction
function _set_framehandle takes integer _this,integer _key,framehandle _value returns nothing
    call SaveFrameHandle(_ht, _this, _key, _value)
endfunction

function _get_originframetype takes integer _this,integer _key returns originframetype
    return ConvertOriginFrameType(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_originframetype takes integer _this,integer _key, originframetype _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction
function _get_framepointtype takes integer _this,integer _key returns framepointtype
    return ConvertFramePointType(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_framepointtype takes integer _this,integer _key, framepointtype _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction
function _get_textaligntype takes integer _this,integer _key returns textaligntype
    return ConvertTextAlignType(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_textaligntype takes integer _this,integer _key, textaligntype _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction
function _get_frameeventtype takes integer _this,integer _key returns frameeventtype
    return ConvertFrameEventType(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_frameeventtype takes integer _this,integer _key, frameeventtype _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction
function _get_oskeytype takes integer _this,integer _key returns oskeytype
    return ConvertOsKeyType(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_oskeytype takes integer _this,integer _key, oskeytype _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction
function _get_abilityintegerfield takes integer _this,integer _key returns abilityintegerfield
    return ConvertAbilityIntegerField(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_abilityintegerfield takes integer _this,integer _key, abilityintegerfield _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction
function _get_abilityrealfield takes integer _this,integer _key returns abilityrealfield
    return ConvertAbilityRealField(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_abilityrealfield takes integer _this,integer _key, abilityrealfield _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction
function _get_abilitybooleanfield takes integer _this,integer _key returns abilitybooleanfield
    return ConvertAbilityBooleanField(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_abilitybooleanfield takes integer _this,integer _key, abilitybooleanfield _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction
function _get_abilitystringfield takes integer _this,integer _key returns abilitystringfield
    return ConvertAbilityStringField(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_abilitystringfield takes integer _this,integer _key, abilitystringfield _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction
function _get_abilityintegerlevelfield takes integer _this,integer _key returns abilityintegerlevelfield
    return ConvertAbilityIntegerLevelField(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_abilityintegerlevelfield takes integer _this,integer _key, abilityintegerlevelfield _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction
function _get_abilityreallevelfield takes integer _this,integer _key returns abilityreallevelfield
    return ConvertAbilityRealLevelField(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_abilityreallevelfield takes integer _this,integer _key, abilityreallevelfield _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction
function _get_abilitybooleanlevelfield takes integer _this,integer _key returns abilitybooleanlevelfield
    return ConvertAbilityBooleanLevelField(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_abilitybooleanlevelfield takes integer _this,integer _key, abilitybooleanlevelfield _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction
function _get_abilitystringlevelfield takes integer _this,integer _key returns abilitystringlevelfield
    return ConvertAbilityStringLevelField(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_abilitystringlevelfield takes integer _this,integer _key, abilitystringlevelfield _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction
function _get_abilityintegerlevelarrayfield takes integer _this,integer _key returns abilityintegerlevelarrayfield
    return ConvertAbilityIntegerLevelArrayField(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_abilityintegerlevelarrayfield takes integer _this,integer _key, abilityintegerlevelarrayfield _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction
function _get_abilityreallevelarrayfield takes integer _this,integer _key returns abilityreallevelarrayfield
    return ConvertAbilityRealLevelArrayField(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_abilityreallevelarrayfield takes integer _this,integer _key, abilityreallevelarrayfield _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction
function _get_abilitybooleanlevelarrayfield takes integer _this,integer _key returns abilitybooleanlevelarrayfield
    return ConvertAbilityBooleanLevelArrayField(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_abilitybooleanlevelarrayfield takes integer _this,integer _key, abilitybooleanlevelarrayfield _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction
function _get_abilitystringlevelarrayfield takes integer _this,integer _key returns abilitystringlevelarrayfield
    return ConvertAbilityStringLevelArrayField(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_abilitystringlevelarrayfield takes integer _this,integer _key, abilitystringlevelarrayfield _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction
function _get_unitintegerfield takes integer _this,integer _key returns unitintegerfield
    return ConvertUnitIntegerField(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_unitintegerfield takes integer _this,integer _key, unitintegerfield _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction
function _get_unitrealfield takes integer _this,integer _key returns unitrealfield
    return ConvertUnitRealField(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_unitrealfield takes integer _this,integer _key, unitrealfield _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction
function _get_unitbooleanfield takes integer _this,integer _key returns unitbooleanfield
    return ConvertUnitBooleanField(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_unitbooleanfield takes integer _this,integer _key, unitbooleanfield _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction
function _get_unitstringfield takes integer _this,integer _key returns unitstringfield
    return ConvertUnitStringField(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_unitstringfield takes integer _this,integer _key, unitstringfield _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction
function _get_unitweaponintegerfield takes integer _this,integer _key returns unitweaponintegerfield
    return ConvertUnitWeaponIntegerField(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_unitweaponintegerfield takes integer _this,integer _key, unitweaponintegerfield _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction
function _get_unitweaponrealfield takes integer _this,integer _key returns unitweaponrealfield
    return ConvertUnitWeaponRealField(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_unitweaponrealfield takes integer _this,integer _key, unitweaponrealfield _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction
function _get_unitweaponbooleanfield takes integer _this,integer _key returns unitweaponbooleanfield
    return ConvertUnitWeaponBooleanField(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_unitweaponbooleanfield takes integer _this,integer _key, unitweaponbooleanfield _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction
function _get_unitweaponstringfield takes integer _this,integer _key returns unitweaponstringfield
    return ConvertUnitWeaponStringField(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_unitweaponstringfield takes integer _this,integer _key, unitweaponstringfield _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction
function _get_itemintegerfield takes integer _this,integer _key returns itemintegerfield
    return ConvertItemIntegerField(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_itemintegerfield takes integer _this,integer _key, itemintegerfield _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction
function _get_itemrealfield takes integer _this,integer _key returns itemrealfield
    return ConvertItemRealField(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_itemrealfield takes integer _this,integer _key, itemrealfield _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction
function _get_itembooleanfield takes integer _this,integer _key returns itembooleanfield
    return ConvertItemBooleanField(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_itembooleanfield takes integer _this,integer _key, itembooleanfield _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction
function _get_itemstringfield takes integer _this,integer _key returns itemstringfield
    return ConvertItemStringField(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_itemstringfield takes integer _this,integer _key, itemstringfield _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction
function _get_movetype takes integer _this,integer _key returns movetype
    return ConvertMoveType(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_movetype takes integer _this,integer _key, movetype _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction
function _get_targetflag takes integer _this,integer _key returns targetflag
    return ConvertTargetFlag(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_targetflag takes integer _this,integer _key, targetflag _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction
function _get_armortype takes integer _this,integer _key returns armortype
    return ConvertArmorType(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_armortype takes integer _this,integer _key, armortype _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction
function _get_heroattribute takes integer _this,integer _key returns heroattribute
    return ConvertHeroAttribute(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_heroattribute takes integer _this,integer _key, heroattribute _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction
function _get_defensetype takes integer _this,integer _key returns defensetype
    return ConvertDefenseType(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_defensetype takes integer _this,integer _key, defensetype _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction
function _get_regentype takes integer _this,integer _key returns regentype
    return ConvertRegenType(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_regentype takes integer _this,integer _key, regentype _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction
function _get_unitcategory takes integer _this,integer _key returns unitcategory
    return ConvertUnitCategory(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_unitcategory takes integer _this,integer _key, unitcategory _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction
function _get_pathingflag takes integer _this,integer _key returns pathingflag
    return ConvertPathingFlag(GetHandleId(LoadFogStateHandle(_ht, _this, _key)))
endfunction
function _set_pathingflag takes integer _this,integer _key, pathingflag _value returns nothing
    call SaveFogStateHandle(_ht, _this, _key, ConvertFogState(GetHandleId(_value)))
endfunction

#endif

#if PATCH_LVL >= 132
// patch 1.32

function _get_minimapicon takes integer _this,integer _key returns minimapicon
    return _minimapicon[(LoadInteger(_ht, _this, _key))]
endfunction
function _set_minimapicon takes integer _this,integer _key,minimapicon _value returns nothing
    set _list[_this] = List#_cons(_list[_this])
    set _minimapicon[_list[_this]]=_value
    call SaveInteger(_ht, _this, _key, _list[_this])
endfunction

function _get_commandbuttoneffect takes integer _this,integer _key returns commandbuttoneffect
    return _commandbuttoneffect[(LoadInteger(_ht, _this, _key))]
endfunction
function _set_commandbuttoneffect takes integer _this,integer _key,commandbuttoneffect _value returns nothing
    set _list[_this] = List#_cons(_list[_this])
    set _commandbuttoneffect[_list[_this]]=_value
    call SaveInteger(_ht, _this, _key, _list[_this])
endfunction

#endif