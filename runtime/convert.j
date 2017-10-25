globals
integer array _toTypeOffset
endglobals
function _convert takes integer toType,integer toReg,integer fromType,integer fromReg returns nothing
if (_toTypeOffset[toType]) < (5) then
if (_toTypeOffset[toType]) < (3) then
if (_toTypeOffset[toType]) < (2) then
if (fromType) < (47) then
if (fromType) < (24) then
if (fromType) < (13) then
if (fromType) < (7) then
if (fromType) < (4) then
if (fromType) < (3) then
call _set_handle(_scope,toReg,_get_weathereffect(_scope,fromReg))

else
call _set_handle(_scope,toReg,_get_weapontype(_scope,fromReg))
endif

else
if (fromType) < (5) then
call _set_handle(_scope,toReg,_get_volumegroup(_scope,fromReg))

else
if (fromType) < (6) then
call _set_handle(_scope,toReg,_get_version(_scope,fromReg))

else
call _set_handle(_scope,toReg,_get_unittype(_scope,fromReg))
endif
endif
endif

else
if (fromType) < (10) then
if (fromType) < (8) then
call _set_handle(_scope,toReg,_get_unitstate(_scope,fromReg))

else
if (fromType) < (9) then
call _set_handle(_scope,toReg,_get_unitpool(_scope,fromReg))

else
call _set_handle(_scope,toReg,_get_ubersplat(_scope,fromReg))
endif
endif

else
if (fromType) < (11) then
call _set_handle(_scope,toReg,_get_triggeraction(_scope,fromReg))

else
if (fromType) < (12) then
call _set_handle(_scope,toReg,_get_texttag(_scope,fromReg))

else
call _set_handle(_scope,toReg,_get_texmapflags(_scope,fromReg))
endif
endif
endif
endif

else
if (fromType) < (18) then
if (fromType) < (15) then
if (fromType) < (14) then
call _set_handle(_scope,toReg,_get_terraindeformation(_scope,fromReg))

else
call _set_handle(_scope,toReg,_get_startlocprio(_scope,fromReg))
endif

else
if (fromType) < (16) then
call _set_handle(_scope,toReg,_get_soundtype(_scope,fromReg))

else
if (fromType) < (17) then
call _set_handle(_scope,toReg,_get_raritycontrol(_scope,fromReg))

else
call _set_handle(_scope,toReg,_get_racepreference(_scope,fromReg))
endif
endif
endif

else
if (fromType) < (21) then
if (fromType) < (19) then
call _set_handle(_scope,toReg,_get_race(_scope,fromReg))

else
if (fromType) < (20) then
call _set_handle(_scope,toReg,_get_playerstate(_scope,fromReg))

else
call _set_handle(_scope,toReg,_get_playerslotstate(_scope,fromReg))
endif
endif

else
if (fromType) < (22) then
call _set_handle(_scope,toReg,_get_playerscore(_scope,fromReg))

else
if (fromType) < (23) then
call _set_handle(_scope,toReg,_get_playergameresult(_scope,fromReg))

else
call _set_handle(_scope,toReg,_get_playercolor(_scope,fromReg))
endif
endif
endif
endif
endif

else
if (fromType) < (35) then
if (fromType) < (29) then
if (fromType) < (26) then
if (fromType) < (25) then
call _set_handle(_scope,toReg,_get_placement(_scope,fromReg))

else
call _set_handle(_scope,toReg,_get_pathingtype(_scope,fromReg))
endif

else
if (fromType) < (27) then
call _set_handle(_scope,toReg,_get_mapvisibility(_scope,fromReg))

else
if (fromType) < (28) then
call _set_handle(_scope,toReg,_get_mapsetting(_scope,fromReg))

else
call _set_handle(_scope,toReg,_get_mapflag(_scope,fromReg))
endif
endif
endif

else
if (fromType) < (32) then
if (fromType) < (30) then
call _set_handle(_scope,toReg,_get_mapdensity(_scope,fromReg))

else
if (fromType) < (31) then
call _set_handle(_scope,toReg,_get_mapcontrol(_scope,fromReg))

else
call _set_handle(_scope,toReg,_get_lightning(_scope,fromReg))
endif
endif

else
if (fromType) < (33) then
call _set_handle(_scope,toReg,_get_itemtype(_scope,fromReg))

else
if (fromType) < (34) then
call _set_handle(_scope,toReg,_get_itempool(_scope,fromReg))

else
call _set_handle(_scope,toReg,_get_image(_scope,fromReg))
endif
endif
endif
endif

else
if (fromType) < (41) then
if (fromType) < (38) then
if (fromType) < (36) then
call _set_handle(_scope,toReg,_get_gametype(_scope,fromReg))

else
if (fromType) < (37) then
call _set_handle(_scope,toReg,_get_gamestate(_scope,fromReg))

else
call _set_handle(_scope,toReg,_get_igamestate(_scope,fromReg))
endif
endif

else
if (fromType) < (39) then
call _set_handle(_scope,toReg,_get_fgamestate(_scope,fromReg))

else
if (fromType) < (40) then
call _set_handle(_scope,toReg,_get_gamespeed(_scope,fromReg))

else
call _set_handle(_scope,toReg,_get_gamedifficulty(_scope,fromReg))
endif
endif
endif

else
if (fromType) < (44) then
if (fromType) < (42) then
call _set_handle(_scope,toReg,_get_fogstate(_scope,fromReg))

else
if (fromType) < (43) then
call _set_handle(_scope,toReg,_get_eventid(_scope,fromReg))

else
call _set_handle(_scope,toReg,_get_widgetevent(_scope,fromReg))
endif
endif

else
if (fromType) < (45) then
call _set_handle(_scope,toReg,_get_unitevent(_scope,fromReg))

else
if (fromType) < (46) then
call _set_handle(_scope,toReg,_get_playerunitevent(_scope,fromReg))

else
call _set_handle(_scope,toReg,_get_playerevent(_scope,fromReg))
endif
endif
endif
endif
endif
endif

else
if (fromType) < (70) then
if (fromType) < (58) then
if (fromType) < (52) then
if (fromType) < (49) then
if (fromType) < (48) then
call _set_handle(_scope,toReg,_get_limitop(_scope,fromReg))

else
call _set_handle(_scope,toReg,_get_gameevent(_scope,fromReg))
endif

else
if (fromType) < (50) then
call _set_handle(_scope,toReg,_get_dialogevent(_scope,fromReg))

else
if (fromType) < (51) then
call _set_handle(_scope,toReg,_get_effecttype(_scope,fromReg))

else
call _set_handle(_scope,toReg,_get_damagetype(_scope,fromReg))
endif
endif
endif

else
if (fromType) < (55) then
if (fromType) < (53) then
call _set_handle(_scope,toReg,_get_camerasetup(_scope,fromReg))

else
if (fromType) < (54) then
call _set_handle(_scope,toReg,_get_camerafield(_scope,fromReg))

else
call _set_handle(_scope,toReg,_get_blendmode(_scope,fromReg))
endif
endif

else
if (fromType) < (56) then
call _set_handle(_scope,toReg,_get_attacktype(_scope,fromReg))

else
if (fromType) < (57) then
call _set_handle(_scope,toReg,_get_alliancetype(_scope,fromReg))

else
call _set_handle(_scope,toReg,_get_aidifficulty(_scope,fromReg))
endif
endif
endif
endif

else
if (fromType) < (64) then
if (fromType) < (61) then
if (fromType) < (59) then
call _set_handle(_scope,toReg,_get_agent(_scope,fromReg))

else
if (fromType) < (60) then
call _set_handle(_scope,toReg,_get_widget(_scope,fromReg))

else
call _set_handle(_scope,toReg,_get_unit(_scope,fromReg))
endif
endif

else
if (fromType) < (62) then
call _set_handle(_scope,toReg,_get_item(_scope,fromReg))

else
if (fromType) < (63) then
call _set_handle(_scope,toReg,_get_destructable(_scope,fromReg))

else
call _set_handle(_scope,toReg,_get_triggercondition(_scope,fromReg))
endif
endif
endif

else
if (fromType) < (67) then
if (fromType) < (65) then
call _set_handle(_scope,toReg,_get_trigger(_scope,fromReg))

else
if (fromType) < (66) then
call _set_handle(_scope,toReg,_get_trackable(_scope,fromReg))

else
call _set_handle(_scope,toReg,_get_timerdialog(_scope,fromReg))
endif
endif

else
if (fromType) < (68) then
call _set_handle(_scope,toReg,_get_timer(_scope,fromReg))

else
if (fromType) < (69) then
call _set_handle(_scope,toReg,_get_sound(_scope,fromReg))

else
call _set_handle(_scope,toReg,_get_region(_scope,fromReg))
endif
endif
endif
endif
endif

else
if (fromType) < (81) then
if (fromType) < (75) then
if (fromType) < (72) then
if (fromType) < (71) then
call _set_handle(_scope,toReg,_get_rect(_scope,fromReg))

else
call _set_handle(_scope,toReg,_get_questitem(_scope,fromReg))
endif

else
if (fromType) < (73) then
call _set_handle(_scope,toReg,_get_quest(_scope,fromReg))

else
if (fromType) < (74) then
call _set_handle(_scope,toReg,_get_player(_scope,fromReg))

else
call _set_handle(_scope,toReg,_get_multiboarditem(_scope,fromReg))
endif
endif
endif

else
if (fromType) < (78) then
if (fromType) < (76) then
call _set_handle(_scope,toReg,_get_multiboard(_scope,fromReg))

else
if (fromType) < (77) then
call _set_handle(_scope,toReg,_get_location(_scope,fromReg))

else
call _set_handle(_scope,toReg,_get_leaderboard(_scope,fromReg))
endif
endif

else
if (fromType) < (79) then
call _set_handle(_scope,toReg,_get_hashtable(_scope,fromReg))

else
if (fromType) < (80) then
call _set_handle(_scope,toReg,_get_group(_scope,fromReg))

else
call _set_handle(_scope,toReg,_get_gamecache(_scope,fromReg))
endif
endif
endif
endif

else
if (fromType) < (87) then
if (fromType) < (84) then
if (fromType) < (82) then
call _set_handle(_scope,toReg,_get_force(_scope,fromReg))

else
if (fromType) < (83) then
call _set_handle(_scope,toReg,_get_fogmodifier(_scope,fromReg))

else
call _set_handle(_scope,toReg,_get_event(_scope,fromReg))
endif
endif

else
if (fromType) < (85) then
call _set_handle(_scope,toReg,_get_effect(_scope,fromReg))

else
if (fromType) < (86) then
call _set_handle(_scope,toReg,_get_dialog(_scope,fromReg))

else
call _set_handle(_scope,toReg,_get_defeatcondition(_scope,fromReg))
endif
endif
endif

else
if (fromType) < (90) then
if (fromType) < (88) then
call _set_handle(_scope,toReg,_get_button(_scope,fromReg))

else
if (fromType) < (89) then
call _set_handle(_scope,toReg,_get_boolexpr(_scope,fromReg))

else
call _set_handle(_scope,toReg,_get_filterfunc(_scope,fromReg))
endif
endif

else
if (fromType) < (91) then
call _set_handle(_scope,toReg,_get_conditionfunc(_scope,fromReg))

else
if (fromType) < (92) then
call _set_handle(_scope,toReg,_get_ability(_scope,fromReg))

else
call _set_handle(_scope,toReg,_get_buff(_scope,fromReg))
endif
endif
endif
endif
endif
endif
endif

else
if (fromType) < (38) then
call _set_gamestate(_scope,toReg,_get_igamestate(_scope,fromReg))

else
call _set_gamestate(_scope,toReg,_get_fgamestate(_scope,fromReg))
endif
endif

else
if (_toTypeOffset[toType]) < (4) then
if (fromType) < (46) then
if (fromType) < (44) then
call _set_eventid(_scope,toReg,_get_widgetevent(_scope,fromReg))

else
if (fromType) < (45) then
call _set_eventid(_scope,toReg,_get_unitevent(_scope,fromReg))

else
call _set_eventid(_scope,toReg,_get_playerunitevent(_scope,fromReg))
endif
endif

else
if (fromType) < (48) then
if (fromType) < (47) then
call _set_eventid(_scope,toReg,_get_playerevent(_scope,fromReg))

else
call _set_eventid(_scope,toReg,_get_limitop(_scope,fromReg))
endif

else
if (fromType) < (49) then
call _set_eventid(_scope,toReg,_get_gameevent(_scope,fromReg))

else
call _set_eventid(_scope,toReg,_get_dialogevent(_scope,fromReg))
endif
endif
endif

else
if (fromType) < (76) then
if (fromType) < (67) then
if (fromType) < (63) then
if (fromType) < (61) then
if (fromType) < (60) then
call _set_agent(_scope,toReg,_get_widget(_scope,fromReg))

else
call _set_agent(_scope,toReg,_get_unit(_scope,fromReg))
endif

else
if (fromType) < (62) then
call _set_agent(_scope,toReg,_get_item(_scope,fromReg))

else
call _set_agent(_scope,toReg,_get_destructable(_scope,fromReg))
endif
endif

else
if (fromType) < (65) then
if (fromType) < (64) then
call _set_agent(_scope,toReg,_get_triggercondition(_scope,fromReg))

else
call _set_agent(_scope,toReg,_get_trigger(_scope,fromReg))
endif

else
if (fromType) < (66) then
call _set_agent(_scope,toReg,_get_trackable(_scope,fromReg))

else
call _set_agent(_scope,toReg,_get_timerdialog(_scope,fromReg))
endif
endif
endif

else
if (fromType) < (71) then
if (fromType) < (69) then
if (fromType) < (68) then
call _set_agent(_scope,toReg,_get_timer(_scope,fromReg))

else
call _set_agent(_scope,toReg,_get_sound(_scope,fromReg))
endif

else
if (fromType) < (70) then
call _set_agent(_scope,toReg,_get_region(_scope,fromReg))

else
call _set_agent(_scope,toReg,_get_rect(_scope,fromReg))
endif
endif

else
if (fromType) < (73) then
if (fromType) < (72) then
call _set_agent(_scope,toReg,_get_questitem(_scope,fromReg))

else
call _set_agent(_scope,toReg,_get_quest(_scope,fromReg))
endif

else
if (fromType) < (74) then
call _set_agent(_scope,toReg,_get_player(_scope,fromReg))

else
if (fromType) < (75) then
call _set_agent(_scope,toReg,_get_multiboarditem(_scope,fromReg))

else
call _set_agent(_scope,toReg,_get_multiboard(_scope,fromReg))
endif
endif
endif
endif
endif

else
if (fromType) < (84) then
if (fromType) < (80) then
if (fromType) < (78) then
if (fromType) < (77) then
call _set_agent(_scope,toReg,_get_location(_scope,fromReg))

else
call _set_agent(_scope,toReg,_get_leaderboard(_scope,fromReg))
endif

else
if (fromType) < (79) then
call _set_agent(_scope,toReg,_get_hashtable(_scope,fromReg))

else
call _set_agent(_scope,toReg,_get_group(_scope,fromReg))
endif
endif

else
if (fromType) < (82) then
if (fromType) < (81) then
call _set_agent(_scope,toReg,_get_gamecache(_scope,fromReg))

else
call _set_agent(_scope,toReg,_get_force(_scope,fromReg))
endif

else
if (fromType) < (83) then
call _set_agent(_scope,toReg,_get_fogmodifier(_scope,fromReg))

else
call _set_agent(_scope,toReg,_get_event(_scope,fromReg))
endif
endif
endif

else
if (fromType) < (88) then
if (fromType) < (86) then
if (fromType) < (85) then
call _set_agent(_scope,toReg,_get_effect(_scope,fromReg))

else
call _set_agent(_scope,toReg,_get_dialog(_scope,fromReg))
endif

else
if (fromType) < (87) then
call _set_agent(_scope,toReg,_get_defeatcondition(_scope,fromReg))

else
call _set_agent(_scope,toReg,_get_button(_scope,fromReg))
endif
endif

else
if (fromType) < (90) then
if (fromType) < (89) then
call _set_agent(_scope,toReg,_get_boolexpr(_scope,fromReg))

else
call _set_agent(_scope,toReg,_get_filterfunc(_scope,fromReg))
endif

else
if (fromType) < (91) then
call _set_agent(_scope,toReg,_get_conditionfunc(_scope,fromReg))

else
if (fromType) < (92) then
call _set_agent(_scope,toReg,_get_ability(_scope,fromReg))

else
call _set_agent(_scope,toReg,_get_buff(_scope,fromReg))
endif
endif
endif
endif
endif
endif
endif
endif

else
if (_toTypeOffset[toType]) < (7) then
if (_toTypeOffset[toType]) < (6) then
if (fromType) < (61) then
call _set_widget(_scope,toReg,_get_unit(_scope,fromReg))

else
if (fromType) < (62) then
call _set_widget(_scope,toReg,_get_item(_scope,fromReg))

else
call _set_widget(_scope,toReg,_get_destructable(_scope,fromReg))
endif
endif

else
if (fromType) < (90) then
call _set_boolexpr(_scope,toReg,_get_filterfunc(_scope,fromReg))

else
call _set_boolexpr(_scope,toReg,_get_conditionfunc(_scope,fromReg))
endif
endif

else
if (_toTypeOffset[toType]) < (8) then
call _set_ability(_scope,toReg,_get_buff(_scope,fromReg))

else
call _set_real(_scope,toReg,_get_integer(_scope,fromReg))
endif
endif
endif
endfunction