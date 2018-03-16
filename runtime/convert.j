// scope Convert

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
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_weathereffect(Scopes#_scope,fromReg))

else
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_weapontype(Scopes#_scope,fromReg))
endif

else
if (fromType) < (5) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_volumegroup(Scopes#_scope,fromReg))

else
if (fromType) < (6) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_version(Scopes#_scope,fromReg))

else
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_unittype(Scopes#_scope,fromReg))
endif
endif
endif

else
if (fromType) < (10) then
if (fromType) < (8) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_unitstate(Scopes#_scope,fromReg))

else
if (fromType) < (9) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_unitpool(Scopes#_scope,fromReg))

else
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_ubersplat(Scopes#_scope,fromReg))
endif
endif

else
if (fromType) < (11) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_triggeraction(Scopes#_scope,fromReg))

else
if (fromType) < (12) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_texttag(Scopes#_scope,fromReg))

else
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_texmapflags(Scopes#_scope,fromReg))
endif
endif
endif
endif

else
if (fromType) < (18) then
if (fromType) < (15) then
if (fromType) < (14) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_terraindeformation(Scopes#_scope,fromReg))

else
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_startlocprio(Scopes#_scope,fromReg))
endif

else
if (fromType) < (16) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_soundtype(Scopes#_scope,fromReg))

else
if (fromType) < (17) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_raritycontrol(Scopes#_scope,fromReg))

else
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_racepreference(Scopes#_scope,fromReg))
endif
endif
endif

else
if (fromType) < (21) then
if (fromType) < (19) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_race(Scopes#_scope,fromReg))

else
if (fromType) < (20) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_playerstate(Scopes#_scope,fromReg))

else
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_playerslotstate(Scopes#_scope,fromReg))
endif
endif

else
if (fromType) < (22) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_playerscore(Scopes#_scope,fromReg))

else
if (fromType) < (23) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_playergameresult(Scopes#_scope,fromReg))

else
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_playercolor(Scopes#_scope,fromReg))
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
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_placement(Scopes#_scope,fromReg))

else
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_pathingtype(Scopes#_scope,fromReg))
endif

else
if (fromType) < (27) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_mapvisibility(Scopes#_scope,fromReg))

else
if (fromType) < (28) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_mapsetting(Scopes#_scope,fromReg))

else
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_mapflag(Scopes#_scope,fromReg))
endif
endif
endif

else
if (fromType) < (32) then
if (fromType) < (30) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_mapdensity(Scopes#_scope,fromReg))

else
if (fromType) < (31) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_mapcontrol(Scopes#_scope,fromReg))

else
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_lightning(Scopes#_scope,fromReg))
endif
endif

else
if (fromType) < (33) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_itemtype(Scopes#_scope,fromReg))

else
if (fromType) < (34) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_itempool(Scopes#_scope,fromReg))

else
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_image(Scopes#_scope,fromReg))
endif
endif
endif
endif

else
if (fromType) < (41) then
if (fromType) < (38) then
if (fromType) < (36) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_gametype(Scopes#_scope,fromReg))

else
if (fromType) < (37) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_gamestate(Scopes#_scope,fromReg))

else
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_igamestate(Scopes#_scope,fromReg))
endif
endif

else
if (fromType) < (39) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_fgamestate(Scopes#_scope,fromReg))

else
if (fromType) < (40) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_gamespeed(Scopes#_scope,fromReg))

else
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_gamedifficulty(Scopes#_scope,fromReg))
endif
endif
endif

else
if (fromType) < (44) then
if (fromType) < (42) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_fogstate(Scopes#_scope,fromReg))

else
if (fromType) < (43) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_eventid(Scopes#_scope,fromReg))

else
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_widgetevent(Scopes#_scope,fromReg))
endif
endif

else
if (fromType) < (45) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_unitevent(Scopes#_scope,fromReg))

else
if (fromType) < (46) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_playerunitevent(Scopes#_scope,fromReg))

else
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_playerevent(Scopes#_scope,fromReg))
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
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_limitop(Scopes#_scope,fromReg))

else
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_gameevent(Scopes#_scope,fromReg))
endif

else
if (fromType) < (50) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_dialogevent(Scopes#_scope,fromReg))

else
if (fromType) < (51) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_effecttype(Scopes#_scope,fromReg))

else
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_damagetype(Scopes#_scope,fromReg))
endif
endif
endif

else
if (fromType) < (55) then
if (fromType) < (53) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_camerasetup(Scopes#_scope,fromReg))

else
if (fromType) < (54) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_camerafield(Scopes#_scope,fromReg))

else
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_blendmode(Scopes#_scope,fromReg))
endif
endif

else
if (fromType) < (56) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_attacktype(Scopes#_scope,fromReg))

else
if (fromType) < (57) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_alliancetype(Scopes#_scope,fromReg))

else
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_aidifficulty(Scopes#_scope,fromReg))
endif
endif
endif
endif

else
if (fromType) < (64) then
if (fromType) < (61) then
if (fromType) < (59) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_agent(Scopes#_scope,fromReg))

else
if (fromType) < (60) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_widget(Scopes#_scope,fromReg))

else
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_unit(Scopes#_scope,fromReg))
endif
endif

else
if (fromType) < (62) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_item(Scopes#_scope,fromReg))

else
if (fromType) < (63) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_destructable(Scopes#_scope,fromReg))

else
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_triggercondition(Scopes#_scope,fromReg))
endif
endif
endif

else
if (fromType) < (67) then
if (fromType) < (65) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_trigger(Scopes#_scope,fromReg))

else
if (fromType) < (66) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_trackable(Scopes#_scope,fromReg))

else
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_timerdialog(Scopes#_scope,fromReg))
endif
endif

else
if (fromType) < (68) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_timer(Scopes#_scope,fromReg))

else
if (fromType) < (69) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_sound(Scopes#_scope,fromReg))

else
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_region(Scopes#_scope,fromReg))
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
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_rect(Scopes#_scope,fromReg))

else
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_questitem(Scopes#_scope,fromReg))
endif

else
if (fromType) < (73) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_quest(Scopes#_scope,fromReg))

else
if (fromType) < (74) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_player(Scopes#_scope,fromReg))

else
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_multiboarditem(Scopes#_scope,fromReg))
endif
endif
endif

else
if (fromType) < (78) then
if (fromType) < (76) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_multiboard(Scopes#_scope,fromReg))

else
if (fromType) < (77) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_location(Scopes#_scope,fromReg))

else
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_leaderboard(Scopes#_scope,fromReg))
endif
endif

else
if (fromType) < (79) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_hashtable(Scopes#_scope,fromReg))

else
if (fromType) < (80) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_group(Scopes#_scope,fromReg))

else
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_gamecache(Scopes#_scope,fromReg))
endif
endif
endif
endif

else
if (fromType) < (87) then
if (fromType) < (84) then
if (fromType) < (82) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_force(Scopes#_scope,fromReg))

else
if (fromType) < (83) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_fogmodifier(Scopes#_scope,fromReg))

else
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_event(Scopes#_scope,fromReg))
endif
endif

else
if (fromType) < (85) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_effect(Scopes#_scope,fromReg))

else
if (fromType) < (86) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_dialog(Scopes#_scope,fromReg))

else
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_defeatcondition(Scopes#_scope,fromReg))
endif
endif
endif

else
if (fromType) < (90) then
if (fromType) < (88) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_button(Scopes#_scope,fromReg))

else
if (fromType) < (89) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_boolexpr(Scopes#_scope,fromReg))

else
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_filterfunc(Scopes#_scope,fromReg))
endif
endif

else
if (fromType) < (91) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_conditionfunc(Scopes#_scope,fromReg))

else
if (fromType) < (92) then
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_ability(Scopes#_scope,fromReg))

else
call Table#_set_handle(Scopes#_scope,toReg,Table#_get_buff(Scopes#_scope,fromReg))
endif
endif
endif
endif
endif
endif
endif

else
if (fromType) < (38) then
call Table#_set_gamestate(Scopes#_scope,toReg,Table#_get_igamestate(Scopes#_scope,fromReg))

else
call Table#_set_gamestate(Scopes#_scope,toReg,Table#_get_fgamestate(Scopes#_scope,fromReg))
endif
endif

else
if (_toTypeOffset[toType]) < (4) then
if (fromType) < (46) then
if (fromType) < (44) then
call Table#_set_eventid(Scopes#_scope,toReg,Table#_get_widgetevent(Scopes#_scope,fromReg))

else
if (fromType) < (45) then
call Table#_set_eventid(Scopes#_scope,toReg,Table#_get_unitevent(Scopes#_scope,fromReg))

else
call Table#_set_eventid(Scopes#_scope,toReg,Table#_get_playerunitevent(Scopes#_scope,fromReg))
endif
endif

else
if (fromType) < (48) then
if (fromType) < (47) then
call Table#_set_eventid(Scopes#_scope,toReg,Table#_get_playerevent(Scopes#_scope,fromReg))

else
call Table#_set_eventid(Scopes#_scope,toReg,Table#_get_limitop(Scopes#_scope,fromReg))
endif

else
if (fromType) < (49) then
call Table#_set_eventid(Scopes#_scope,toReg,Table#_get_gameevent(Scopes#_scope,fromReg))

else
call Table#_set_eventid(Scopes#_scope,toReg,Table#_get_dialogevent(Scopes#_scope,fromReg))
endif
endif
endif

else
if (fromType) < (76) then
if (fromType) < (67) then
if (fromType) < (63) then
if (fromType) < (61) then
if (fromType) < (60) then
call Table#_set_agent(Scopes#_scope,toReg,Table#_get_widget(Scopes#_scope,fromReg))

else
call Table#_set_agent(Scopes#_scope,toReg,Table#_get_unit(Scopes#_scope,fromReg))
endif

else
if (fromType) < (62) then
call Table#_set_agent(Scopes#_scope,toReg,Table#_get_item(Scopes#_scope,fromReg))

else
call Table#_set_agent(Scopes#_scope,toReg,Table#_get_destructable(Scopes#_scope,fromReg))
endif
endif

else
if (fromType) < (65) then
if (fromType) < (64) then
call Table#_set_agent(Scopes#_scope,toReg,Table#_get_triggercondition(Scopes#_scope,fromReg))

else
call Table#_set_agent(Scopes#_scope,toReg,Table#_get_trigger(Scopes#_scope,fromReg))
endif

else
if (fromType) < (66) then
call Table#_set_agent(Scopes#_scope,toReg,Table#_get_trackable(Scopes#_scope,fromReg))

else
call Table#_set_agent(Scopes#_scope,toReg,Table#_get_timerdialog(Scopes#_scope,fromReg))
endif
endif
endif

else
if (fromType) < (71) then
if (fromType) < (69) then
if (fromType) < (68) then
call Table#_set_agent(Scopes#_scope,toReg,Table#_get_timer(Scopes#_scope,fromReg))

else
call Table#_set_agent(Scopes#_scope,toReg,Table#_get_sound(Scopes#_scope,fromReg))
endif

else
if (fromType) < (70) then
call Table#_set_agent(Scopes#_scope,toReg,Table#_get_region(Scopes#_scope,fromReg))

else
call Table#_set_agent(Scopes#_scope,toReg,Table#_get_rect(Scopes#_scope,fromReg))
endif
endif

else
if (fromType) < (73) then
if (fromType) < (72) then
call Table#_set_agent(Scopes#_scope,toReg,Table#_get_questitem(Scopes#_scope,fromReg))

else
call Table#_set_agent(Scopes#_scope,toReg,Table#_get_quest(Scopes#_scope,fromReg))
endif

else
if (fromType) < (74) then
call Table#_set_agent(Scopes#_scope,toReg,Table#_get_player(Scopes#_scope,fromReg))

else
if (fromType) < (75) then
call Table#_set_agent(Scopes#_scope,toReg,Table#_get_multiboarditem(Scopes#_scope,fromReg))

else
call Table#_set_agent(Scopes#_scope,toReg,Table#_get_multiboard(Scopes#_scope,fromReg))
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
call Table#_set_agent(Scopes#_scope,toReg,Table#_get_location(Scopes#_scope,fromReg))

else
call Table#_set_agent(Scopes#_scope,toReg,Table#_get_leaderboard(Scopes#_scope,fromReg))
endif

else
if (fromType) < (79) then
call Table#_set_agent(Scopes#_scope,toReg,Table#_get_hashtable(Scopes#_scope,fromReg))

else
call Table#_set_agent(Scopes#_scope,toReg,Table#_get_group(Scopes#_scope,fromReg))
endif
endif

else
if (fromType) < (82) then
if (fromType) < (81) then
call Table#_set_agent(Scopes#_scope,toReg,Table#_get_gamecache(Scopes#_scope,fromReg))

else
call Table#_set_agent(Scopes#_scope,toReg,Table#_get_force(Scopes#_scope,fromReg))
endif

else
if (fromType) < (83) then
call Table#_set_agent(Scopes#_scope,toReg,Table#_get_fogmodifier(Scopes#_scope,fromReg))

else
call Table#_set_agent(Scopes#_scope,toReg,Table#_get_event(Scopes#_scope,fromReg))
endif
endif
endif

else
if (fromType) < (88) then
if (fromType) < (86) then
if (fromType) < (85) then
call Table#_set_agent(Scopes#_scope,toReg,Table#_get_effect(Scopes#_scope,fromReg))

else
call Table#_set_agent(Scopes#_scope,toReg,Table#_get_dialog(Scopes#_scope,fromReg))
endif

else
if (fromType) < (87) then
call Table#_set_agent(Scopes#_scope,toReg,Table#_get_defeatcondition(Scopes#_scope,fromReg))

else
call Table#_set_agent(Scopes#_scope,toReg,Table#_get_button(Scopes#_scope,fromReg))
endif
endif

else
if (fromType) < (90) then
if (fromType) < (89) then
call Table#_set_agent(Scopes#_scope,toReg,Table#_get_boolexpr(Scopes#_scope,fromReg))

else
call Table#_set_agent(Scopes#_scope,toReg,Table#_get_filterfunc(Scopes#_scope,fromReg))
endif

else
if (fromType) < (91) then
call Table#_set_agent(Scopes#_scope,toReg,Table#_get_conditionfunc(Scopes#_scope,fromReg))

else
if (fromType) < (92) then
call Table#_set_agent(Scopes#_scope,toReg,Table#_get_ability(Scopes#_scope,fromReg))

else
call Table#_set_agent(Scopes#_scope,toReg,Table#_get_buff(Scopes#_scope,fromReg))
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
call Table#_set_widget(Scopes#_scope,toReg,Table#_get_unit(Scopes#_scope,fromReg))

else
if (fromType) < (62) then
call Table#_set_widget(Scopes#_scope,toReg,Table#_get_item(Scopes#_scope,fromReg))

else
call Table#_set_widget(Scopes#_scope,toReg,Table#_get_destructable(Scopes#_scope,fromReg))
endif
endif

else
if (fromType) < (90) then
call Table#_set_boolexpr(Scopes#_scope,toReg,Table#_get_filterfunc(Scopes#_scope,fromReg))

else
call Table#_set_boolexpr(Scopes#_scope,toReg,Table#_get_conditionfunc(Scopes#_scope,fromReg))
endif
endif

else
if (_toTypeOffset[toType]) < (8) then
call Table#_set_ability(Scopes#_scope,toReg,Table#_get_buff(Scopes#_scope,fromReg))

else
call Table#_set_real(Scopes#_scope,toReg,Table#_get_integer(Scopes#_scope,fromReg))
endif
endif
endif
endfunction