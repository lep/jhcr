{-# LANGUAGE OverloadedStrings #-}

module Hot.Types (types) where

import Data.ByteString.Lazy (ByteString)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Int



types :: Map String Int8
types = Map.fromList [("handle",1),("weathereffect",2),("weapontype",3),("volumegroup",4),("version",5),("unittype",6),("unitstate",7),("unitpool",8),("ubersplat",9),("triggeraction",10),("texttag",11),("texmapflags",12),("terraindeformation",13),("startlocprio",14),("soundtype",15),("raritycontrol",16),("racepreference",17),("race",18),("playerstate",19),("playerslotstate",20),("playerscore",21),("playergameresult",22),("playercolor",23),("placement",24),("pathingtype",25),("mapvisibility",26),("mapsetting",27),("mapflag",28),("mapdensity",29),("mapcontrol",30),("lightning",31),("itemtype",32),("itempool",33),("image",34),("gametype",35),("gamestate",36),("igamestate",37),("fgamestate",38),("gamespeed",39),("gamedifficulty",40),("fogstate",41),("eventid",42),("widgetevent",43),("unitevent",44),("playerunitevent",45),("playerevent",46),("limitop",47),("gameevent",48),("dialogevent",49),("effecttype",50),("damagetype",51),("camerasetup",52),("camerafield",53),("blendmode",54),("attacktype",55),("alliancetype",56),("aidifficulty",57),("agent",58),("widget",59),("unit",60),("item",61),("destructable",62),("triggercondition",63),("trigger",64),("trackable",65),("timerdialog",66),("timer",67),("sound",68),("region",69),("rect",70),("questitem",71),("quest",72),("player",73),("multiboarditem",74),("multiboard",75),("location",76),("leaderboard",77),("hashtable",78),("group",79),("gamecache",80),("force",81),("fogmodifier",82),("event",83),("effect",84),("dialog",85),("defeatcondition",86),("button",87),("boolexpr",88),("filterfunc",89),("conditionfunc",90),("ability",91),("buff",92),("real",93),("integer",94), ("string", 95), ("code",96), ("boolean", 97), ("nothing",98)]
