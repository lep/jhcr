{-# LANGUAGE OverloadedStrings #-}

module Hot.Types (types) where

import Data.ByteString.Lazy (ByteString)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Int



types :: Map ByteString Int32
types = Map.fromList [("handle",1),("agent",2),("ability",3),("buff",4),("boolexpr",5),("conditionfunc",6),("filterfunc",7),("button",8),("defeatcondition",9),("dialog",10),("effect",11),("event",12),("fogmodifier",13),("force",14),("gamecache",15),("group",16),("hashtable",17),("leaderboard",18),("location",19),("multiboard",20),("multiboarditem",21),("player",22),("quest",23),("questitem",24),("rect",25),("region",26),("sound",27),("timer",28),("timerdialog",29),("trackable",30),("trigger",31),("triggercondition",32),("widget",33),("destructable",34),("item",35),("unit",36),("aidifficulty",37),("alliancetype",38),("attacktype",39),("blendmode",40),("camerafield",41),("camerasetup",42),("damagetype",43),("effecttype",44),("eventid",45),("dialogevent",46),("gameevent",47),("limitop",48),("playerevent",49),("playerunitevent",50),("unitevent",51),("widgetevent",52),("fogstate",53),("gamedifficulty",54),("gamespeed",55),("gamestate",56),("fgamestate",57),("igamestate",58),("gametype",59),("image",60),("itempool",61),("itemtype",62),("lightning",63),("mapcontrol",64),("mapdensity",65),("mapflag",66),("mapsetting",67),("mapvisibility",68),("pathingtype",69),("placement",70),("playercolor",71),("playergameresult",72),("playerscore",73),("playerslotstate",74),("playerstate",75),("race",76),("racepreference",77),("raritycontrol",78),("soundtype",79),("startlocprio",80),("terraindeformation",81),("texmapflags",82),("texttag",83),("triggeraction",84),("ubersplat",85),("unitpool",86),("unitstate",87),("unittype",88),("version",89),("volumegroup",90),("weapontype",91),("weathereffect",92),("code",93),("string",94),("real",95),("integer",96),("boolean",97),("nothing",98)]

