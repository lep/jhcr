// scope API
// REQUIRES 

globals
	integer _cnt = 0
	trigger array _triggers

	constant integer _SUCCESS = 1
	constant integer _NO_DATA = 2
	constant integer _OP_LIMIT = 3
	integer _last_status = -1
endglobals

function _RegisterReload takes trigger _t returns nothing
	set _triggers[_cnt] = _t
	set _cnt = _cnt +1
endfunction

function _GetSeqNumber takes nothing returns integer
	return Init#_seq
endfunction

function _GetLastStatus takes nothing returns integer
	return _last_status
endfunction
