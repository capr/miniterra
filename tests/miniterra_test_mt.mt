--require'pp'
local mt = require'miniterra'
import'miniterra'

local b = {type = 'type'}

terra f()
	var a
	if a < b then end
	--if a .. b .. c then end
	--if not i < x or j < y or k < z then end
	--if i < x .. j < y .. k < z then end
end

--[==[
print'hello'

local int  = {type = 'type', ctype = 'int'}
local bool = {type = 'type', ctype = 'bool'}

struct S {

	x: int,
	y: bool,

}

terra g :: {int, int} -> bool

function add(sym)
	pr(sym)
end

terra f(a: bool, b: int) : {int, bool}

	var i: int, j: bool = 5, true

	if i == 5 then

		[ add(i) ]

	end

end

a = 2 + 3 / 5 ^ 7

--pr(S)
--pr(g)
--pr(f)
]==]

pr(f)
pr(mt.format_expr(f[2].cond))

