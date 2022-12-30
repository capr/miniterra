local mt = require'miniterra'
import'miniterra'

struct s {
	a: int,
	b: bool,
}

terra g :: {int, int} -> bool

terra h() end

terra f()
	--var i: int, j: bool = 5, i
	--var i: int = 5
	--var j: bool = i
	--a.b, b = c:m()+5, d
	--var a
	--if a < b then
	--else
	--end
	--g()
	--if a .. b .. c then end
	--if not i < x or j < y or k < z then end
	--if i < x .. j < y .. k < z then end
end

--[==[
print'hello'

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

pr(s)
pr(g)
pr(h)
pr(f)
--pr(f[2].cond:tostring())

