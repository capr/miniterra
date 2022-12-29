--go@ x:\sdk\bin\windows\luajit.exe miniterra_test.lua

require'glue'

if ... ~= 'miniterra' then --cli
	print'Usage: miniterra FILE.mt'
	exit(1)
end

local
	type, push, pop, concat, tuple, unpack =
	type, push, pop, concat, tuple, unpack

local mt = {}

function mt.struct(t)
	t.type = 'type'
	t.kind = 'struct'
	return t
end

function mt.type(t)
	if not t.type then --tuple
		t = tuple(unpack(t))
		t.type = 'type'
		t.kind = 'tuple'
	end
	return t
end

function mt.func(t)
	t.type = 'func'
	return t
end

function mt.functype(t)
	t.type = 'type'
	t.kind = 'func'
	return t
end

local function add(t, v)
	push(t, v)
	return #t
end

local function expression_function(lx)

	local cur = lx.cur
	local next = lx.next
	local nextif = lx.nextif
	local expect = lx.expect
	local expectval = lx.expectval
	local errorexpected = lx.errorexpected
	local expectmatch = lx.expectmatch
	local line = lx.line
	local luaexpr = lx.luaexpr
	local name = lx.name
	local luastats = lx.luastats
	local symbol = lx.symbol
	local begin_scope = lx.begin_scope
	local end_scope = lx.end_scope
	local val = lx.val

	local function isend() --check for end of block
		local tk = cur()
		return tk == 'else' or tk == 'elseif' or tk == 'end'
			or tk == 'until' or tk == '<eof>'
	end

	local function expectname()
		return expectval'<name>'
	end

	local expr, block -- fw. decl.

	local function terratype()
		local bind_type = luaexpr()
		if nextif'->' then
			--TODO: this is wrong: operator priority > than that of `and` and `or`
			--TODO: remove this after implementing extensible operators.
			local bind_rettype = luaexpr()
			return function()
				return mt.functype({
					args_type = bind_type(),
					ret_type  = bind_rettype(),
				})
			end
		else
			return function()
				local t = bind_type()
				return mt.type(t)
			end
		end
	end

	local function funcdecl(name) --args_type [-> return_type]
		local bind_args_type = luaexpr()
		expect'->'
		local bind_ret_type = luaexpr()
		return function()
			return mt.func{
				name = name,
				args_type = bind_args_type(),
				ret_type = bind_ret_type(),
			}
		end
	end

	local function enter_scope()
		begin_scope()
	end

	local function exit_scope()
		end_scope()
	end

	local cur_block

	local function bind_all(t)
		for k,v in pairs(t) do
			if type(v) == 'function' then
				t[k] = v()
			elseif type(v) == 'table' then
				bind_all(v)
			end
		end
	end

	local function funcdef(name, line, pos)

		--params: (name:type,...[,...])
		local tk = expect'('
		local f = {name = name, arg_names = {}, args_type = {}}
		if tk ~= ')' then
			repeat
				if tk == '<name>' then
					local name = expectname()
					expect':'
					local bind_type = luaexpr()
					add(f.arg_names, name)
					add(f.args_type, bind_type)
				elseif tk == '...' then
					f.vararg = true
					next()
					break
				else
					errorexpected'<name> or "..."'
				end
				tk = nextif','
			until not tk
		end
		expectmatch(')', 'terra', line, pos)

		--return type: [:type]
		if nextif':' then
			f.ret_type = luaexpr()
		end

		--body
		cur_block = f
		block()
		if cur() ~= 'end' then
			expectmatch('end', 'terra', line, pos)
		end
		next()
		return function()
			bind_all(f)
			return mt.func(f)
		end
	end

	local function struct(name, line, pos)
		local bind = noop
		local tk = expect'{'
		local t = {name = name, fields = {}}
		while tk ~= '}' do
			local name = expectname()
			expect':'
			local bind_type = terratype()
			add(t.fields, name)
			add(t.fields, bind_type)
			tk = nextif','
			if not tk then break end
		end
		expectmatch('}', 'struct', line, pos)
		return function()
			bind_all(t)
			return mt.struct(t)
		end
	end

	local function expr_field() --.:name
		next()
		expectname()
	end

	local function expr_bracket() --[expr]
		next()
		expr()
		expect']'
	end

	local function expr_struct_initializer() --{[expr]|name=expr,;...}
		local line, pos = line()
		local tk = expect'{'
		while tk ~= '}' do
			if tk == '[' then
				expr_bracket()
				expect'='
			elseif tk == '<name>' and lookahead() == '=' then
				expectname()
				expect'='
			end
			expr()
			if not nextif',' and not nextif';' then break end
			tk = cur()
		end
		expectmatch('}', '{', line, pos)
	end

	local function expr_list(t) --expr,...
		expr(t)
		while nextif',' do
			expr(t)
		end
	end

	local function args() --(expr,...)|{struct_initializer}|string
		local tk = cur()
		if tk == '(' then
			local line, pos = line()
			tk = next()
			if tk == ')' then --f()
			else
				expr_list()
			end
			expectmatch(')', '(', line, pos)
		elseif tk == '{' then
			expr_struct_initializer()
		elseif tk == '<string>' then
			next()
		else
			errorexpected'function arguments'
		end
	end

	local function expr_primary() --(expr)|name .name|[expr]|:nameargs|args ...
		local iscall
		--parse prefix expression.
		local tk = cur()
		local t
		if tk == '(' then
			local line, pos = line()
			next()
			t = expr()
			expectmatch(')', '(', line, pos)
		elseif tk == '<name>' then
			t = {type = 'name', name = val(), val = name()}
		else
			error('unexpected symbol '..tk)
		end
		local tk = cur()
		while true do --parse multiple expression suffixes.
			if tk == '.' then
				expr_field()
				iscall = false
			elseif tk == '[' then
				expr_bracket()
				iscall = false
			elseif tk == ':' then
				next()
				expectname()
				args()
				iscall = true
			elseif tk == '(' or tk == '<string>' or tk == '{' then
				args()
				iscall = true
			else
				break
			end
			tk = cur()
		end
		return t, iscall
	end

	local function expr_simple() --literal|...|{struct_initializer}|expr_primary
		local tk = cur()
		if tk == '<number>' or tk == '<imag>' or tk == '<int>' or tk == '<u32>'
			or tk == '<i64>' or tk == '<u64>' or tk == '<string>' or tk == 'nil'
			or tk == 'true' or tk == 'false' or tk == '...'
		then --literal
			local t = {type = 'literal', val = val()}
			next()
			return t
		elseif tk == '{' then --{struct_initializer}
			return expr_struct_initializer()
		else
			return expr_primary()
		end
	end

	expr = lx.expression_parser{
		unary = 'not - #',
		priority = {
			'or',
			'and',
			'->',
			'> >= < <= ~= ==',
			'>> << xor',
			'..',
			'- +',
			'% / *',
			'not - #',
			'^',
		},
		right_associative = '^ ..',
		operand = expr_simple,
		operation = function(op, v1, v2)
			return {type = 'op', op = op, v1 = v1, v2 = v2}
		end,
	}

	local function assignment() --expr_primary,... = expr,...
		if nextif',' then --collect LHS list and recurse upwards.
			expr_primary()
			assignment()
		else --parse RHS.
			expect'='
			expr_list()
		end
	end

	local function label() --::name::
		next()
		expectname()
		local tk = expect'::'
		--recursively parse trailing statements: labels and ';' (Lua 5.2 only).
		while true do
			if tk == '::' then
				label()
			elseif tk == ';' then
				next()
			else
				break
			end
			tk = cur()
		end
	end

	--parse a statement. returns true if it must be the last one in a chunk.
	local function stmt()
		local tk = cur()
		if tk == 'if' then --if expr then block [elseif expr then block]... [else block] end
			local line, pos = line()
			local stmt = {type = 'if'}
			next()
			stmt.cond = expr()
			expect'then'
			local outer_block = cur_block
			add(cur_block, stmt)
			cur_block = stmt
			block()
			cur_block = outer_block
			tk = cur()
			while tk == 'elseif' do --elseif expr then block...
				next()
				expr()
				expect'then'
				block()
				tk = cur()
			end
			if tk == 'else' then --else block
				next()
				block()
			end
			expectmatch('end', 'if', line, pos)
		elseif tk == 'while' then --while expr do block end
			local line, pos = line()
			next()
			expr()
			expect'do'
			block()
			expectmatch('end', 'while', line, pos)
		elseif tk == 'do' then  --do block end
			local line, pos = line()
			next()
			block()
			expectmatch('end', 'do', line, pos)
		elseif tk == 'for' then
			--for name = expr, expr [,expr] do block end
			--for name,... in expr,... do block end
			local line, pos = line()
			next()
			expectname()
			local tk = cur()
			if tk == '=' then -- = expr, expr [,expr]
				next()
				expr()
				expect','
				expr()
				if nextif',' then expr() end
			elseif tk == ',' or tk == 'in' then -- ,name... in expr,...
				while nextif',' do
					expectname()
				end
				expect'in'
				expr_list()
			else
				errorexpected'"=" or "in"'
			end
			expect'do'
			block()
			expectmatch('end', 'for', line, pos)
		elseif tk == 'repeat' then --repeat block until expr
			local line, pos = line()
			next()
			block(false)
			expectmatch('until', 'repeat', line, pos)
			expr() --parse condition (still inside inner scope).
			exit_scope()
		elseif tk == 'terra' then --terra name body  |  terra name functype
			local line, pos = line()
			next()
			local name, next_tk = expectname()
			if next_tk == '::' then
				next()
				funcdecl(name)
			else
				funcdef(name, line, pos)
			end
		elseif tk == 'var' then
			--var name1[:type1],...[=expr1],...
			local line, pos = line()
			next()
			repeat --name[:type],...
				local name = expectname()
				local var = {type = 'var'}
				add(cur_block, var)
				symbol(name, var)
				if nextif':' then
					var.var_type = luaexpr()
				end
			until not nextif','
			if nextif'=' then -- =expr,...
				local exprs = {}
				expr_list(exprs)
			end
		elseif tk == 'return' then --return [expr,...]
			tk = next()
			if not (isend(tk) or tk == ';') then
				expr_list()
			end
			return true --must be last
		elseif tk == 'break' then
			next()
		elseif tk == ';' then
			next()
		elseif tk == '::' then
			label()
		elseif tk == 'goto' then --goto name
			next()
			expectname()
		elseif tk == '[' then --escape
			next()
			local stmt = luaexpr()
			add(cur_block, stmt)
			expect']'
		else
			local t, iscall = expr_primary()
			if not iscall then --function call or assignment
				assignment()
			end
		end
		return false
	end

	function block(do_exit_scope) --stmt[;]...
		enter_scope()
		local islast
		while not islast and not isend() do
			islast = stmt()
			nextif';'
		end
		if do_exit_scope ~= false then
			exit_scope()
		end
	end

	return function(_, tk, stmt)
		next()
		if tk == 'struct' then
			local line, pos = line()
			local name = stmt and expectval'<name>'
			return struct(name, line, pos), name and {name}
		elseif tk == 'terra' then
			local bind
			local line, pos = line()
			local name, next_tk
			if stmt then
				name, next_tk = expectname()
			end
			if next_tk == '::' then
				next()
				bind = funcdecl(name)
			else
				bind = funcdef(name, line, pos)
			end
			return bind, name and {name}
		elseif tk == 'quote' then
			--
		end
		assert(false)
	end
end

function mt.lang(lx)
	return {
		keywords = {'terra', 'quote', 'struct', 'var'},
		entrypoints = {
			statement = {'terra', 'struct'},
			expression = {'`'},
		},
		expression = expression_function(lx),
	}
end

function mt.format_expr(e)
	local t = {}
	local add = table.insert
	local function format(e)
		if e.type == 'op' then
			add(t, '(')
			if e.v1 then format(e.v1); add(t, ' ') end
			add(t, e.op)
			if e.v2 then add(t, ' '); format(e.v2) end
			add(t, ')')
		elseif e.type == 'name' then
			add(t, e.name)
		elseif e.type == 'literal' then
			add(t, e.val)
		end
	end
	format(e)
	return concat(t)
end

return mt
