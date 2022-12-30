--go@ x:\sdk\bin\windows\luajit.exe ..\tests\miniterra_test.lua
--[[

	Miniterra: a Terra dialect with a C backend.
	Written by Cosmin Apreutesei. Public Domain.

	local mt = require'miniterra'
	package.miniterrapath = '...'      searchpath for .mt files
	require'foo' --loads `foo.mt`

]]

require'glue'
local lx = require'lx'

local
	type, add, push, pop, concat, tuple, unpack =
	type, add, push, pop, concat, tuple, unpack

local mt = {} --miniterra module

--Terra compiler -------------------------------------------------------------

function mt.struct(t)
	t.type = 'type'
	t.terra_type = 'struct'
	return t
end

function mt.type(t)
	t.type = 'type'
	return t
end

function mt.func(t)
	t.type = 'func'
	return t
end

function mt.functype(t)
	t.type = 'type'
	t.terra_type = 'func'
	return t
end

mt.op_class = {}
mt.op_class.__index = mt.op_class

function mt.op(t)
	return setmetatable(t, mt.op_class)
end

function mt.op_class:tostring()
	local t = {}
	local function format(e)
		if e.type == 'binop' then
			add(t, '(')
			format(e.lhs)
			add(t, ' ')
			add(t, e.op)
			add(t, ' ')
			format(e.rhs)
			add(t, ')')
		elseif e.type == 'unop' then
			add(t, '(')
			add(t, e.op)
			add(t, ' ')
			format(e.arg)
			add(t, ')')
		elseif e.type == 'name' then
			add(t, e.name)
		elseif e.type == 'literal' then
			add(t, e.val)
		end
	end
	format(self)
	return concat(t)
end

--Terra parser as an lx embedded language ------------------------------------

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
				if not t.type then
					t = tuple(unpack(t))
					t.terra_type = 'tuple'
				end
				return mt.type(t)
			end
		end
	end

	local function functype(name) --args_type [-> return_type]
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
		local f = {name = name, arg_names = {}, args_type = {}, line = line, pos = pos}
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
		block(f)
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
		local t = {name = name, fields = {}, line = line, pos = pos}
		while tk ~= '}' do
			local field = {}
			add(t.fields, field)
			field.name = expectname()
			expect':'
			field.type = terratype()
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
		return expectname()
	end

	local function expr_bracket() --[expr]
		next()
		local t = expr()
		expect']'
		return t
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

	local function expr_list() --expr,...
		local t = {}
		add(t, expr())
		while nextif',' do
			add(t, expr())
		end
		return t
	end

	local function args() --(expr,...)|{struct_initializer}|string
		local tk = cur()
		local t
		if tk == '(' then
			local line, pos = line()
			tk = next()
			if tk == ')' then --f()
				t = empty
			else
				t = expr_list()
			end
			expectmatch(')', '(', line, pos)
		elseif tk == '{' then
			t = expr_struct_initializer()
		elseif tk == '<string>' then
			t = {val()}
			next()
		else
			errorexpected'function arguments'
		end
		return t
	end

	local function expr_primary() --(expr)|name  .name|[expr]|:nameargs|args ...
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
		while true do --parse multiple expression suffixes.
			local tk = cur()
			if tk == '.' then
				t = {type = 'indexing', target = t}
				t.key = expr_field()
				iscall = false
			elseif tk == '[' then
				t = {type = 'indexing', target = t}
				t.key = expr_bracket()
				iscall = false
			elseif tk == ':' then
				t = {type = 'methodcall', target = t}
				next()
				t.method = expectname()
				t.args = args()
				iscall = true
			elseif tk == '(' or tk == '<string>' or tk == '{' then
				t = {type = 'call'}
				t.args = args()
				iscall = true
			else
				break
			end
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
		unary = 'not - # &',
		priority = {
			'or',
			'and',
			'->',
			'> >= < <= ~= ==',
			'>> << xor',
			'..',
			'- +',
			'% / *',
			'not - # &',
			'^',
		},
		right_associative = '^ ..',
		operand = expr_simple,
		operation = function(op, v1, v2)
			if v2 == nil then
				return mt.op{type = 'unop', op = op, arg = v1}
			else
				return mt.op{type = 'binop', op = op, lhs = v1, rhs = v2}
			end
		end,
	}

	local function assignment(stmt) --expr_primary,... = expr,...
		while nextif',' do
			add(stmt.lhs, expr_primary())
		end
		expect'='
		stmt.rhs = expr_list()
		add(cur_block, stmt)
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
			next()
			local stmt = {type = 'if'}
			stmt.cond = expr()
			expect'then'
			add(cur_block, stmt)
			block(stmt)
			while cur() == 'elseif' do --elseif expr then block...
				next()
				local stmt = {type = 'elseif'}
				stmt.cond = expr()
				expect'then'
				add(cur_block, stmt)
				block(stmt)
			end
			if cur() == 'else' then --else block
				next()
				local stmt = {type = 'else'}
				add(cur_block, stmt)
				block(stmt)
			end
			expectmatch('end', 'if', line, pos)
		elseif tk == 'while' then --while expr do block end
			local line, pos = line()
			next()
			local stmt = {type = 'while'}
			stmt.cond = expr()
			expect'do'
			add(cur_block, stmt)
			block(stmt)
			expectmatch('end', 'while', line, pos)
		elseif tk == 'do' then  --do block end
			local line, pos = line()
			next()
			local stmt = {type = 'do'}
			add(cur_block, stmt)
			block(stmt)
			expectmatch('end', 'do', line, pos)
		elseif tk == 'for' then
			--for name = expr, expr [,expr] do block end
			--for name,... in expr,... do block end
			local line, pos = line()
			next()
			local stmt = {type = 'for'}
			add(cur_block, stmt)
			stmt.var = expectname()
			local tk = cur()
			if tk == '=' then -- = expr, expr [,expr]
				next()
				stmt.from = expr()
				expect','
				stmt.to = expr()
				if nextif',' then
					stmt.step = expr()
				end
			elseif tk == ',' or tk == 'in' then -- ,name... in expr,...
				stmt.vars = {}
				while nextif',' do
					add(stmt.vars, expectname())
				end
				expect'in'
				stnt.iter = expr_list()
			else
				errorexpected'"=" or "in"'
			end
			expect'do'
			block(stmt)
			expectmatch('end', 'for', line, pos)
		elseif tk == 'repeat' then --repeat block until expr
			local line, pos = line()
			next()
			local stmt = {type = 'repeat'}
			add(cur_block, stmt)
			block(stmt, false)
			expectmatch('until', 'repeat', line, pos)
			stmt.cond = expr() --parse condition (still inside inner scope).
			exit_scope()
		elseif tk == 'terra' then --terra name body  |  terra name functype
			local line, pos = line()
			next()
			local name, next_tk = expectname()
			if next_tk == '::' then
				next()
				functype(name)
			else
				funcdef(name, line, pos)
			end
		elseif tk == 'var' then --var name1[:type1],...[=expr1],...
			local line, pos = line()
			next()
			local vars = {}
			repeat --name[:type],...
				local var = {type = 'var'}
				var.name = expectname()
				add(vars, var)
				if nextif':' then
					var.var_type = luaexpr()
				end
			until not nextif','
			if nextif'=' then -- =expr,...
				for i,expr in ipairs(expr_list()) do
					vars[i].val = expr
				end
			end
			for i,var in ipairs(vars) do
				symbol(var.name, var)
				add(cur_block, var)
			end
		elseif tk == 'return' then --return [expr,...]
			tk = next()
			if not (isend(tk) or tk == ';') then
				expr_list()
			end
			return true --must be last
		elseif tk == 'break' then
			add(cur_block, 'break')
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
				assignment({type = 'assignment', lhs = {t}})
			end
		end
		return false
	end

	function block(new_block, do_exit_scope) --stmt[;]...
		local outer_block = cur_block
		cur_block = new_block
		enter_scope()
		local islast
		while not islast and not isend() do
			islast = stmt()
			nextif';'
		end
		if do_exit_scope ~= false then
			exit_scope()
		end
		cur_block = outer_block
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
				bind = functype(name)
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
			statement = {'terra', 'struct', 'var'},
			expression = {'`'},
		},
		expression = expression_function(lx),
	}
end

mt.env = setmetatable({}, {__index = _G})

mt.env.i32  = mt.type{terra_type = 'i32', ctype = 'i32'}
mt.env.i16  = mt.type{terra_type = 'i16', ctype = 'i16'}
mt.env.i8   = mt.type{terra_type = 'i8' , ctype = 'i8' }
mt.env.u32  = mt.type{terra_type = 'u32', ctype = 'u32'}
mt.env.u16  = mt.type{terra_type = 'i16', ctype = 'u16'}
mt.env.u8   = mt.type{terra_type = 'u8' , ctype = 'u8' }
mt.env.int  = mt.env.i32
mt.env.uint = mt.env.u32
mt.env.bool = mt.type{terra_type = 'bool', ctype = 'i8'}

--require loader for .mt files -----------------------------------------------

push(package.loaders, function(name)
	local paths = package.miniterrapath or package.path:gsub('%.lua', '.mt')
	local path, err = package.searchpath(name, paths)
	if not path then return nil, err end
	return function()
		local f = assert(io.open(path, 'r'))
		local s, err = f:read'*a'
		f:close()
		assert(s, err)
		local lx = lx.lexer(s, path)
		lx.import'miniterra'
		local chunk = lx.load(mt.env)
		return chunk()
	end
end)

--cmdline --------------------------------------------------------------------

if ... ~= 'miniterra' then --cli
	print'Usage: miniterra FILE.mt'
	exit(1)
end

return mt
