require'glue'
require'fs'
local lua_dir = indir(scriptdir(), '../lua')
local bin_dir = indir(scriptdir(), indir('../bin', win and 'windows' or 'linux'))
luapath(lua_dir)
luapath(scriptdir())
sopath(bin_dir)
--require'lx'
--require'miniterra_test_luax'
require'miniterra'
require'miniterra_test_mt'
