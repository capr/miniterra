require'glue'
require'fs'
luapath(scriptdir())
sopath(indir(scriptdir(), indir('bin', win and 'windows' or 'linux')))
require'lx'
require'miniterra_test_luax'
