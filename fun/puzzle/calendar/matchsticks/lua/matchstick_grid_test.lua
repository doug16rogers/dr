
args = { ... }

if #args < 3 then
   print('')
   print('Usage: matchstick_grid_test <matchstick-descriptions> <row-burnt-counts> <column-burnt-counts>')
   print('')
   print('  Matchstick descriptions should be comma-delimited. Horizontal matches should')
   print('  start with a row number then have two letters representing the head column')
   print('  and tail column, in that order. Vertical matches should begin with a column')
   print('  letter followed by the two row numbers, head first.')
   print('')
   print('Example: lua matchstick_grid_test.lua 1ba,a32,b23,c31 122 212')
   print('')
   print('** NOTE ** This does not yet solve the puzzle. Actually, I switched to Prolog.')
   print('')
   os.exit(1)
end

MSG = require('matchstick_grid')
g = MSG.new(args[1], args[2], args[3])
assert(g)
-- @todo: Load these from the command line as optional arguments:
g:burn_matchstick_segments(1, 1)
g:burn_matchstick_segments(2, 2)
g:display()
