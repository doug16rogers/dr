#!/usr/bin/env /usr/local/bin/lua

-- Test driver for PostScript module.
PS = require('PostScript')

ps = PS.new()

p0 = { x = 72 * 1.0, y = 72 *  1.0 }
p1 = { x = 72 * 7.5, y = 72 * 10.0 }

ps:move(p0.x, p0.y):linex(p1.x):liney(p1.y):linex(p0.x):liney(p0.y):stroke()
ps:move(100, 100):linex(300):liney(300):linex(100):liney(100):fill(0.5)
ps:setfont("Courier-Bold", 12)
ps:move(200-20,400-6):show("Howdy!"):close()
ps:move(200-20,200-6):setgray(1):show("Doody!"):close()
ps:showpage()

font_size = 18
ps:setfont("Times", font_size)
ps:setgray(0)
ps:move(72,10*72):show("Whenever I see that I'm stuck up a tree")
ps:movedy(-font_size):show("And don't know the right way to go,")
ps:movedy(-font_size):show("I turn to the sky with the sun in my eye,")
ps:movedy(-font_size):show("And leave all the dirt far below.")
ps:setfont("Times-Italic", font_size - 2)
ps:movedy(-2*font_size):show("   or from a different point of view...")
ps:setfont("Times", font_size)
ps:movedy(-2*font_size):show("Whenever I see that I'm stuck up a tree")
ps:movedy(-font_size):show("With tangles of brown at my sleeve,")
ps:movedy(-font_size):show("I turn to the limb with a thrust of my chin")
ps:movedy(-font_size):show("And wait for the branches to leave!")
ps:rmove(200,-2*font_size):show("-- Doug Rogers 3EE5")
ps:showpage()

local test_file = 'PostScriptTest.ps'
ps:writefile(test_file)
os.execute('evince ' .. test_file)

