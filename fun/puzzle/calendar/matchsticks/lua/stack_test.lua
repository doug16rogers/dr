stack = require('stack')

st = stack.new()
st:push(11):push(23):push(57)
print('count', st:count())
print('pop3:', st:pop(), st:pop(), st:pop())
print('count', st:count())
