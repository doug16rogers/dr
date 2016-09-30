import time
import sys

count=5
if (len(sys.argv) > 1):
    count = int(sys.argv[1])

print("counting to " + str(count))

for i in range(1, count):
    print("count " + str(i))
    time.sleep(1)
print("count " + str(count))
