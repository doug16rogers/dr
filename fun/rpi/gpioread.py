import RPi.GPIO as GPIO
import sys
import time

pin=18
count=1

if (len(sys.argv) > 1):
    pin = int(sys.argv[1])
if (len(sys.argv) > 2):
    count = int(sys.argv[2])

print("Reading pin " + str(pin) + " " + str(count) + " times.")
GPIO.setmode(GPIO.BCM)     # Alternative is GPIO.BOARD for pin naming.
GPIO.setup(pin, GPIO.IN)
i = 0
nils = 0
ones = 0
while i < count:
    value = GPIO.input(pin)
    if value == 0:
        nils = nils + 1
    else:
        ones = ones + 1
    i = i + 1

if count == 1:
    print("Pin " + str(pin) + " has value " + str(value) + ".")
else:
    value = 0
    if ones > nils:
       value = 1
    print("Pin " + str(pin) + " (" + str(nils) + ":" + str(ones) + ") has value " + str(value) + ".")
GPIO.cleanup()
