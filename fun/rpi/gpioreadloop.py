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
kPerLine=120
col=0
while True:
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

    if count != 1:
        value = 0
        if ones > nils:
           value = 1
    sys.stdout.write((chr(48 + value)))
    col = col + 1
    if col >= kPerLine:
       col = 0
       sys.stdout.write(chr(10))
    sys.stdout.flush()

GPIO.cleanup()
