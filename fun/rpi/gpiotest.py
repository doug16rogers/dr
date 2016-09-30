import RPi.GPIO as GPIO
import sys
import time

pin=23
count=10

if (len(sys.argv) > 1):
    pin = int(sys.argv[1])
if (len(sys.argv) > 2):
    count = int(sys.argv[2])

print("Toggling pin " + str(pin) + " " + str(count) + " times.")

GPIO.setmode(GPIO.BCM)     # Alternative is GPIO.BOARD for pin naming.
GPIO.setup(pin, GPIO.OUT)

for i in range(1, count + 1):
    plural = "y" if (i == 1) else "ies"
    print(str(i) + " blinky blink" + plural)
    GPIO.output(pin, 1)
    time.sleep(0.5)
    GPIO.output(pin, 0)
    time.sleep(0.5)

GPIO.cleanup()
