import RPi.GPIO as GPIO
import sys
import time

pin=23
value=1

if (len(sys.argv) > 1):
    pin = int(sys.argv[1])
if (len(sys.argv) > 2):
    value = int(sys.argv[2])
if value != 0:
    value = 1

print("Setting pin " + str(pin) + " to " + str(value) + ".")

GPIO.setmode(GPIO.BCM)     # Alternative is GPIO.BOARD for pin naming.
GPIO.setup(pin, GPIO.OUT)
GPIO.output(pin, value)
# GPIO.cleanup()
