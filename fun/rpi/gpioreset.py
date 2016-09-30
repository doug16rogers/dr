import RPi.GPIO as GPIO
import sys
import time

pin=23

if (len(sys.argv) > 1):
    pin = int(sys.argv[1])

print("Resetting pin " + str(pin) + ".")

GPIO.setmode(GPIO.BCM)     # Alternative is GPIO.BOARD for pin naming.
GPIO.setup(pin, GPIO.IN)
GPIO.cleanup()
