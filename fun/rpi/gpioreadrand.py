import RPi.GPIO as GPIO
import sys
import time

pin=18
filename=''
file = sys.stdout
bytes_to_get = 0x00100000     # A MbByte.

if (len(sys.argv) > 1):
    pin = int(sys.argv[1])
if (len(sys.argv) > 2):
    filename = sys.argv[2]
    if filename != '':
        file = open(filename, 'wb')
        print('Writing random data to "' + filename + '".')
        # Add code to check for error.

if (len(sys.argv) > 3):
    bytes_to_get = int(sys.argv[3])

print("Reading pin " + str(pin) + ' for ' + str(bytes_to_get) + ' random bytes.')
GPIO.setmode(GPIO.BCM)     # Alternative is GPIO.BOARD for pin naming.
GPIO.setup(pin, GPIO.IN)
kSameCountReportLimit=10000
same_count = 0      # Number of consecutive matching pairs (that have the same value).
kPerLine = 32
col = 0
byte = 0
bit_count = 0
byte_count = 0
while (bytes_to_get == 0) or (byte_count < bytes_to_get):
    a = GPIO.input(pin)
    b = GPIO.input(pin)
    if a == b:
        same_count += 1
        if same_count == kSameCountReportLimit:
            sys.stdout.write(chr(10))
            print('**** Received ' + str(kSameCountReportLimit) + ' pairs of ' +
                  str(a) + 's in a row; ' +
                  'continuing to look for randomness... ****')
            # same_count = 0
    else:
        same_count = 0
        byte = (byte << 1) + a     # Or b. Doesn't matter.
        bit_count += 1
        if bit_count >= 8:
            byte_count += 1
            # if (byte_count % 100) == 0:
            #     print(str(byte_count) + ' bytes...')
            if file == sys.stdout:
                file.write(format(byte, '02X'))
                col += 1
                if col >= kPerLine:
                    col = 0
                    file.write(chr(10))
            else:
                file.write(chr(byte))
            bit_count = 0
            byte = 0
            file.flush()

if file != sys.stdout:
    file.close()

GPIO.cleanup()
