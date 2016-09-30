#include <unistd.h>

/**
 * For dit I chose the middle dot charater U+00B7, '·'.
 */
const char* kMiddleDotUtf8 = "\xC2\xB7";

/**
 * Another option is to use the bullet operator U+2022, '•'.  
 */
const char* kBulletOperatorUtf8 = "\xE2\x80\xA2";

/**
 * Yet another option is to use the dot operator U+22C5, '⋅'.  
 */
const char* kDotOperatorUtf8 = "\xE2\x8B\x85";

/**
 * Among others we could try the Katakana middle dot U+30FB, '・'. But that's
 * a wide character.
 */
const char* kKatakanaMiddleDotUtf8 = "\xE3\x83\xBB";

/**
 * The Morse code table - ITU 1677.
 */
char kMorseChart[] =
    "  A · =       N = ·       1 · = = = =   . · = · = · =    <AA> · = · =           New line            \n"
    "  B = · · ·   O = = =     2 · · = = =   ? · · = = · ·    <AR> · = · = ·         New page            \n"
    "  C = · = ·   P · = = ·   3 · · · = =   ; = · = · = ·    <AS> · = · · ·         Wait (&)            \n"
    "  D = · ·     Q = · = ·   4 · · · · =   ! = · = · = =    <BT> = · · · =         New paragraph       \n"
    "  E ·         R · = ·     5 · · · · ·   , = = · · = =    <CT> = · = · =         Attention           \n"
    "  F · · = ·   S · · ·     6 = · · · ·   : = = = · · ·    <HH> · · · · · · · ·   Error/????          \n"
    "  G = = ·     T =         7 = = · · ·   \" · = · · = ·     <K> = · =             Any station reply   \n"
    "  H · · · ·   U · · =     8 = = = · ·   ' · = = = = ·    <KN> = · = = ·         Named station reply \n"
    "  I · ·       V · · · =   9 = = = = ·   @ · = = · = ·    <SK> · · · = · =       End contact         \n"
    "  J · = = =   W · = =     0 = = = = =   _ · · = = · =    <SN> · · · = ·         Understood          \n"
    "  K = · =     X = · · =   / = · · = ·   + · = · = ·     <SOS> · · · = = = · · · Distress            \n"
    "  L · = · ·   Y = · = =   = = · · · =   - = · · · · =    <BK> = · · ·   = · =   Break               \n"
    "  M = =       Z = = · ·   ( = . = = .   ) = · = = · =    <CL> = · = ·   · = · · Closing             \n";

int main() {
    write(1, kMorseChart, sizeof(kMorseChart) - 1);
    return 0;
}
