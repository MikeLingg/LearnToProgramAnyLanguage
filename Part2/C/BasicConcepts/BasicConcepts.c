#include <limits.h>
#include <float.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>

/* ANSI 24-bit (RGB) color code format: \x1b[38;2;R;G;Bm */
#define COLOR_RESET "\x1b[0m"
#define COLOR_BOLD  "\x1b[1m"

void print_bg_rgb_color(uint8_t r, uint8_t g, uint8_t b, const char* text) {
    printf("\x1b[48;2;%d;%d;%dm%s" COLOR_RESET, r, g, b, text);
}

/* Map a value in range [0.0, 1.0] to RGB color gradient (blue -> green -> red) */
void value_to_color(double normalized, uint8_t* r, uint8_t* g, uint8_t* b) {
    if (normalized < 0.5) {
        /* Blue to Green */
        double t = normalized * 2.0;
        *r = 0;
        *g = (uint8_t)(t * 255);
        *b = (uint8_t)((1.0 - t) * 255);
    } else {
        /* Green to Red */
        double t = (normalized - 0.5) * 2.0;
        *r = (uint8_t)(t * 255);
        *g = (uint8_t)((1.0 - t) * 255);
        *b = 0;
    }
}

void demonstrate_8bit() {
    printf("\n%s=== 8-BIT UNSIGNED (0-255) ===%s\n", COLOR_BOLD, COLOR_RESET);
    printf("256 possible values - Coarse granularity\n\n");
    
    uint16_t values[] = {10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 127, 255};
    int count = sizeof(values) / sizeof(values[0]);
    
    for (int i = 0; i < count; i++) {
        uint16_t val = values[i];
        double normalized = (double)val / 255.0;
        uint8_t r, g, b;
        value_to_color(normalized, &r, &g, &b);
        
        printf("  %3d: ", val);
        print_bg_rgb_color(r, g, b, "    ");
        printf(" RGB(%3d,%3d,%3d)\n", r, g, b);
    }
}

void demonstrate_16bit() {
    printf("\n%s=== 16-BIT UNSIGNED (0-65535) ===%s\n", COLOR_BOLD, COLOR_RESET);
    printf("65,536 possible values - 256x finer than 8-bit\n\n");
    
    uint32_t values[] = {10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 3000, 6000, 9000, 12000, 15000, 18000, 21000, 24000, 27000, 32767, 65535};
    int count = sizeof(values) / sizeof(values[0]);
    
    for (int i = 0; i < count; i++) {
        uint32_t val = values[i];
        double normalized = (double)val / 65535.0;
        uint8_t r, g, b;
        value_to_color(normalized, &r, &g, &b);
        
        printf("  %5d: ", val);
        print_bg_rgb_color(r, g, b, "    ");
        printf(" RGB(%3d,%3d,%3d)\n", r, g, b);
    }
}

void demonstrate_32bit() {
    printf("\n%s=== 32-BIT UNSIGNED (0-4294967295) ===%s\n", COLOR_BOLD, COLOR_RESET);
    printf("4,294,967,296 possible values - 65,536x finer than 16-bit\n\n");
    
    uint64_t values[] = {10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 250000000, 500000000, 1000000000, 1500000000, 2000000000,
                         2147483647ULL, 4294967295ULL};
    int count = sizeof(values) / sizeof(values[0]);
    
    for (int i = 0; i < count; i++) {
        uint64_t val = values[i];
        double normalized = (double)val / 4294967295.0;
        uint8_t r, g, b;
        value_to_color(normalized, &r, &g, &b);
        
        printf("  %10llu: ", (unsigned long long)val);
        print_bg_rgb_color(r, g, b, "    ");
        printf(" RGB(%3d,%3d,%3d)\n", r, g, b);
    }
}

void demonstrate_64bit() {
    printf("\n%s=== 64-BIT UNSIGNED (0-18446744073709551615) ===%s\n", COLOR_BOLD, COLOR_RESET);
    printf("18,446,744,073,709,551,616 possible values - 4,294,967,296x finer than 32-bit\n\n");
    
    uint64_t values[] = {10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 625000000000000000ULL, 1250000000000000000ULL, 2500000000000000000ULL, 5000000000000000000ULL,
                         9223372036854775807ULL, 18446744073709551615ULL};
    int count = sizeof(values) / sizeof(values[0]);
    
    for (int i = 0; i < count; i++) {
        uint64_t val = values[i];
        double normalized = (double)val / 18446744073709551615.0;
        uint8_t r, g, b;
        value_to_color(normalized, &r, &g, &b);
        
        printf("  %20llu: ", (unsigned long long)val);
        print_bg_rgb_color(r, g, b, "    ");
        printf(" RGB(%3d,%3d,%3d)\n", r, g, b);
    }
}

int main()
{
    // This signifies a number of basic concepts that would be included in one program, 
    // except some of these concepts will prevent compilation or crash the program. 
    // So this program will be broken up as specific languages require it.

    bool falseBoolean = false;
    bool trueBoolean = true;
    
    printf ( "Boolean range: %d %d\n", falseBoolean, trueBoolean );
    
    signed char minSigned8 = -128;
    signed char maxSigned8 = 127;
    unsigned char minUnsigned8 = 0;
    unsigned char maxUnsigned8 = 255;
    
    printf ( "8 bit signed int range: %d %d\n", minSigned8, maxSigned8 );
    printf ( "8 bit unsigned int range: %u %u\n", minUnsigned8, maxUnsigned8 );
    
    short minSigned16 = -32768;
    short maxSigned16 = 32767;
    unsigned short minUnsigned16 = 0;
    unsigned short maxUnsigned16 = 65535;
    
    printf ( "16 bit signed int range: %d %d\n", minSigned16, maxSigned16 );
    printf ( "16 bit unsigned int range %u %u\n", minUnsigned16, maxUnsigned16 );
    
    int minSigned32 = -2147483648;
    int maxSigned32 = 2147483647;
    unsigned int minUnsigned32 = 0;
    unsigned int maxUnsigned32 = 4294967295;
    
    printf ( "32 bit signed int range: %d %d\n", minSigned32, maxSigned32 );
    printf ( "32 bit unsigned int range: %u %u\n", minUnsigned32, maxUnsigned32 );
    
    printf ( "Note: LLONG_MIN is used instead of -9,223,372,036,854,775,808 as C++ will first try to set +9,223,372,036,854,775,808, which is out of range, before applying the -.\n" );
    long long minSigned64 = LLONG_MIN;
    long long maxSigned64 = 9223372036854775807;
    unsigned long long minUnsigned64 = 0ULL;
    unsigned long long maxUnsigned64 = 18446744073709551615ULL;
    
    printf ( "64 bit signed int range: %lld %lld\n", minSigned64, maxSigned64 );
    printf ( "64 bit unsigned int range: %llu %llu\n", minUnsigned64, maxUnsigned64 );
    
    demonstrate_8bit();
    demonstrate_16bit();
    demonstrate_32bit();
    demonstrate_64bit();

    float floatMax = FLT_MAX;
    float floatMin = FLT_MIN;
    
    printf ( "Note that scientific notation must be used to print such a small number.\n" );
    printf ( "32 bit float: %e %f\n", floatMin, floatMax );
    
    float zeroPointOne = 0.1f;
    float zeroPointTwo = 0.2f;
    float zeroPointThree = 0.3f;
    
    // So let's look at how far off the actual floating point value is from the value it was set to.
    printf ( "Floating point 0.1, 0.2, 0.3 -> %.17f and %.17f and %.17f\n", zeroPointOne, zeroPointTwo, zeroPointThree );
    
    double doubleMax = DBL_MAX;
    double doubleMin = DBL_MIN;
    
    printf ( "Note that scientific notation must be used to print such a small number.\n" );
    printf ( "64 bit float range: %e %f\n", doubleMin, doubleMax );
    
    // Shows the basics of ASCII characters, including special ones.
    // This should print 1 followed by a tab followed by 2, then on the next line print 3.
    char charOne = '1';
    char charTab = '\t';
    char singleQuotes = '\'';
    char charNewLine = '\n';
    char doubleQuotes = '\"';
    printf ( "Characters: %c%c%c%c%c\n", charOne, charTab, singleQuotes, charNewLine, doubleQuotes );
    
    // Show how printing as an integer, not a character, can be confusing
    printf ( "charOne as an integer: %d\n", charOne );
    
    // Note some languages may not compile this code so it will be in a separate file, others may throw warnings.
    bool outOfRangeBoolean = 55;
    
    printf ( "Out of range Boolean: %d\n", outOfRangeBoolean );
    
    // Note some languages may not compile this code so it will be in a separate file, others may throw warnings.
    short outOfRange = 100000;
    
    printf ( "Out of range value: %d\n", outOfRange );
    
    printf ( "Note that adding a small amount to FLT_MAX or DBL_MAX is lost in the precision, so the value is added to itself to be out of range.\n" );
    float outOfRangeFloat = FLT_MAX + FLT_MAX;
    double outOfRangeDouble = -DBL_MAX - DBL_MAX;
    
    printf ( "Out of range float and double: %f %f\n", outOfRangeFloat, outOfRangeDouble );

    char outOfRangeChar = 257;

    printf ( "Out of range char:%c\n", outOfRangeChar );

    return 0;
}
