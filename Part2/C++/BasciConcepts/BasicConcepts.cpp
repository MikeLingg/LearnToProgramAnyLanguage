#include <cstdio>
#include <climits>
#include <cfloat>

int main()
{
    // This signifies a number of basic concepts that would be included in one program, 
    // except some of these concepts will prevent compilation or crash the program. 
    // So this program will be broken up as specific languages require it.

    bool falseBoolean = true;
    bool trueBoolean = false;
    
    printf ( "Boolean range: %d %d\n", falseBoolean, trueBoolean ) ;
    
    signed char minSigned8 = -128;
    signed char maxSigned8 = 127;
    unsigned char minUnsigned8 = 0;
    unsigned char maxUnsigned8 = 255;
    
    printf ( "8 bit signed int range: %d %d\n", minSigned8, maxSigned8 ) ;
    printf ( "8 bit unsigned int range: %u %u\n", minUnsigned8, maxUnsigned8 ) ;
    
    short minSigned16 = -32768;
    short maxSigned16 = 32767;
    unsigned short minUnsigned16 = 0;
    unsigned short maxUnsigned16 = 65535;
    
    printf ( "16 bit signed int range: %d %d\n", minSigned16, maxSigned16 ) ;
    printf ( "16 bit unsigned int range %u %u\n", minUnsigned16, maxUnsigned16 ) ;
    
    int minSigned32 = -2147483648;
    int maxSigned32 = 2147483647;
    unsigned int minUnsigned32 = 0;
    unsigned int maxUnsigned32 = 4294967295;
    
    printf ( "32 bit signed int range: %d %d\n", minSigned32, maxSigned32 ) ;
    printf ( "32 bit unsigned int range: %u %u\n", minUnsigned32, maxUnsigned32 ) ;
    
    printf ( "Note: LLONG_MIN is used instead of -9,223,372,036,854,775,808 as C++ will first try to set +9,223,372,036,854,775,808, which is out of range, before applying the -.\n" );
    long long minSigned64 = LLONG_MIN;
    long long maxSigned64 = 9223372036854775807;
    unsigned long long minUnsigned64 = 0ULL;
    unsigned long long maxUnsigned64 = 18446744073709551615ULL;
    
    printf ( "64 bit signed int range: %lld %lld\n", minSigned64, maxSigned64 ) ;
    printf ( "64 bit unsigned int range: %llu %llu\n", minUnsigned64, maxUnsigned64 ) ;
    
    float floatMax = FLT_MAX;
    float floatMin = FLT_MIN;
    
    printf ( "32 bit float: %f %f\n", floatMin, floatMax ) ;
    
    float zeroPointOne = 0.1f;
    float zeroPointTwo = 0.2f;
    float zeroPointThree = 0.3f;
    
    // So let's look at how far off the actual floating point value is from the value it was set to.
    printf ( "Floating point 0.1, 0.2, 0.3 -> %.17f and %.17f and %.17f\n", zeroPointOne, zeroPointTwo, zeroPointThree ) ;
    
    double doubleMax = DBL_MAX;
    double doubleMin = DBL_MIN;
    
    printf ( "64 bit float range: %f %f\n", doubleMin, doubleMax ) ;
    
    // Shows the basics of ASCII characters, including special ones.
    // This should print 1 followed by a tab followed by 2, then on the next line print 3.
    char charOne = '1';
    char charTab = '\t';
    char charTwo = '2';
    char charNewLine = '\n';
    char charThree = '3';
    printf ( "Characters: %c%c%c%c%c\n", charOne, charTab, charTwo, charNewLine, charThree ) ;
    
    // Show how printing as an integer, not a character, can be confusing
    printf ( "charOne as an integer: %d\n", charOne ) ;
    
    // Note some languages may not compile this code so it will be in a separate file, others may throw warnings.
    bool outOfRangeBoolean = 55;
    
    printf ( "Out of range Boolean: %d\n", outOfRangeBoolean ) ;
    
    // Note some languages may not compile this code so it will be in a separate file, others may throw warnings.
    short outOfRange = 100000;
    
    printf ( "Out of range value: %d\n", outOfRange ) ;
    
    printf ( "Note that adding a small amount to FLT_MAX or DBL_MAX is lost in the precision, so the value is added to itself to be out of range.\n" );
    float outOfRangeFloat = FLT_MAX + FLT_MAX;
    double outOfRangeDouble = DBL_MAX + DBL_MAX;
    
    printf ( "Out of range float and double: %f %f\n", outOfRangeFloat, outOfRangeDouble ) ;

    char outOfRangeChar = 257;

    printf ( "Out of range char:%c\n", outOfRangeChar );

    return 0;
}
