#include <cstdio>
#include <climits>
#include <cfloat>

int main()
{
    // Don't forget to declare your variables as appropriate to the language, some languages will fail to compile with this program
    char validName = 'a';
    char wrongCase = 'a';
    char wrOngLetter = 'a';
    
    printf ( "%c\n", invalidName );
    printf ( "%c\n", validname );
    printf ( "%c\n", wrongcase );
    printf ( "%c\n", wr0ngLetter );
 
    // Don't start your variables with numbers or use hyphens
    int 2NameInvalid = 5;
    char invalid-name = 'a';
    
    printf ( "%d\n", 2NameInvalid );
    printf ( "%c\n", invalid-name );
    
    // Also avoid using keywords already reserved by the programming language
    int class = 1;
    int private = 2;
    
    printf ( "%d\n", class );
    printf ( "%d\n", private );

    return 0;
}
