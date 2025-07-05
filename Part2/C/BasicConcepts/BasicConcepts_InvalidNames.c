#include <stdio.h>

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
    int default = 1; // Maybe this should be defaultSize?
    int case = 2; // Maybe this should be binNumber?
    
    printf ( "%d\n", default );
    printf ( "%d\n", case );

    return 0;
}
