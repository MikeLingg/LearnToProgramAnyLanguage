using System;

class Program
{
    static void Main()
    {
        // Don't forget to declare your variables as appropriate to the language, some languages will fail to compile with this program
        char validName = 'a';
        char wrongCase = 'a';
        char wrOngLetter = 'a';
        
        // This will cause compile error - undefined variable
        Console.WriteLine ( "{0}", invalidName );
        // This will cause compile error - undefined variable
        Console.WriteLine ( "{0}", validname );
        // This will cause compile error - undefined variable
        Console.WriteLine ( "{0}", wrongcase );
        // This will cause compile error - undefined variable
        Console.WriteLine ( "{0}", wr0ngLetter );
     
        // Don't start your variables with numbers or use hyphens
        // This will cause compile error - invalid identifier
        int 2NameInvalid = 5;
        // This will cause compile error - invalid identifier
        char invalid-name = 'a';
        
        Console.WriteLine ( "{0}", 2NameInvalid );
        Console.WriteLine ( "{0}", invalid-name );
        
        // Also avoid using keywords already reserved by the programming language
        // This will cause compile error - reserved keyword
        int class = 1;
        // This will cause compile error - reserved keyword
        int namespace = 2;
        
        Console.WriteLine ( "{0}", class );
        Console.WriteLine( "{0}", namespace );
    }
}
