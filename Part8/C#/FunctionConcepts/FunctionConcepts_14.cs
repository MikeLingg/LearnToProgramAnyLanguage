// Main method with parameters to access command line arguments
using System;

class Program
{
    static void Main ( string[] args )
    {
        Console.WriteLine ( "Number of arguments: {0}", args.Length );
        Console.WriteLine ( "Arguments:" );
        for ( int i = 0; i < args.Length; i++ )
        {
            Console.WriteLine ( "\tArgument {0}: {1}", i, args[i] );
        }
    }
}
