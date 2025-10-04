using System;

class Program
{
    static void Main( string[] args )
    {
        int score = 85;
        bool isValid = true;
        int value = 5;
        string text = "hello";
        
        // ERROR 1: Missing parentheses around condition
        if score >= 90
        {
            Console.Write( "You got an A\n" );
        }
        
        // ERROR 2: Using 'then' keyword (from other languages like VB)
        if ( score >= 90 ) then
        {
            Console.Write( "You got an A\n" );
        }
        
        // ERROR 3: Using 'elif' instead of 'else if' (Python influence)
        if ( score >= 90 )
        {
            Console.Write( "A\n" );
        }
        elif ( score >= 80 )
        {
            Console.Write( "B\n" );
        }
        
        // ERROR 4: Using 'elsif' instead of 'else if' (Ruby influence)
        if ( score >= 90 )
        {
            Console.Write( "A\n" );
        }
        elsif ( score >= 80 )
        {
            Console.Write( "B\n" );
        }
        
        // ERROR 5: Using 'elseif' (one word) instead of 'else if'
        if ( score >= 90 )
        {
            Console.Write( "A\n" );
        }
        elseif ( score >= 80 )
        {
            Console.Write( "B\n" );
        }
        
        // ERROR 6: Missing colon after case in switch
        switch ( value )
        {
            case 1
                Console.Write( "One\n" );
                break;
        }
        
        // ERROR 7: Using equals sign instead of colon in case
        switch ( value )
        {
            case = 1:
                Console.Write( "One\n" );
                break;
        }
        
        // ERROR 8: Missing break in switch (C# requires break, goto, return, or throw)
        switch ( value )
        {
            case 1:
                Console.Write( "One\n" );
            case 2:
                Console.Write( "Two\n" );
                break;
        }
        
        // ERROR 9: Multiple default cases in switch
        switch ( value )
        {
            case 1:
                Console.Write( "One\n" );
                break;
            default:
                Console.Write( "First default\n" );
                break;
            default:
                Console.Write( "Second default\n" );
                break;
        }
        
        // ERROR 10: Duplicate case values in switch
        switch ( value )
        {
            case 1:
                Console.Write( "First one\n" );
                break;
            case 1:
                Console.Write( "Second one\n" );
                break;
        }
        
        // ERROR 11: Wrong comparison operator (=> instead of >=)
        if ( score => 90 )
        {
            Console.Write( "A\n" );
        }
        
        // ERROR 12: Wrong comparison operator (=< instead of <=)
        if ( score =< 90 )
        {
            Console.Write( "Too low\n" );
        }
        
        // ERROR 13: Using if without condition
        if
        {
            Console.Write( "Hello\n" );
        }
        
        // ERROR 14: Case outside of switch
        case 1:
            Console.Write( "One\n" );
            break;
        
        // ERROR 15: Default outside of switch
        default:
            Console.Write( "Default\n" );
            break;
        
        // ERROR 16: Break outside of switch or loop
        if ( score >= 90 )
        {
            Console.Write( "A\n" );
            break;
        }
        
        // ERROR 17: Variable shadowing (C# does not allow this)
        int variable = 10;
        if ( true )
        {
            int variable = 20;
            Console.Write( $"Variable inside: {variable}\n" );
        }
        
        // ERROR 18: Using 'True' or 'False' with capital T/F (C# is case-sensitive)
        if ( True )
        {
            Console.Write( "Hello\n" );
        }
        
        // ERROR 19: Using single & or | instead of && or ||
        if ( score >= 80 & score < 90 )
        {
            Console.Write( "B\n" );
        }
        
        // ERROR 20: Using 'and' or 'or' instead of && or || (C# doesn't support these keywords)
        if ( score >= 80 and score < 90 )
        {
            Console.Write( "B\n" );
        }
        
        // ERROR 21: Missing semicolon after statement inside if
        if ( score >= 90 )
        {
            Console.Write( "A\n" )
        }
        
        // ERROR 22: Using == for string comparison without proper method (works but not recommended)
        // This actually compiles but students should learn proper string comparison
        
        // ERROR 23: Unreachable code after return in switch case
        switch ( value )
        {
            case 1:
                return;
                Console.Write( "One\n" );
                break;
        }
    }
}
