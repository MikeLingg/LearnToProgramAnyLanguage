// Example with three default parameters
using System;

class Program
{
    static void ThreeDefaultParameters ( int FirstParameter_Par = 5, int SecondParameter_Par = 10, int ThirdParameter_Par = 15 )
    {
        Console.WriteLine ( "Parameters: {0} {1} {2}", FirstParameter_Par, SecondParameter_Par, ThirdParameter_Par );
    }

    static void Main ()
    {
        ThreeDefaultParameters ( FirstParameter_Par: 20, SecondParameter_Par: 25, ThirdParameter_Par: 30 );
        // C# supports named parameters so can set any parameter by name
        ThreeDefaultParameters ( SecondParameter_Par: 25 );
    }
}
