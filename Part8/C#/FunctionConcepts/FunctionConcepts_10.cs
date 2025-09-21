// Infinite recursive function
using System;

class Program
{
    static void RecursiveFunction ()
    {
        RecursiveFunction ();
    }

    static void Main ()
    {
        RecursiveFunction ();
    }
}
