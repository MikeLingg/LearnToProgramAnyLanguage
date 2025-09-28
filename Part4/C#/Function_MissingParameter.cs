using System;

class Program
{
    static void Main()
    {
        string userInput;
        
        Console.WriteLine("Type 1 and press enter.");
        userInput = Console.ReadLine();
        
        // Function requires 1 parameter, but we're passing 0
        int enteredInteger = int.Parse();
        
        Console.WriteLine("The user entered the integer {0}", enteredInteger);
    }
}
