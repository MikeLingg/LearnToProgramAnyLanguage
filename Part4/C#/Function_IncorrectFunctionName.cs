using System;

class Program
{
    static void Main()
    {
        string userInput;
        
        Console.WriteLine("Type 1 and press enter.");
        userInput = Console.ReadLine();
        
        // Function name is wrong - should be int.Parse
        int enteredInteger = int.ParseInt(userInput);
        
        Console.WriteLine("The user entered the integer {0}", enteredInteger);
    }
}
