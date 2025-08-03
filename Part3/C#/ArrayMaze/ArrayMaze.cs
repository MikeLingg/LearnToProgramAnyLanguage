using System;

class Program
{
    public static void Main()
    {
        // This maze program does not necessarily progress the program, 
        // but provides a construct we will use eventually in the program.
        char[ , ] mazeWalls = {
            { '|', ' ', '\0' },
            { ' ', ' ', ' ' },
            { ' ', '|', '\0' },
            { ' ', '-', ' ' },
            { '|', ' ', '\0' }
        };
        
        const int LEFT = 0;
        const int UP = 1;
        const int RIGHT = 2;
        const int DOWN = 3;
        
        int[ , ] neighborLookup = {
            { -1, -1, -1, 3 }, // Cell 0
            { -1, -1, 2, 4 },  // Cell 1
            { 1, -1, -1, 5 },  // Cell 2
            { -1, 0, 4, 6 },   // Cell 3
            { 3, 1, -1, -1 },  // Cell 4
            { -1, 2, -1, 8 },  // Cell 5
            { -1, 3, -1, -1 }, // Cell 6
            { -1, -1, 8, -1 }, // Cell 7
            { 7, 5, -1, -1 }   // Cell 8
        };
        
        char[ ] topBottomRow = { '+', '-', '+', '-', '+', '-', '+' };
        
        // This sort of code will look better when we have loops and functions
        // We see how a 3x3 maze is created from the array of data, and using the 
        // neighbor lookup provides -1 where a wall is in that direction, otherwise 
        // provides the cell number in that direction.
        Console.Write( topBottomRow[ 0 ] );
        Console.Write( topBottomRow[ 1 ] );
        Console.Write( topBottomRow[ 2 ] );
        Console.Write( topBottomRow[ 3 ] );
        Console.Write( topBottomRow[ 4 ] );
        Console.Write( topBottomRow[ 5 ] );
        Console.Write( topBottomRow[ 6 ] );
        Console.WriteLine();
        
        Console.Write( "|0" );
        Console.Write( mazeWalls[ 0, 0 ] );
        Console.Write( "1" );
        Console.Write( mazeWalls[ 0, 1 ] );
        Console.Write( "2|" );
        Console.WriteLine();
        
        Console.Write( "+" );
        Console.Write( mazeWalls[ 1, 0 ] );
        Console.Write( "+" );
        Console.Write( mazeWalls[ 1, 1 ] );
        Console.Write( "+" );
        Console.Write( mazeWalls[ 1, 2 ] );
        Console.Write( "+" );
        Console.WriteLine();
        
        Console.Write( "|3" );
        Console.Write( mazeWalls[ 2, 0 ] );
        Console.Write( "4" );
        Console.Write( mazeWalls[ 2, 1 ] );
        Console.Write( "5|" );
        Console.WriteLine();
        
        Console.Write( "+" );
        Console.Write( mazeWalls[ 3, 0 ] );
        Console.Write( "+" );
        Console.Write( mazeWalls[ 3, 1 ] );
        Console.Write( "+" );
        Console.Write( mazeWalls[ 3, 2 ] );
        Console.Write( "+" );
        Console.WriteLine();
        
        Console.Write( "|6" );
        Console.Write( mazeWalls[ 4, 0 ] );
        Console.Write( "7" );
        Console.Write( mazeWalls[ 4, 1 ] );
        Console.Write( "8|" );
        Console.WriteLine();
        
        Console.Write( topBottomRow[ 0 ] );
        Console.Write( topBottomRow[ 1 ] );
        Console.Write( topBottomRow[ 2 ] );
        Console.Write( topBottomRow[ 3 ] );
        Console.Write( topBottomRow[ 4 ] );
        Console.Write( topBottomRow[ 5 ] );
        Console.Write( topBottomRow[ 6 ] );
        Console.WriteLine();
        
        // So take cell number 4 and see what rooms are around it, 
        // cell 3 is to the left and cell 1 is up, but walls are right and down.
        Console.WriteLine( $"Cell to left of cell 4 is: { neighborLookup[ 4, LEFT ] }" );
        Console.WriteLine( $"Cell to up of cell 4 is: { neighborLookup[ 4, UP ] }" );
        Console.WriteLine( $"Cell to right of cell 4 is: { neighborLookup[ 4, RIGHT ] }" );
        Console.WriteLine( $"Cell to down of cell 4 is: { neighborLookup[ 4, DOWN ] }" );
    }
}
