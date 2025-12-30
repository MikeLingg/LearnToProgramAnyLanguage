#include <cstdio>

int main() {
    // This maze program does not necessarily progress the program, 
    // but provides a construct we will use eventually in the program.
    char mazeWalls[ 5 ][ 3 ] = {
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
    
    int neighborLookup[ 9 ][ 4 ] = {
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
    
    char topBottomRow[ ] = { '+', '-', '+', '-', '+', '-', '+' };
    
    // This sort of code will look better when we have loops and functions
    // We see how a 3x3 maze is created from the array of data, and using the 
    // neighbor lookup provides -1 where a wall is in that direction, otherwise 
    // provides the cell number in that direction.
    printf( "%c", topBottomRow[ 0 ] );
    printf( "%c", topBottomRow[ 1 ] );
    printf( "%c", topBottomRow[ 2 ] );
    printf( "%c", topBottomRow[ 3 ] );
    printf( "%c", topBottomRow[ 4 ] );
    printf( "%c", topBottomRow[ 5 ] );
    printf( "%c", topBottomRow[ 6 ] );
    printf( "\n" );
    
    printf( "|0" );
    printf( "%c", mazeWalls[ 0 ][ 0 ] );
    printf( "1" );
    printf( "%c", mazeWalls[ 0 ][ 1 ] );
    printf( "2|" );
    printf( "\n" );
    
    printf( "+" );
    printf( "%c", mazeWalls[ 1 ][ 0 ] );
    printf( "+" );
    printf( "%c", mazeWalls[ 1 ][ 1 ] );
    printf( "+" );
    printf( "%c", mazeWalls[ 1 ][ 2 ] );
    printf( "+" );
    printf( "\n" );
    
    printf( "|3" );
    printf( "%c", mazeWalls[ 2 ][ 0 ] );
    printf( "4" );
    printf( "%c", mazeWalls[ 2 ][ 1 ] );
    printf( "5|" );
    printf( "\n" );
    
    printf( "+" );
    printf( "%c", mazeWalls[ 3 ][ 0 ] );
    printf( "+" );
    printf( "%c", mazeWalls[ 3 ][ 1 ] );
    printf( "+" );
    printf( "%c", mazeWalls[ 3 ][ 2 ] );
    printf( "+" );
    printf( "\n" );
    
    printf( "|6" );
    printf( "%c", mazeWalls[ 4 ][ 0 ] );
    printf( "7" );
    printf( "%c", mazeWalls[ 4 ][ 1 ] );
    printf( "8|" );
    printf( "\n" );
    
    printf( "%c", topBottomRow[ 0 ] );
    printf( "%c", topBottomRow[ 1 ] );
    printf( "%c", topBottomRow[ 2 ] );
    printf( "%c", topBottomRow[ 3 ] );
    printf( "%c", topBottomRow[ 4 ] );
    printf( "%c", topBottomRow[ 5 ] );
    printf( "%c", topBottomRow[ 6 ] );
    printf( "\n" );
    
    // So take cell number 4 and see what rooms are around it, 
    // cell 3 is to the left and cell 1 is up, but walls are right and down.
    printf( "Cell to left of cell 4 is: %d\n", neighborLookup[ 4 ][ LEFT ] );
    printf( "Cell to up of cell 4 is: %d\n", neighborLookup[ 4 ][ UP ] );
    printf( "Cell to right of cell 4 is: %d\n", neighborLookup[ 4 ][ RIGHT ] );
    printf( "Cell to down of cell 4 is: %d\n", neighborLookup[ 4 ][ DOWN ] );
    
    return 0;
}
