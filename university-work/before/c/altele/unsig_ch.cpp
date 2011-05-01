#include <stdio.h>
#include <conio.h>

main()
{
   clrscr();

   int  x;
   char c;

   x = 10;
   x = 'A';

   c = 'B';
   c = 66;

   unsigned char c11, c12;
   char c21, c22;

   c21 = 200;
   c22 = 2;

   if( c21 > c22 )
      printf("\nC21 > C22");
   else
      printf("\nC21 < C22");
   getch();
}