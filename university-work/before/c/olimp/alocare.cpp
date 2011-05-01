#include <stdio.h>
#include <conio.h>
#include <alloc.h>

int far *c;

main()
{
   clrscr();
   printf("\n");

   c= (int far *) farmalloc(double(90000L)*double(90000L)*sizeof(int));
   if (c == NULL)
   {
      printf("NULL!!!");
      getch();
   }

   return 0;
}