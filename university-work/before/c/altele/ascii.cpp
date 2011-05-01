#include <stdio.h>
#include <conio.h>

main()
{
   for (int i= 1; i <= 255; i++)
   {
      printf("Alt + %i = %c", i, i);
      getch();
      printf("\n");
   }
   getch();

   return 0;
}