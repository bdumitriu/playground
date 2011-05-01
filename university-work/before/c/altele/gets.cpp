#include <stdio.h>
#include <conio.h>

main()
{
   char *buffer;

   clrscr();

   buffer= (char*) malloc(100);
   gets(buffer);
   puts(buffer);

   getch();
}
