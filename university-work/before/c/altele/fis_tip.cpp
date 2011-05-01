#include <stdio.h>
#include <conio.h>
#include <io.h>

typedef struct
{
   int x, y;
   char c;
} articol;

void main()
{
   int g;
   FILE *f;
   articol buf;

   f= fopen("temp.dat", "r");
   g= fileno(f);
   while (!feof(f))
   {
      fscanf(f, "%i %i %c", &buf.x, &buf.y, &buf.c);
      printf("Numarul 1 : %i \n", buf.x);
      printf("Numarul 2 : %i \n", buf.y);
      printf("Caracterul : %c \n", buf.c);
   }

   getch();
   return;
}