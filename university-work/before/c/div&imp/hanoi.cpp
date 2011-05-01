/* problema turnurilor din Hanoi */
#include <stdio.h>
#include <conio.h>

hanoi(int n, char a, char b, char c)
{
   if (n == 1)
      printf("                 %c - %c\n", a, b);
   else
   {
      hanoi(n-1, a, c, b);
      printf("                 %c - %c\n", a, b);
      hanoi(n-1, c, b, a);
   }

   return 0;
}

main()
{
   int n;

   clrscr();
   printf("\n");

   printf(" Numarul de discuri : ");
   scanf("%i", &n);
   printf("\n      Succesiunea de mutari este :\n\n");
   hanoi(n, 'A', 'B', 'C');

   getch();
   return 0;
}