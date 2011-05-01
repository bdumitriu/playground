#include <stdio.h>
#include <conio.h>

int i= 0;

int sep(char a, char *x)
{
   int j;

   for (j= 0; j < 5; j= j+1)
      if (a == x[j])
	 return 1;

   return 0;
}


int ret_token (char *s, char *x)
{
   int j, k;

   while (sep(s[i], x))
      i= i+1;
   j= i;
   while(!sep(s[j], x))
      j= j+1;
   for (k= i; k < j; k= k+1)
      printf("%c", s[k]);
   printf(" ");
   i = j+1;
   if (s[i-1]=='\0')
      return 0;
   else
      return 1;
}

main()
{
   char s[100]= "saf ,asjk agf;dgsd";
   char x[] = {' ', ',', ';', '\t', '\0'};

   clrscr();

   while (ret_token(s, x));
   getch();

}