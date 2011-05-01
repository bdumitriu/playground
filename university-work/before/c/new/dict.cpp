#include <fstream.h>
#include <iostream.h>
#include <stdio.h>
#include <conio.h>
#include <string.h>

char pus[10][10];
char ch[10][10];
char val[10][10];
int k= 0;
ofstream f_fin("c:\\bc31\\work\\surse\\new\\whatever.cpp");

ok(char *c)
{
   for (int i= 0; i < k; i++)
      if (strcmp(c, pus[i]) == 0)
	 return 0;
   return 1;
}

check_do(char *y)
{
   for (int i= 0; i < 4; i++)
      if ((strcmp(ch[i], y) == 0) && ok(val[i]))
      {
	 f_fin<<"#include <"<<val[i]<<".h>\n";
	 memcpy(pus[k++], val[i], strlen(val[i]));
      }

   return 0;
}

main()
{
   ifstream file("c:\\bc31\\work\\surse\\new\\dict.dat");
   char hp[13], *x, y[50];
   int i;

   for (i= 0; i < 4; i++)
   {
      file>>ch[i];
      file>>hp;
      file>>val[i];
   }

/*
   cout<<"Numele fisierului de modificat : ";
   cin>>hp;
*/
   printf("Numele fisierului de modificat : ");
   scanf("%s", hp);

   ifstream fis(hp);

   for (i= 1; i <= 28; i++)          // Cum verific daca am ajuns la sf. fis?
   {
      fis>>x;
      y[0]= '\0';
      for (int j= 0; j < strlen(x); j++)
      {
	 if (x[j] == ';')
	 {
	    y[0]= '\0';
	    j++;
	 }
	 if (x[j] != '(')
	 {
	    char *aux;
	    *aux= x[j];
	    strcat(y, aux);
	 }
	 else
	 {
	     check_do(y);
	     y[0]='\0';
	 }
      }
   }
   //fis.~ifstream();
/*
   FILE *f;
   if ((f= fopen("copie.cpp", "r")) == NULL)
   {
      printf("Eroare la deschiderea fisierului.");
      getch();
      return 0;
   }
   while (!feof(f))
   {
      fscanf(f, "%s", x);
      f_fin<<x;
   }
   fclose(f);
*/
//   rename("c:\\bc31\\work\\surse\\new\\whatever.cpp", hp);

   //getch();
   return 0;
}