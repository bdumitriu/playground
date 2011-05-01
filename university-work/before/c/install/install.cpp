#include <stdio.h>
#include <string.h>
#include <conio.h>
#include <dir.h>
#include <errno.h>
#include <process.h>

char s[300], t[300];

Copiaza(char numef[300])
{
   char p[300];
   FILE *f1, *f2;
   char b[8001];
   int k;

   p[0]= '\0';
   memcpy(p, s, strlen(s));
   p[strlen(s)]= '\0';
   strcat(p, "\\");
   strcat(p, numef);

   printf(" Instalez fisierul %s... ", numef);
   if ((f1= fopen(numef, "rb")) == NULL)
   {
      printf("\n Eroare la deschiderea fisierul '%s'.\n", numef);
      return 1;
   }
   if ((f2= fopen(p, "wb")) == NULL)
   {
      printf("\n Eroare la crearea fisierul %s.\n", p);
      return 1;
   }
   do
   {
      for (int j= 1; (j < 8000) && (!feof(f1)); j++)
	 b[j]= getc(f1);
      for (k= 1; ((k < j) && (!feof(f1))) || ((k < j-1) && (feof(f1))); k++)
	 putc(b[k], f2);
   }
   while (!feof(f1));

   fclose(f1);
   fclose(f2);
   printf(" û\n");

   return 0;
}

main()
{
   char aux[300];

   Iar:
   ;
   clrscr();

   printf("\n  Acest program va instala programul <NUMELE PROGRAMULUI> pe calculatorul");
   printf("\n");
   printf(" dumneavoastra.\n\n");
   printf(" Va rog sa introduceti calea de unde doriti sa-l instalati : \n  ");
   scanf("%s", t);

   aux[0]= '\0';
   memcpy(aux, t, strlen(t));
   aux[strlen(t)]= '\0';
   if (aux[strlen(t)-1] == '\\')
      strcat(aux, "test.tst");
   else
      strcat(aux, "\\test.tst");
   if (searchpath(aux) == NULL)
   {
      printf(" Calea gresita.");
      getch();
      goto Iar;
   }

   printf(" Va rog sa introduceti calea unde doriti sa-l instalati : \n  ");
   scanf("%s", s);

   if (searchpath(s) == NULL)
   {
      char c;

      Aici:
      ;
      printf("\n Calea introdusa de dumneavoastra nu exista.");
      printf("\n Doriti sa creati acest director (d/n) ? ");
      scanf("%c", &c);
      scanf("%c", &c);
      if ((c != 'd') && (c != 'n'))
	  goto Aici;
      else
	 if (c == 'd')
	    if (mkdir(s) == 0)
	       printf(" Directorul a fost creat.");
	    else
	    {
	       printf(" Eroare la crearea directorului.\n");
	       printf(" Verificati urmatoarele :\n");
	       printf("  - calea introdusa exista pana la penultimul director.\n");
	       printf("    Adica daca ati introdus 'c:\\aaa\\program', c:\\aaa trebuie sa existe.\n");
	       printf("  - calea introdusa este corecta.\n");
	       printf("  - aveti drepturi de scriere in calea respectiva.\n");
	       getch();
	       return 0;
	    }
	 else
	    return 0;
   }

   chdir(s);
   aux[0]= '\0';
   memcpy(aux, s, strlen(s));
   aux[strlen(s)]= '\0';
   if (aux[strlen(s)-1] == '\\')
      strcat(aux, "exemple");
   else
      strcat(aux, "\\exemple");
   printf("\n Creez directorul EXEMPLE... ");
   if (mkdir(aux) == -1)
   {
      printf(" Eroare la crearea directorului EXEMPLE.\n");
      printf(" Instalarea abandonata.\n");
      return 0;
   }
   else
      printf("ÿû\n");
   aux[0]= '\0';
   memcpy(aux, s, strlen(s));
   aux[strlen(s)]= '\0';
   if (aux[strlen(s)-1] == '\\')
      strcat(aux, "privat");
   else
      strcat(aux, "\\privat");
   printf(" Creez directorul PRIVAT... ");
   if (mkdir(aux) == -1)
   {
      printf(" Eroare la crearea directorului PRIVAT.\n");
      printf(" Instalarea abandonata.\n");
      return 0;
   }
   else
      printf("ÿû\n");
   chdir(t);

   Copiaza("test.tst");
   Copiaza("test2.tst");
   Copiaza("test3.tst");

   strcat(s, "\\exemple");

   Copiaza("baza1.dbf");
   Copiaza("baza2.dbf");
   Copiaza("baza3.dbf");

   //if (spawnl(P_WAIT, "c:\\tp\\bin\\turbo.exe", NULL) == -1)
   //   printf("%i", errno);


   return 0;
}