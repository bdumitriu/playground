#include <stdio.h>
#include <conio.h>    // daca nu-l ai scoateti-l si scoate si clrscr()-urile
#include <iostream.h> // si getch()-urile.
#include <string.h>

typedef struct    // Cu chestia asta imi definesc un nou tip structura,
{                 // tip al carui nume va fi sol si va fi compus dintr-un
   int x;         // intreg si dintr-un caracter.
   char c;        // O s-o folosesc in procedura GenereazaParole.
} sol;

FILE *f;  // FILE (vezi ca-i case sensitive) ii tipul fisier. Toate
          // functiile pe care le folosesc cu f ca parametru se afla in
          // stdio.h

int DeschideFisierIntrare()
{
   if ((f = fopen("carct.txt", "r")) == NULL)  // Cu fopen se deschid fisiere.
   {                                        // Atributul "r" ii pt. Read Only.
      cout << " Could not find the file carct.txt. Make sure it exists and";
      cout << " that it is in the\n";
      cout << "same directory as the executable file.";
      getch();
      return 1;
   }

   return 0;
}

void CitesteDinFisier(char *c)
{
   char *s;
   int i = 0;         // Acum citesc din fisier caracterele pe care pot sa le
                      // folosesc in generarea parolelor. Le memorez in sirul
   do                 // de caractere c.
   {
      fgets(s, 3, f);  // fgets(s, n, f) citeste o linie din fisierul f in
      c[i++] = s[0];   // stringul s sau pana citeste n-1 caractere sau pana
   }                   // intalneste un caracter sf. de linie ("\n").
   while (!(feof(f)));
   c[i] = '\0';       // Pun terminator de sir ('\0') pe ultima pozitie.
   fclose(f);         // Inchid fisierul.

   return;
}

void CitesteDateIntrare(int *n, char *op, char *of)
{
   cout << " Please enter the number of characters in a password. Take care";
   cout << " for the number\n";
   cout << "you type not to be greater than the number of characters in the";
	cout << "carct.txt file.\n\n";
   do
   {
      cout << "  The number of characters in a password [1 through 255]: ";
      cin >> *n;      // echivalent cu scanf("%d", &n);
   }
   while ((*n < 1) || (*n > 255));

   do
   {
      cout << "  Please enter your choice of output, file or screen (f/s): ";
      cin >> *op;
   }
   while ((*op != 'f') && (*op != 's'));

   if (*op == 'f')
   {
      cout << "  The name of the output file (if it exists it will be ";
      cout << "overwritten):\n";
      cout << "                         ";
      cin >> of;
   }
}

int CreeazaFisierIesire(char *op)
{
   if ((f = fopen(op, "w")) == NULL)    // Acum creez un nou fisier cu numele
   {                                    // stocat in variabila op.
      cout << " Error creating the file " << op << ".";
      getch();
      return 1;
   }

   return 0;
}

void Scrie(char op, int n, sol p[255])
{
   if (op == 's')
   {
      cout << " ";
      for (int i = 0; i <= n; i++)
         cout << p[i].c;
      getch();
      cout << "\n";
   }
   else
   {
      for (int i = 0; i <= n; i++)
         fprintf(f, "%c", p[i].c);
      fprintf(f, "\n");
   }

   return;
}

void GenereazaParole(char *c, int n, char op)
{
   int nc = strlen(c), i = 0;
   sol p[255];       // p va fi o variabila - sir de elemente de tip sol. Am
                     // presupus ca n va fi maxim 255.
   p[0].x = 0;
   do
   {
      while (p[i].x < nc)
      {
         p[i].c = c[p[i].x++];
         if (i == n-1)
            Scrie(op, n-1, p);
         else
         {
            i++;
            p[i].x = 0;
         }
      }
      i--;
   }
   while (i > -1);

   return;
}

void main()
{
   char c[255], op, *of;
   int n;

   clrscr();
   cout << "\n";  // cout ii echivalenta cu printf dar mai usor de folosit.
                  // Daca in Linux nu exista iostream.h inlocuieste-le cu
                  // printf-uri.

   if (DeschideFisierIntrare() == 1)
      return;
   CitesteDinFisier(c);
   CitesteDateIntrare(&n, &op, of);
   if (op == 'f')
   {
      if (CreeazaFisierIesire(of) == 1)
         return;
      cout << "\n\n  Please be patient, especially if you have enered a ";
      cout << "number greater than 6.\n";
      cout << "  This may take quite a while (minutes, not seconds).\n";
      cout << "  Generating file... ";
   }
   GenereazaParole(c, n, op);
   if (op == 'f')
   {
      fclose(f);
      cout << "Done";
   }

   return;
}
