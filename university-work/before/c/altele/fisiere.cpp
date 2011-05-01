#include <stdio.h>
#include <conio.h>
typedef struct
{
   char n[20];
   int v;
} stud;

main()
{
   stud t[4]={"asgh", 24, "agajdf", 35, "anyrf", 20, "neudj", 23};
   stud v[4];
   FILE *f;

   clrscr();

   if( (f = fopen("test.dat", "w")) == NULL )
   {
      printf("eroare!");
      getch();
      return 0;
   }
/*   if (fwrite(t, sizeof(stud), 4, f) != 4)
   {
      printf("Eroare la scriere.");
      getch();
      fclose(f);
      return 0;
   }
*/
   for (int i= 0; i <= 3; i++)
      if (fprintf(f,"%s %i", t[i].n, t[i].v) == EOF)
	 {
	    printf("Eroare la scriere.");
	    getch();
	    fclose(f);
	    return 0;
	 }

   fclose(f);
   if( (f = fopen("test.dat", "r")) == NULL )
   {
      printf("eroare!");
      getch();
      return 0;
   }
/*   if (fread(v, sizeof(stud), 4, f) != 4)
   {
      printf("Eroare la citire.");
      getch();
      return 0;
   }*/
   for (i= 0; i <= 3; i++)
   {
      fscanf(f, "%s %i", v[i].n, &v[i].v);
      printf("Numele studentului %i este %s.\n", i, v[i].n);
      printf("Varsta studentului %i este %i.\n", i, v[i].v);
   }
   getch();
   return 0;

}
