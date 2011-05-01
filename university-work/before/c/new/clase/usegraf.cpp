#include <conio.h>
#include "c:\bc31\work\surse\new\clase\graf.h"

main()
{
   Graf g(6);

   clrscr();

   g.adm(0, 1);
   g.adm(0, 2);
   g.adm(0, 3);
   g.adm(0, 5);
   g.adm(1, 2);
   g.adm(1, 4);
   g.adm(2, 3);
   g.adm(2, 5);
   g.adm(3, 4);
   g.adm(4, 5);
   g.bf();
   g.df();
   g.bf(3);
   g.df(3);

   return 0;
}
     /* while ((k < n) && (((t[s[p]][k] == 0) && (t[k][s[p]] == 0)) ||
			(((t[s[p]][k] == 1) || (t[k][s[p]] == 1)) && (viz[k] == 1))))*/