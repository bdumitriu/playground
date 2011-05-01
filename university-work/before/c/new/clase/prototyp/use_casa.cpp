#include "acopct.h"
#include "percar.h"
#include "casa.h"
#include <conio.h>

void main()
{
   Casa c1(Acoperis(3, 0), Percar(100, 5));
   Casa c2(Acoperisct(250, 5, 1), Percar(300, 8));

   clrscr();
   c1.Afisare();
   c2.Afisare();
   getch();
}