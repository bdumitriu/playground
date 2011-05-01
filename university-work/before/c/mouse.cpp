#include <stdio.h>
#include <dos.h>
#include <conio.h>

main()
{
   union REGS reg;

   clrscr();
   reg.x.ax = 0x0A;
   reg.x.bx = 0;
   reg.x.cx = 1;
   reg.x.dx = 2;
   int86(0x33, &reg, &reg);
   reg.x.ax = 1;
   int86(0x33, &reg, &reg);
   gotoxy(1, 1);
   getch();
   reg.x.ax = 2;
   int86(0x33, &reg, &reg);

   return 0;
}