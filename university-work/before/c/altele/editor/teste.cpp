#include <dos.h>
#include <iostream.h>
#include <conio.h>

main()
{
   unsigned int far *screen;

   screen= (unsigned int *) MK_FP(0xB800, 0);

   clrscr();
   cout<<"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
   for (int i= 0; i < 52; i++)
      cout<<screen[i]<<' ';
   //screen[0]= 0x700+'a';
   getch();

   return 0;
}