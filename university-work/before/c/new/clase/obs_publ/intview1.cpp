#include "intview1.h"
#include "intdst.h"
#include <conio.h>
#include <stdio.h>

IntView1::IntView1(int x, int y)
{
   this->cx = x;
   this->cy = y;
}

IntView1::~IntView1()
{}


void DrawBorder1()
{
   gotoxy(1, 1);
   cprintf("Ú");
   for (int i = 0; i < 19; i++)
      cprintf("Ä");
   cprintf("¿");
   for (i = 2; i < 9; i++)
   {
      gotoxy(1, i);
      cprintf("³");
      gotoxy(21, i);
      cprintf("³");
   }
   gotoxy(1, 9);
   cprintf("À");
   for (i = 0; i < 19; i++)
      cprintf("Ä");
   cprintf("Ù");
}

void IntView1::SetScreen()
{
   window(1, 1, 80, 25);
   window(cx, cy, cx+20, cy+9);
   textbackground(BLUE);
   DrawBorder1();
   window(1, 1, 80, 25);
   window(cx+1, cy+1, cx+19, cy+7);
   clrscr();
   textcolor(YELLOW);
   gotoxy(10, 4);
   cprintf("%d", x);
}

void IntView1::Notify()
{
/*
   IntDocumentState *a = new IntDocumentState(1);
   a = (IntDocumentState*) (pub->GetState());
   this->x = a->x;
   SetScreen();
   delete a;
*/
   x = pub->GetState();
   SetScreen();
}

int IntView1::operator==(const Observer& ob) const
{
   return 0;
}