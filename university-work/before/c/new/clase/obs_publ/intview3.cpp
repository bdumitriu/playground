#include "intview3.h"
#include "intdst.h"
#include <conio.h>
#include <stdio.h>

IntView3::IntView3(int x, int y)
{
   this->cx = x;
   this->cy = y;
}

IntView3::~IntView3()
{}


void DrawBorder3()
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

void IntView3::SetScreen()
{
   window(1, 1, 80, 25);
   window(cx, cy, cx+20, cy+9);
   textbackground(BLUE);
   DrawBorder3();
   window(1, 1, 80, 25);
   window(cx+1, cy+1, cx+19, cy+7);
   clrscr();
   textcolor(YELLOW);
   gotoxy(10, 4);
   cprintf("%d", x);
   gotoxy(18, 3);
   cprintf("%c", char(24));
   gotoxy(18, 5);
   cprintf("%c", char(25));
}

int IntView3::GetKey()
{
   char c = 0;
   while ((c != 9) && (c != 27))
   {
      c = getch();
      if (c == 0)
      {
	 c = getch();
	 if (c == 72)
	    pub->SetVal(x+1);
	 if (c == 80)
	    pub->SetVal(x-1);
      }
   }
   if (c == 9)
      return 1;
   if (c == 27)
      return 0;
}

void IntView3::Notify()
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

int IntView3::operator==(const Observer& ob) const
{
   return 0;
}