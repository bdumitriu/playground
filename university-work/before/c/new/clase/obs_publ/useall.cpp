#include "observer.h"
#include "publish.h"
#include "state.h"
#include "intdoc.h"
#include "intdst.h"
#include "intview1.h"
#include "intview2.h"
#include "intview3.h"
#include <conio.h>
#include <dos.h>

void main()
{
   Publisher *id1 = new IntDocument(6);
   Publisher *id2 = new IntDocument(2);
   IntView1 *ob11 = new IntView1(1, 1);
   IntView2 *ob12 = new IntView2(23, 1);
   IntView3 *ob13 = new IntView3(45, 1);
   IntView1 *ob21 = new IntView1(1, 10);
   IntView2 *ob22 = new IntView2(23, 10);
   IntView3 *ob23 = new IntView3(45, 10);

   textbackground(BLACK);
   clrscr();

   id1->Attach(*ob11);
   id1->Attach(*ob12);
/*
   for (int i = 0; i < 9; i++)
   {
      id1->SetVal(i);
      delay(1000);
   }
*/
   id1->Attach(*ob13);
//   ob13->GetKey();

   id2->Attach(*ob21);
   id2->Attach(*ob22);
   id2->Attach(*ob23);

  int x = ob13->GetKey(), crtview = 1;
   while (x)
   {
      crtview++;
      if (crtview == 3)
	 crtview = 1;
      if (crtview == 1)
	 x = ob13->GetKey();
      if (crtview == 2)
	 x = ob23->GetKey();
   }
}