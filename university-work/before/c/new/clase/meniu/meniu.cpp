#include "meniu.h"

Meniu::Meniu(const String& s, int x, int y, int t, COLORS BkNorm,
	     COLORS BkInv, COLORS FgNorm, COLORS FgInv)
   : ElementMeniu(s, BkNorm, BkInv, FgNorm, FgInv)
{
   this->x = x;
   this->y = y;
   tip = t;
   nrElem = 0;
   Elements = (ElementMeniu**)(new (ElementMeniu*) [10]);
   maxlen = 0;
   LastSel = 0;
}

Meniu::~Meniu()
{
  // for (int i = 0; i < nrElem; i++)
  //    delete Elements[i];
   delete [] Elements;
}

void Meniu::Adaugare(ElementMeniu* e)
{
   Elements[nrElem++] = e;
   if ((tip == 1) && (e->text.Length() > maxlen))
      maxlen = e->text.Length();
}

void Meniu::DeleteMeniu() const
{
   textbackground(BLACK);
   for (int i = y; i <= y+1+nrElem; i++)
   {
      gotoxy(1, i);
      cprintf("                                                                              ");
   }
}

void Meniu::AfisareSubmeniu()
{
   gotoxy(x, y);

   if (tip == 0)
   {
      for (int i = 0; i < nrElem; i++)
	 if (i == LastSel)
	    Elements[i]->Afisare(1);
	 else
	    Elements[i]->Afisare(0);
   }
   else
   {
      for (int i = 0; i < nrElem; i++)
      {
	 gotoxy(x+2, y+i+1);
	 if (i == LastSel)
	    Elements[i]->Afisare(1);
	 else
	    Elements[i]->Afisare(0);
	 for (int k = 1; k < maxlen-Elements[i]->text.Length()+1; k++)
	    cprintf(" ");
      }
      gotoxy(x, y);
      textbackground(BkNorm);
      textcolor(FgNorm);
      cprintf(" Ú");
      for (int j = 1; j < maxlen+3; j++)
	 cprintf("Ä");
      cprintf("¿ ");
      for (j = 1; j <= nrElem; j++)
      {
	 gotoxy(x+0, y+j);
	 cprintf(" ³");
	 gotoxy(x+4+maxlen, y+j);
	 cprintf("³ ");
      }
      gotoxy(x, y+1+nrElem);
      cprintf(" À");
      for (j = 1; j < maxlen+3; j++)
	 cprintf("Ä");
      cprintf("Ù ");
   }
}

void Meniu::MoveNext(int *i)
{
   if (tip == 0)
   {
      int xx = x;
      for (int j = 0; j < *i; j++)
	 xx += Elements[*i]->text.Length()+2;
      gotoxy(xx, wherey());
      Elements[*i]->Afisare(0);
      (*i)++;
      if (*i == nrElem)
	 *i = 0;
      xx = x;
      for (j = 0; j < *i; j++)
	 xx += Elements[*i]->text.Length()+2;
      gotoxy(xx, wherey());
      Elements[*i]->Afisare(1);
   }
   else
   {
      gotoxy(x+2, y+(*i)+1);
      Elements[*i]->Afisare(0);
      for (int j = 1; j < maxlen-Elements[*i]->text.Length()+1; j++)
	 cprintf(" ");
      (*i)++;
      if (*i == nrElem)
	 *i = 0;
      gotoxy(x+2, y+(*i)+1);
      Elements[*i]->Afisare(1);
      for (j = 1; j < maxlen-Elements[*i]->text.Length()+1; j++)
	 cprintf(" ");
   }
}

void Meniu::MovePrev(int *i)
{
   if (tip == 0)
   {
      int xx = x;
      for (int j = 0; j < *i; j++)
	 xx += Elements[*i]->text.Length()+2;
      gotoxy(xx, wherey());
      Elements[*i]->Afisare(0);
      (*i)--;
      if (*i == -1)
	 *i = nrElem-1;
      xx = x;
      for (j = 0; j < *i; j++)
	 xx += Elements[*i]->text.Length()+2;
      gotoxy(xx, wherey());
      Elements[*i]->Afisare(1);
   }
   else
   {
      gotoxy(x+2, y+(*i)+1);
      Elements[*i]->Afisare(0);
      for (int j = 1; j < maxlen-Elements[*i]->text.Length()+1; j++)
	 cprintf(" ");
      (*i)--;
      if (*i == -1)
	 *i = nrElem-1;
      gotoxy(x+2, y+(*i)+1);
      Elements[*i]->Afisare(1);
      for (j = 1; j < maxlen-Elements[*i]->text.Length()+1; j++)
	 cprintf(" ");
   }
}

Ex_codes Meniu::Execute(int a)
{
   AfisareSubmeniu();

   int i = LastSel;
   char c;
//   if (a == 1)
//      goto Here;
   if (tip == 0)
   {
      do
      {
	 c = getch();
	 if (c == 0)
	    c = getch();
	 if (c == 77)
	    MoveNext(&i);
	 if (c == 75)
	    MovePrev(&i);
      }
      while ((c != 13) && (c != 27));

      LastSel = i;
     // Here:
     // ;
      Ex_codes cod;

      if (c == 13)
      {
	  cod = Elements[LastSel]->Execute(0);
	  while ((cod != ID_ESC) || (cod != ID_EXEC))
	  {
	     if (c == ID_PREV)
	     {
		LastSel--;
		if (LastSel == -1)
		   LastSel = nrElem-1;
		this->Execute(1);
	     }
	     if (c == ID_NEXT)
	     {
		LastSel++;
		if (LastSel == nrElem)
		   LastSel = 0;
		this->Execute(1);
	     }
	  }
	  this->Execute(0);
      }
      else if (c == 27)
	 return ID_ESC;
   }

   if (tip == 1)
   {
      do
      {
	 c = getch();
	 if (c == 0)
	    c = getch();
	 if (c == 80)
	    MoveNext(&i);
	 if (c == 72)
	    MovePrev(&i);
	 if (a != 2)
	 {
	    if (c == 75)
	    {
	       LastSel = i;
	       this->DeleteMeniu();
	       return ID_PREV;
	    }
	    if (c == 77)
	    {
	       LastSel = i;
	       this->DeleteMeniu();
	       return ID_NEXT;
	    }
	 }
      }
      while ((c != 13) && (c != 27));

      LastSel = i;
      if (c == 13)
      {
	 if (!(Elements[i]->isMeniu()))
	 {
	    this->DeleteMeniu();
	    Elements[i]->Execute(0);
	    return ID_ESC;
	 }
	 else
	    if (Elements[i]->Execute(2) == ID_ESC)
	       this->Execute(0);
	    else
	    {
	       this->DeleteMeniu();
	       return ID_ESC;
	    }
      }
      else if (c == 27)
      {
	 this->DeleteMeniu();
	 return ID_ESC;
      }
   }

}

int Meniu::isMeniu() const
{
   return 1;
}