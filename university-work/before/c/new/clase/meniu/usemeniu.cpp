#include "elem_men.h"
#include "element.h"
#include "strcomm.h"
#include "meniu.h"
#include <conio.h>
#include <stdio.h>
 
main()
{
   textbackground(BLACK);
   clrscr ();

   Meniu* m = new Meniu("", 5, 1, 0);
   Meniu* file = new Meniu("File", 4, 2, 1);
   Meniu* edit = new Meniu("Edit", 10, 2, 1);
   Meniu* neww = new Meniu("New   >", 17, 3, 1);
   Element* open = new Element(new StrCommand("Sir de caractere", 50, 22), "Open");
   Element* save = new Element(new StrCommand("Sir de caractere", 50, 22), "Save");
   Element* undo = new Element(new StrCommand("Sir de caractere", 50, 22), "Undo");
   Element* redo = new Element(new StrCommand("Sir de caractere", 50, 22), "Redo");
   Element* aux1 = new Element(new StrCommand("Sir de caractere", 50, 22), "Optiunea generala 1");
   Element* aux2 = new Element(new StrCommand("Sir de caractere", 50, 22), "Optiunea particulara 23455");
   neww->Adaugare(aux1);
   neww->Adaugare(aux2);
   file->Adaugare(neww);
   file->Adaugare(open);
   file->Adaugare(save);
   edit->Adaugare(undo);
   edit->Adaugare(redo);
   m->Adaugare(file);
   m->Adaugare(edit);
   m->Execute(0);

//   delete m;
   return 0;
}