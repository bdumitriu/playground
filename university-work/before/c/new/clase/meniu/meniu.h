#if !defined(__MENIU_H__)
#define __MENIU_H__

#include "elem_men.h"
#include "element.h"

class Meniu : public ElementMeniu
{
public:
   Meniu(const String& s, int x, int y, int t, COLORS BkNorm = WHITE,
	 COLORS BkInv = GREEN, COLORS FgNorm = BLACK, COLORS FgInv = BLACK);
   ~Meniu();

   void     Adaugare(ElementMeniu* e);
   Ex_codes Execute(int a);
   int      isMeniu() const;
   void     DeleteMeniu() const;

private:
   int tip, nrElem, maxlen, x, y, LastSel;
   ElementMeniu** Elements;

   void MoveNext(int *i);
   void MovePrev(int *i);
   void AfisareSubmeniu();

};

#endif