#if !defined(__INTVIEW2_H__)
#define __INTVIEW2_H__

#include "observer.h"
#include "intdoc.h"

class IntView2 : public Observer
{
public:
   IntView2(int x, int y);
   ~IntView2();

   void Notify();
   void SetScreen();

   int operator==(const Observer& ob) const;

private:
   int cx, cy; // coltul x, coltul y
   int x;
};


#endif