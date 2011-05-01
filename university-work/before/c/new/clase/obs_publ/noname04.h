#if !defined(__INTVIEW2_H__)
#define __INTVIEW2_H__

#include "observer.h"
#include "intdoc.h"

class IntView1 : public Observer
{
public:
   IntView1(int x, int y);
   ~IntView1();

   void Notify();
   void SetScreen();

   int operator==(const Observer& ob) const;

private:
   int cx, cy; // coltul x, coltul y
   int x;
};


#endif