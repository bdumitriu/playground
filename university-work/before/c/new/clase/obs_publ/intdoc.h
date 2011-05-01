#if !defined(__INTDOC_H__)
#define __INTDOC_H__

#include "publish.h"

class IntDocument : public Publisher
{
public:

   IntDocument(int i = 0);
   ~IntDocument();

//   State* GetState() const;
   int GetState() const;
   void SetVal(int i);

private:
   int x;

};

#endif