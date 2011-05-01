#if !defined(__PUBLISH_H__)
#define __PUBLISH_H__

#include "observer.h"
#include "state.h"

class Publisher
{
public:
   Publisher();
   ~Publisher();

   void Attach(Observer& ob);
   void Detach(Observer& ob);
//   virtual State* GetState() const = 0;
   virtual int GetState() const = 0;
   void NotifyObservers() const;
   virtual void SetVal(int i) = 0;

private:
   Observer** ObsList;
   int idx;

};

#endif