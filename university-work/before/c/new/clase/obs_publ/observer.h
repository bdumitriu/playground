#if !defined(__OBSERVER_H__)
#define __OBSERVER_H__

class Observer
{
public:

   Observer();
   ~Observer();

   virtual void Notify() = 0;
   virtual int operator==(const Observer& ob) const = 0;
   friend class Publisher;

protected:
   Publisher* pub;

};


#endif