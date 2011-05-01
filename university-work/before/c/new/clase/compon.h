#if !defined(__COMPON_H__)
#define __COMPON_H__

class Component
{
public:
   Component(int x = 10);
   ~Component();

   int operator==(const Component& c) const;

   virtual void Afisare() const = 0;

protected:
   int i;
};

#endif