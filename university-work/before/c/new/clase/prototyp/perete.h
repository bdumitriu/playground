#if !defined(__PERETE_H__)
#define __PERETE_H__

class Perete
{
public:
   Perete(int cul);
   virtual void Afisare() const;
   virtual Perete* Clone() const;

protected:
   int Culoare;
};

#endif