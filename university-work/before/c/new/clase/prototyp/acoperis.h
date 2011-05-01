#if !defined(__ACOPERIS_H__)
#define __ACOPERIS_H__

class Acoperis
{
public:
   Acoperis(int cul, int ah);
   virtual void Afisare() const;
   virtual Acoperis* Clone() const;

protected:
   int Culoare, AreHorn;
};

#endif