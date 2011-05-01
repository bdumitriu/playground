#if !defined(__ENT_GR_H__)
#define __ENT_GR_H__

class EntitateGrafica //clasa abstracta (are cel putin o functie pur virtuala)
{
public:
   EntitateGrafica(int x, int y);
   EntitateGrafica();
   ~EntitateGrafica();

   int   X() const;
   int   Y() const;
   void  Pozitionare(int x, int y);

   virtual int  operator==(const EntitateGrafica& e) const = 0;

   virtual double Perimetru() const = 0;  //functie pur virtuala
   virtual double Arie() const = 0;
   virtual void   Afisare() const = 0;

protected:
   int x, y;

};

#endif
