#include <iostream.h>
#include "str.h"

class Pers
{
public:
   Pers();
   Pers(const String& n, const String& p);
   Pers(const Pers& p);
   ~Pers();

   Pers& operator=(const Pers& p);
   int operator==(const Pers& p) const;
   friend ostream& operator<<(ostream& os, const Pers& p);

   void List() const;


private:
  String nume;
  String prenume;

};