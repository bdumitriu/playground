#include <stdio.h>

#define CREATE(className) new className#

class X
{
public:
   X(int xx = 10) { _xx = xx; }
   void print() const { printf("%d", _xx); }
   const char* getName() const { return "X"; }
private:
   int _xx;
};


void main()
{
     X x(12);
//   X* x =
     CREATE(x.getName());
//   x.print();
}
