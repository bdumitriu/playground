#include <conio.h>
#include <fstream.h>
#include "str.h"

main()
{
   /*String s1;
   String s2("DGHJGFJ");
   String s3(6, 'f');
   String s4(s3);
   String s6=s3;*/
   String s1;
   String s2("Alt text trebuie scris aici.");
   String s3, s4;
   fstream f("xxx.tst", ios::out);

   clrscr();

   f<<s1<<s2;
   f.close();

   fstream g("xxx.tst", ios::in);

   g>>s3>>s4;

   s3.List();
   s4.List();
   /*
   s1.Print();
   s2.Print();
   s3.Print();
   s4.Print();


   String s5= s2+s3;
   s5.Print();

   s6=s2;
   s6.Print();

   s1= s2+"xxx";
   s1.Print();

   s1= String("xxx")+s2;
   s1.Print();

   getch();
   */
   return 0;
}