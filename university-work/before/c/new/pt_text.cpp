#include <conio.h>
#include <iostream.h>
#include <fstream.h>
#include <string.h>

main()
{
   char s[20] = "xxxxxxxxxxxxxxxxxxx";
   ifstream fis("text.txt");
   ofstream fos("text.txt");

   clrscr();

   fis.putback('s');
   fis.getline(s, 15);
   fos.write(s, strlen(s));
   cout<<s;

   getch();
   return 0;
}