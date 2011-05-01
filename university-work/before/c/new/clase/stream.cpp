#include <fstream.h>
#include <dir.h>

main()
{
   char buf[81];
   ofstream ofs("test.txt");
   ifstream ifs("test.txt");

   chdir("c:\\bc31\\work\\surse\\new\\clase");
   ofs<<"Linia adaugata\n";

   while(!ifs.eof())
   {
      ifs.getline(buf, 80);
      ofs<<buf;
   }

   return 0;
}