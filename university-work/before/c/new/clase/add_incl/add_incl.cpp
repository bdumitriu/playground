#include <conio.h>
#include <string.h>
#include <iostream.h>
#include <fstream.h>
#include "str.h"
#include "tpl_dict.h"

Dict<String, String> com(5);
ofstream f_o("c:\\bc31\\work\\surse\\new\\clase\\add_incl\\whatever.cpp");
char str[20][10];
int i= 0;

Ok(char y[100])
{
   for (int j= 0; j < i; j++)
      if (strcmp(str[j], com.Gaseste(y)) == 0)
	 return 0;
   return 1;
}

Nu_exista(char y[100])
{
   for (int j= 0; j < i; j++)
      if (strcmp(str[j], y) == 0)
	 return 0;
   return 1;
}

Proceseaza_linie(String s)
{
   char y[100];
   int idx;

   y[0]= '\0';
   idx= 0;

   for (int j= 0; j < s.Length(); j++)
   {
      if (s[j] == ';')
      {
	 y[0]= '\0';
	 idx= 0;
	 j++;
      }
      if ((s[j] != '(') && (s[j] != ' '))
      {
	 char *aux;
	 *aux= s[j];
	 strcat(y, aux);
	 idx++;
	 y[idx]= '\0';
      }
      else if (s[j] == '(')
      {
	 if ((com.Exista(y)) && (Ok(y)))
	 {
	    //cout<<"Am gasit pentru "<<y<<" valoarea "<<com.Gaseste(y)<<".\n";
	    //getch();
	    f_o<<"#include <"<<com.Gaseste(y)<<".h>\n";
	    if (Nu_exista(y))
	       memcpy(str[i++], com.Gaseste(y), strlen(y));
	    y[0]='\0';
	    idx= 0;
	 }
      }
   }
   return 0;
}

main()
{
   String s(80);
   String t(13);

   clrscr();

   com.Adauga("printf", "stdio");
   com.Adauga("scanf", "stdio");
   com.Adauga("clrscr", "conio");
   com.Adauga("getch", "conio");

   cout<<"\n Numele fisieului de modificat : ";
   cin>>t;

   ifstream f_in(t);
   //ifstream f_in("testfile.cpp");

   while (!f_in.eof())
   {
      f_in.getline(s, 80);
      Proceseaza_linie(s);
   }

   ifstream f_aux(t);
   //ifstream f_aux("testfile.cpp");

   f_o<<'\n';
   while (!f_aux.eof())
   {
      f_aux.getline(s, 80);
      f_o<<s<<'\n';
   }
   f_o.close();

   remove(t);
   rename("whatever.cpp", "testfile.cpp");
   //remove("testfile.cpp");
   //rename("whatever.cpp", "testfile.cpp");

   return 0;
}