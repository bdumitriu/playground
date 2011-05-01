#include <stdio.h>
#include <conio.h>
#include <string.h>
#include "str.h"

String::String() // constructor implicit
{
   str= new char[1];
   str[0]= '\0';
}

String::String(int len, char c)
{
   str= new char[len+1];
   for (int i= 0; i < len; i++)
      str[i]= c;
   str[len]= '\0';
}

String::String(const char* c)
{
   str= new char[strlen(c)+1];  //str=strcpy(new char[strlen(c)+1], c);
   strcpy(str, c);
}

String::String(const String& s) // copy constructor
{
   str= strcpy(new char[strlen(s.str)+1], s.str);
   //*this= s;
}

String::~String()
{
   delete [] str;
}

void String::List() const
{
   printf("\n Sirul este : %s", str);
}

int String::Length() const
{
   return strlen(str);
}

String String::operator+(const char* s) // const pt. ca s-ul nu va fi mo-
{                                       // dificat? Si unde trebuie obli-
   String sRez(Length()+strlen(s));     // gatoriu sa punem const ?

   strcpy(sRez, str);
   strcat(sRez, s);

   return sRez;
}

String String::operator+(const String& s)
{
   String sRez(Length()+s.Length());

   strcpy(sRez, str);
   strcat(sRez, s.str);

   return sRez;
}

String& String::operator=(const String& s)
{
   if (this != &s)
   {
      delete [] str;
      str= new char[s.Length()+1];
      strcpy(str, s.str);
   }

   return *this;
}

int String::operator==(const String& s) const
{
   return !strcmp(str, s.str);
}

ostream& operator<<(ostream& os, const String& s)
{
   os<<s.Length()<<' '<<s.str;
   return os;
}

istream& operator>>(istream& is, String& s)
{
   char buf[1000];
   int len;

   is>>len;
   is.get();
   is.getline(buf, len+1);
	s = String(buf);

   return is;
}

String::operator char*()
{
   return str;
}