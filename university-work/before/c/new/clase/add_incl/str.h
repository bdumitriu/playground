#if !defined(__STR_H__)
#define __STR_H__

#include <iostream.h>

class String
{
public:
   String();  // constructor implicit
   String(int len, char c= ' ');
   String(const char* c);
   String(const String& s); // copy constructor
   String& operator=(const String& s);
   ~String();

   void List() const;
   int Length() const;

   String operator+(const char* s);
   String operator+(const String& s);
   char& operator[](int idx);
   int operator==(const String& s) const;
   friend ostream& operator<<(ostream& os, const String& s);

   operator char*();

private:
   char* str;

};

#endif