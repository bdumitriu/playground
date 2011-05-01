#if !defined(__STRCOMM_H__)
#define __STRCOMM_H__

#include "str.h"
#include "command.h"

class StrCommand : public Command
{
public:
   StrCommand(const String& s, int x, int y);
   ~StrCommand();

   void DoCommand();

private:
   int x, y;
   String str;

};

#endif