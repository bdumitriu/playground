#include "strcomm.h"
#include <conio.h>
#include <stdio.h>

StrCommand::StrCommand(const String& s, int x, int y)
   : str(s), x(x), y(y)
{}

StrCommand::~StrCommand()
{}

void StrCommand::DoCommand()
{
   textbackground(BLACK);
   textcolor(LIGHTGRAY);
   gotoxy(x, y);
   cprintf(" %s ", str);
}