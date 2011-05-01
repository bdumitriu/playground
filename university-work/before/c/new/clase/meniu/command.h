#if !defined(__COMMAND_H__)
#define __COMMAND_H__

class Command
{
public:
   Command();
   virtual ~Command();

   virtual void DoCommand() = 0;

};

#endif