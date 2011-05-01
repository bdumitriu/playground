#include <process.h>
#include <errno.h>
#include <stdio.h>
#include <conio.h>

main()
{
   if (spawnl(P_WAIT, "c:\command.com", NULL) == -1)
   {
      switch(errno)
      {
	 case E2BIG: printf("Arg list too long"); break;
	 case EINVAL:   printf("Invalid argument"); break;
	 case ENOENT:   printf("Path or file name not found"); break;
	 case ENOEXEC:  printf("Exec format error"); break;
	 case ENOMEM:   printf("Not enough core"); break;
      }
      getch();
   }

   return 0;
}