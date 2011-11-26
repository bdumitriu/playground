#include <stdio.h>
#include <ncurses.h>

main()
{
	initscr();
	clear();
	printf("Hello world!\n");
	delay_output(1000);
	endwin();
	return 0;
}