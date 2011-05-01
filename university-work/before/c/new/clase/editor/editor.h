#if !defined(__EDITOR_H__)
#define __EDITOR_H__

#include <stdio.h>
#include <conio.h>
#include <string.h>
#include <stdlib.h>

class Editor
{
public:
	Editor(int ExitCodes[30], int UpperLeftX = 1, int UpperLeftY = 2,
			 int LowerRightX = 80, int LowerRightY = 24,
			 int ForegroundColor = YELLOW, int BackgroundColor = BLUE,
			 char Title[20] = "", int CleaningColor = BLACK);
	// For ExitCodes[30] you will have to build an array with the
	// ascii codes of the commands that will exit the editor. In case the
	// ascii code is formed of two numbers, that is if it begins with 0 and
	// then a code then use the negative form (eg. if Alt+F exits the editor
	// then use -33 because the code for Alt+F is 0 33). The int Edit() member
	// function will return the element of the array when the editing is
	// finished (if the user pressed Alt+F then means the function will return
	// -33. The codes are considered until the first element = 0 is
	// encountered.
	~Editor();

	void Initialize();
	int  Edit();
	void ClearScreen();// clears the area previously ocupied by the editor u-
							 // sing the CleaningColor
	void CursorOn() const; // turns the cursor on
	void CursorOff() const; // turns the cursor off


protected:
	char **buffer; // the screen will only allow a maximum of 500
						// characters per line

private:
	int bi, bj;     // indexes for the buffer
	int lines;      // the number of lines
	int x1, y1, x2, y2, fgc, bgc, cc;
	int cur_x, cur_y, x_max, y_max;
	int excod[30], dim;
	char title[20];
	int NotExitCode(char c, int zero);
};

#endif