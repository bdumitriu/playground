/*
 * Autor:		Bogdan DUMITRIU
 * Data:		07.05.2002
 * Versiune:	0.1
 */

#define Uses_TApplication
#define Uses_TKeys
#define Uses_TRect
#define Uses_TMenu
#define Uses_TMenuBar
#define Uses_TSubMenu
#define Uses_TMenuItem
#define Uses_TMenuBox
#define Uses_TDialog
#define Uses_TStatusLine
#define Uses_TStatusItem
#define Uses_TStatusDef
#define Uses_TDeskTop
#define Uses_TButton

#include <conio.h>
#include <tv.h>
#include <dos.h>
#include "commands.h"
#include "helpwin.h"

// some new commands
const int cmdHome = 200;
const int cmdAjutor = 201;
const int cmdDespre = 202;

const int cmd1 = 301;
const int cmd2 = 302;
const int cmd3 = 303;
const int cmd4 = 304;
const int cmd5 = 305;
const int cmd6 = 306;
const int cmd7 = 307;

const int cmd01 = 310;	// function 01h
const int cmd08 = 311;	// function 08h
const int cmd07 = 312;	// function 07h
const int cmd06 = 313;	// function 06h
const int cmd0b = 314;	// function 0bh
const int cmd0c = 315;	// function 0ch
const int cmd0a = 316;	// function 0ah
const int cmd02 = 317;	// function 02h
const int cmd09 = 318;	// function 09h
const int cmd05 = 319;	// function 05h

const int cmd4406 = 320;	// function 4406h
const int cmd4407 = 321;	// function 4407h
const int cmd4408 = 322;	// function 4408h

const int cmd39 = 323;	// function 39h
const int cmd3a = 324;	// function 3ah
const int cmd3c = 325;	// function 3ch
const int cmd41 = 326;	// function 41h
const int cmd56 = 327;	// function 56h
const int cmd4300 = 328;	// function 4300h
const int cmd4301 = 329;	// function 4301h

const int cmd440c5f = 330;	// function 430c5fh
const int cmd440c7f = 331;	// function 430c7fh

const int cmd19 = 332;	// function 19h
const int cmd0e = 333;	// function 0eh
const int cmd47 = 334;	// function 47h
const int cmd3b = 335;	// function 3bh
const int cmd36 = 336;	// function 36h
const int cmd3305 = 337;	// function 3305h

const int cmd3d = 338;	// function 3dh
const int cmd3f = 339;	// function 3fh
const int cmd40 = 340;	// function 40h

const int cmd30 = 341;	// function 30h
const int cmd2a = 342;	// function 2ah
const int cmd2c = 343;	// function 2ch
const int cmd35 = 344;	// function 35h

const int cmdback = 400;

class Main: public TApplication
{
public:
	Main();
	static TStatusLine *initStatusLine(TRect r);
	static TMenuBar *initMenuBar(TRect r);
	virtual void handleEvent(TEvent& event);
	void displayMainMenu();

private:
	TMenuBox *menu;
	TMenuBox *men1;
	TMenuBox *men2;
	TMenuBox *men3;
	TMenuBox *men4;
	TMenuBox *men5;
	TMenuBox *men6;
	TMenuBox *men7;
	TDialog *diag;
	HelpWin *helpWin;
};

Main::Main()
: TProgInit(&Main::initStatusLine, &Main::initMenuBar, &Main::initDeskTop)
{
	diag = new TDialog(TRect(20, 8, 61, 15),
		" Doriti explicatii sau exemplu?");

	if (diag)
	{
		diag->insert(new TButton(TRect(7, 3, 21, 5), "~E~xplicatii", cmOK,
			bfDefault));
		diag->insert(new TButton(TRect(24, 3, 35, 5), "E~x~emplu", cmCancel,
			bfNormal));
	}

	helpWin = NULL;

	// main menu
	menu = new TMenuBox(*new TRect(10, 3, 70, 20),
		new TMenu(
			*new TMenuItem("~1~. Functii pentru citire/scriere caractere",
				cmd1, kbNoKey, hcNoContext, 0,
			new TMenuItem("~2~. Functii pentru gestiunea dispozitivelor hard",
				cmd2, kbNoKey, hcNoContext, 0,
			new TMenuItem("~3~. Functii pentru gestiunea directoarelor si fisierelor",
				cmd3, kbNoKey, hcNoContext, 0,
			new TMenuItem("~4~. Functii pentru afisare",
				cmd4, kbNoKey, hcNoContext, 0,
			new TMenuItem("~5~. Functii pentru gestiunea drive-urilor DOS",
				cmd5, kbNoKey, hcNoContext, 0,
			new TMenuItem("~6~. Functii pentru citire/scriere fisiere",
				cmd6, kbNoKey, hcNoContext, 0,
			new TMenuItem("~7~. Functii pentru obtinerea de informatii despre sistem",
				cmd7, kbNoKey, hcNoContext, 0)))))))
		), NULL);

	// submenu for option 1 in main menu
	men1 = new TMenuBox(*new TRect(10, 3, 70, 20),
		new TMenu(
			*new TMenuItem("01h - citire caracter cu afisare",
				cmd01, kbNoKey, hcNoContext, 0,
			new TMenuItem("08h - citire caracter fara afisare",
				cmd08, kbNoKey, hcNoContext, 0,
			new TMenuItem("07h - citire caracter fara Ctrl+Break",
				cmd07, kbNoKey, hcNoContext, 0,
			new TMenuItem("06h - citire/scriere caracter fara Ctrl+Break",
				cmd06, kbNoKey, hcNoContext, 0,
			new TMenuItem("0bh - verificare disponibilitate caracter(e)",
				cmd0b, kbNoKey, hcNoContext, 0,
			new TMenuItem("0ch - golire tampon si citire caractere",
				cmd0c, kbNoKey, hcNoContext, 0,
			new TMenuItem("0ah - citire sir de caractere",
				cmd0a, kbNoKey, hcNoContext, 0,
			new TMenuItem("02h - scriere caracter",
				cmd02, kbNoKey, hcNoContext, 0,
			new TMenuItem("09h - scriere sir de caractere",
				cmd09, kbNoKey, hcNoContext, 0,
			new TMenuItem("05h - trimitere carcter catre imprimanta",
				cmd05, kbNoKey, hcNoContext, 0,
			new TMenuItem("~i~ - meniul anterior",
				cmdback, kbNoKey, hcNoContext, 0)))))))))))

		), NULL);

	// submenu for option 2 in main menu
	men2 = new TMenuBox(*new TRect(10, 3, 70, 20),
		new TMenu(
			*new TMenuItem("4406h - verificare disponibilitate date",
				cmd4406, kbNoKey, hcNoContext, 0,
			new TMenuItem("4407h - verificare disponibilitate primire",
				cmd4407, kbNoKey, hcNoContext, 0,
			new TMenuItem("4408h - verificare medii de stocare portabile",
				cmd4408, kbNoKey, hcNoContext, 0,
			new TMenuItem("~i~ - meniul anterior",
				cmdback, kbNoKey, hcNoContext, 0))))

		), NULL);

	// submenu for option 3 in main menu
	men3 = new TMenuBox(*new TRect(10, 3, 70, 20),
		new TMenu(
			*new TMenuItem("39h - creare director gol",
				cmd39, kbNoKey, hcNoContext, 0,
			new TMenuItem("3ah - stergere director gol",
				cmd3a, kbNoKey, hcNoContext, 0,
			new TMenuItem("3ch - creare fisier nou",
				cmd3c, kbNoKey, hcNoContext, 0,
			new TMenuItem("41h - stergere fisier existent",
				cmd41, kbNoKey, hcNoContext, 0,
			new TMenuItem("56h - redenumire sau mutare fisier",
				cmd56, kbNoKey, hcNoContext, 0,
			new TMenuItem("4300h - citire atribute fisier",
				cmd4300, kbNoKey, hcNoContext, 0,
			new TMenuItem("4301h - modificare atribute fisier",
				cmd4301, kbNoKey, hcNoContext, 0,
			new TMenuItem("~i~ - meniul anterior",
				cmdback, kbNoKey, hcNoContext, 0))))))))
		), NULL);

	// submenu for option 4 in main menu
	men4 = new TMenuBox(*new TRect(10, 3, 70, 20),
		new TMenu(
			*new TMenuItem("02h - scriere caracter",
				cmd02, kbNoKey, hcNoContext, 0,
			new TMenuItem("06h - citire/scriere caracter fara Ctrl+Break",
				cmd06, kbNoKey, hcNoContext, 0,
			new TMenuItem("09h - scriere sir de caractere",
				cmd09, kbNoKey, hcNoContext, 0,
			new TMenuItem("440ch (5fh) - setare mod de afisare",
				cmd440c5f, kbNoKey, hcNoContext, 0,
			new TMenuItem("440ch (7fh) - aflare mod de afisare curent",
				cmd440c7f, kbNoKey, hcNoContext, 0,
			new TMenuItem("~i~ - meniul anterior",
				cmdback, kbNoKey, hcNoContext, 0))))))
		), NULL);

	// submenu for option 5 in main menu
	men5 = new TMenuBox(*new TRect(10, 3, 70, 20),
		new TMenu(
			*new TMenuItem("19h - aflare drive DOS implicit",
				cmd19, kbNoKey, hcNoContext, 0,
			new TMenuItem("0eh - setare drive DOS implicit",
				cmd0e, kbNoKey, hcNoContext, 0,
			new TMenuItem("47h - aflare director implicit pentru un drive",
				cmd47, kbNoKey, hcNoContext, 0,
			new TMenuItem("3bh - setare director implicit pentru un drive",
				cmd3b, kbNoKey, hcNoContext, 0,
			new TMenuItem("36h - aflare spatiu disponibil pe un drive",
				cmd36, kbNoKey, hcNoContext, 0,
			new TMenuItem("3305h - aflare drive de incarcare sistem",
				cmd3305, kbNoKey, hcNoContext, 0,
			new TMenuItem("~i~ - meniul anterior",
				cmdback, kbNoKey, hcNoContext, 0)))))))
		), NULL);

	// submenu for option 6 in main menu
	men6 = new TMenuBox(*new TRect(10, 3, 70, 20),
		new TMenu(
			*new TMenuItem("3ch - creare fisier nou",
				cmd3c, kbNoKey, hcNoContext, 0,
			new TMenuItem("3dh - deschidere fisier pentru citire/scriere",
				cmd3d, kbNoKey, hcNoContext, 0,
			new TMenuItem("41h - stergere fisier existent",
				cmd41, kbNoKey, hcNoContext, 0,
			new TMenuItem("3fh - citire octeti din fisier",
				cmd3f, kbNoKey, hcNoContext, 0,
			new TMenuItem("40h - scriere octeti in fisier",
				cmd40, kbNoKey, hcNoContext, 0,
			new TMenuItem("~i~ - meniul anterior",
				cmdback, kbNoKey, hcNoContext, 0))))))
		), NULL);

	// submenu for option 7 in main menu
	men7 = new TMenuBox(*new TRect(10, 3, 70, 20),
		new TMenu(
			*new TMenuItem("30h - aflare versiune DOS folosita",
				cmd30, kbNoKey, hcNoContext, 0,
			new TMenuItem("3305h - aflare drive de incarcare sistem",
				cmd3305, kbNoKey, hcNoContext, 0,
			new TMenuItem("2ah - aflare data curenta",
				cmd2a, kbNoKey, hcNoContext, 0,
			new TMenuItem("2ch - aflare ora curenta",
				cmd2c, kbNoKey, hcNoContext, 0,
			new TMenuItem("19h - aflare drive DOS implicit",
				cmd19, kbNoKey, hcNoContext, 0,
			new TMenuItem("47h - aflare director implicit pentru un drive",
				cmd47, kbNoKey, hcNoContext, 0,
			new TMenuItem("35h - aflare adresa vector de intrerupere",
				cmd35, kbNoKey, hcNoContext, 0,
			new TMenuItem("~i~ - meniul anterior",
				cmdback, kbNoKey, hcNoContext, 0))))))))
		), NULL);

	displayMainMenu();
}

/*
 * Comment: the operator + is redefined for an "addition"
 * betweeen a TStatusDef and a TStatusItem so that it updates
 * the link in TStatusDef.
 *
 * This function is called by TProgInit's constructor and so we
 * redefine the one from TApplication so that we can get the
 * status line we want.
 */
TStatusLine *Main::initStatusLine(TRect r)
{
	r.a.y = r.b.y - 1;	// move top to 1 line above bottom
	return new TStatusLine(r,
		*new TStatusDef(0, 0xFFFF) +
		*new TStatusItem("~Alt-I~ Iesire", kbCtrlI, cmQuit) +
		*new TStatusItem("~F10~ Meniu", kbF10, cmMenu) +
		*new TStatusItem("~Alt-F3~ Inchidere fereastra", kbAltF3, cmClose));
}

/*
 * This function is called by TProgInit's constructor and so we
 * redefine the one from TApplication so that we can get the
 * menu bar we want.
 */
TMenuBar *Main::initMenuBar(TRect r)
{
	r.b.y = r.a.y + 1;	// move bottom to 1 line below top
	return new TMenuBar(r,
		*new TSubMenu("~P~rogram", kbAltF) +
			*new TMenuItem("~E~cran initial", cmdHome, kbAltE, hcNoContext,
				"Alt+E") +
			newLine() +
			*new TMenuItem("~I~esire", cmQuit, kbAltI, hcNoContext,
				"Alt+I") +
		*new TSubMenu("~A~jutor", kbAltA) +
			*new TMenuItem("~A~jutor", cmdAjutor, kbAltH, hcNoContext,
				"Alt+H") +
			newLine() +
			*new TMenuItem("~D~espre", cmdDespre, kbAltD, hcNoContext,
				"Alt+D"));
}

/*
 * Here we handle the case when the user activates one of our program's
 * commands (by means of menu).
 */
void Main::handleEvent(TEvent& event)
{
	TApplication::handleEvent(event);	// call parent method
	if (event.what == evCommand)		// and, if it's a command we're
	{									// dealing with, run our code
		switch (event.message.command)
		{
			case cmdHome:
				displayMainMenu();
				break;
			default:
				return;
		}
		clearEvent(event);
	}
}

void Main::displayMainMenu()
{
	if (helpWin != NULL)
	{
		deskTop->remove(helpWin);
		helpWin = NULL;
	}

	int test = 1;
	while (test == 1)
	{
		test = 0;
		switch (deskTop->execView(menu))
		{
			case cmd1:
				switch (deskTop->execView(men1))
				{
					case cmd01:
						if (deskTop->execView(diag) == cmOK)
						{
							// explanation window
							TRect r(0, 0, 80, 23);
							helpWin = new HelpWin(r, "Explicatii", 1,
								"01help.txt");
							deskTop->insert(helpWin);
						}
						else
						{
							suspend();
							textbackground(BLACK);
							clrscr();
							fn01h();
							while (!kbhit());
							clrscr();
							resume();
							redraw();
						}
						break;
					case cmd08:
						if (deskTop->execView(diag) == cmOK)
						{
							// explanation window
							TRect r(0, 0, 80, 23);
							helpWin = new HelpWin(r, "Explicatii", 1,
								"08help.txt");
							deskTop->insert(helpWin);
						}
						else
						{
							suspend();
							textbackground(BLACK);
							clrscr();
							fn08h();
							while (!kbhit());
							clrscr();
							resume();
							redraw();
						}
						break;
					case cmd07:
						if (deskTop->execView(diag) == cmOK)
						{
							// explanation window
							TRect r(0, 0, 80, 23);
							helpWin = new HelpWin(r, "Explicatii", 1,
								"07help.txt");
							deskTop->insert(helpWin);
						}
						else
						{
							suspend();
							textbackground(BLACK);
							clrscr();
							fn07h();
							while (!kbhit());
							clrscr();
							resume();
							redraw();
						}
						break;
					case cmd06:
						if (deskTop->execView(diag) == cmOK)
						{
							// explanation window
							TRect r(0, 0, 80, 23);
							helpWin = new HelpWin(r, "Explicatii", 1,
								"06help.txt");
							deskTop->insert(helpWin);
						}
						else
						{
							suspend();
							textbackground(BLACK);
							clrscr();
							fn06h();
							while (!kbhit());
							clrscr();
							resume();
							redraw();
						}
						break;
					case cmd0b:
						if (deskTop->execView(diag) == cmOK)
						{
							// explanation window
							TRect r(0, 0, 80, 23);
							helpWin = new HelpWin(r, "Explicatii", 1,
								"0bhelp.txt");
							deskTop->insert(helpWin);
						}
						else
						{
							suspend();
							textbackground(BLACK);
							clrscr();
							fn0bh();
							while (!kbhit());
							clrscr();
							resume();
							redraw();
						}
						break;
					case cmd0c:
						if (deskTop->execView(diag) == cmOK)
						{
							// explanation window
							TRect r(0, 0, 80, 23);
							helpWin = new HelpWin(r, "Explicatii", 1,
								"0chelp.txt");
							deskTop->insert(helpWin);
						}
						else
						{
							suspend();
							textbackground(BLACK);
							clrscr();
							fn0ch();
							while (!kbhit());
							clrscr();
							resume();
							redraw();
						}
						break;
					case cmd0a:
						if (deskTop->execView(diag) == cmOK)
						{
							// explanation window
							TRect r(0, 0, 80, 23);
							helpWin = new HelpWin(r, "Explicatii", 1,
								"0ahelp.txt");
							deskTop->insert(helpWin);
						}
						else
						{
							suspend();
							textbackground(BLACK);
							clrscr();
							fn0ah();
							while (!kbhit());
							clrscr();
							resume();
							redraw();
						}
						break;
					case cmd02:
						if (deskTop->execView(diag) == cmOK)
						{
							// explanation window
							TRect r(0, 0, 80, 23);
							helpWin = new HelpWin(r, "Explicatii", 1,
								"02help.txt");
							deskTop->insert(helpWin);
						}
						else
						{
							suspend();
							textbackground(BLACK);
							clrscr();
							fn02h();
							while (!kbhit());
							clrscr();
							resume();
							redraw();
						}
						break;
					case cmd09:
						if (deskTop->execView(diag) == cmOK)
						{
							// explanation window
							TRect r(0, 0, 80, 23);
							helpWin = new HelpWin(r, "Explicatii", 1,
								"09help.txt");
							deskTop->insert(helpWin);
						}
						else
						{
							suspend();
							textbackground(BLACK);
							clrscr();
							fn09h();
							while (!kbhit());
							clrscr();
							resume();
							redraw();
						}
						break;
					case cmd05:
						if (deskTop->execView(diag) == cmOK)
						{
							// explanation window
							TRect r(0, 0, 80, 23);
							helpWin = new HelpWin(r, "Explicatii", 1,
								"05help.txt");
							deskTop->insert(helpWin);
						}
						else
						{
							suspend();
							textbackground(BLACK);
							clrscr();
							fn05h();
							while (!kbhit());
							clrscr();
							resume();
							redraw();
						}
						break;
					case cmdback:
						test = 1;
						break;
				}
				break;
			case cmd2:
				switch (deskTop->execView(men2))
				{
					case cmd4406:
						if (deskTop->execView(diag) == cmOK)
						{
							// explanation window
							TRect r(0, 0, 80, 23);
							helpWin = new HelpWin(r, "Explicatii", 1,
								"4406help.txt");
							deskTop->insert(helpWin);
						}
						else
						{
							suspend();
							textbackground(BLACK);
							clrscr();
							fn4406h();
							while (!kbhit());
							clrscr();
							resume();
							redraw();
						}
						break;
					case cmd4407:
						if (deskTop->execView(diag) == cmOK)
						{
							// explanation window
							TRect r(0, 0, 80, 23);
							helpWin = new HelpWin(r, "Explicatii", 1,
								"4407help.txt");
							deskTop->insert(helpWin);
						}
						else
						{
							suspend();
							textbackground(BLACK);
							clrscr();
							fn4407h();
							while (!kbhit());
							clrscr();
							resume();
							redraw();
						}
						break;
					case cmd4408:
						if (deskTop->execView(diag) == cmOK)
						{
							// explanation window
							TRect r(0, 0, 80, 23);
							helpWin = new HelpWin(r, "Explicatii", 1,
								"4408help.txt");
							deskTop->insert(helpWin);
						}
						else
						{
							suspend();
							textbackground(BLACK);
							clrscr();
							fn4408h();
							while (!kbhit());
							clrscr();
							resume();
							redraw();
						}
						break;
					case cmdback:
						test = 1;
						break;
				}
				break;
			case cmd3:
				switch (deskTop->execView(men3))
				{
					case cmd39:
						if (deskTop->execView(diag) == cmOK)
						{
							// explanation window
							TRect r(0, 0, 80, 23);
							helpWin = new HelpWin(r, "Explicatii", 1,
								"39help.txt");
							deskTop->insert(helpWin);
						}
						else
						{
							suspend();
							textbackground(BLACK);
							clrscr();
							fn39h();
							while (!kbhit());
							clrscr();
							resume();
							redraw();
						}
						break;
					case cmd3a:
						if (deskTop->execView(diag) == cmOK)
						{
							// explanation window
							TRect r(0, 0, 80, 23);
							helpWin = new HelpWin(r, "Explicatii", 1,
								"3ahelp.txt");
							deskTop->insert(helpWin);
						}
						else
						{
							suspend();
							textbackground(BLACK);
							clrscr();
							fn3ah();
							while (!kbhit());
							clrscr();
							resume();
							redraw();
						}
						break;
					case cmd3c:
						if (deskTop->execView(diag) == cmOK)
						{
							// explanation window
							TRect r(0, 0, 80, 23);
							helpWin = new HelpWin(r, "Explicatii", 1,
								"3chelp.txt");
							deskTop->insert(helpWin);
						}
						else
						{
							suspend();
							textbackground(BLACK);
							clrscr();
							fn3ch();
							while (!kbhit());
							clrscr();
							resume();
							redraw();
						}
						break;
					case cmd41:
						if (deskTop->execView(diag) == cmOK)
						{
							// explanation window
							TRect r(0, 0, 80, 23);
							helpWin = new HelpWin(r, "Explicatii", 1,
								"41help.txt");
							deskTop->insert(helpWin);
						}
						else
						{
							suspend();
							textbackground(BLACK);
							clrscr();
							fn41h();
							while (!kbhit());
							clrscr();
							resume();
							redraw();
						}
						break;
					case cmd56:
						if (deskTop->execView(diag) == cmOK)
						{
							// explanation window
							TRect r(0, 0, 80, 23);
							helpWin = new HelpWin(r, "Explicatii", 1,
								"56help.txt");
							deskTop->insert(helpWin);
						}
						else
						{
							suspend();
							textbackground(BLACK);
							clrscr();
							fn56h();
							while (!kbhit());
							clrscr();
							resume();
							redraw();
						}
						break;
					case cmd4300:
						if (deskTop->execView(diag) == cmOK)
						{
							// explanation window
							TRect r(0, 0, 80, 23);
							helpWin = new HelpWin(r, "Explicatii", 1,
								"4300help.txt");
							deskTop->insert(helpWin);
						}
						else
						{
							suspend();
							textbackground(BLACK);
							clrscr();
							fn4300h();
							while (!kbhit());
							clrscr();
							resume();
							redraw();
						}
						break;
					case cmd4301:
						if (deskTop->execView(diag) == cmOK)
						{
							// explanation window
							TRect r(0, 0, 80, 23);
							helpWin = new HelpWin(r, "Explicatii", 1,
								"4301help.txt");
							deskTop->insert(helpWin);
						}
						else
						{
							suspend();
							textbackground(BLACK);
							clrscr();
							fn4301h();
							while (!kbhit());
							clrscr();
							resume();
							redraw();
						}
						break;
					case cmdback:
						test = 1;
						break;
				}
				break;
			case cmd4:
				switch (deskTop->execView(men4))
				{
					case cmd02:
						if (deskTop->execView(diag) == cmOK)
						{
							// explanation window
							TRect r(0, 0, 80, 23);
							helpWin = new HelpWin(r, "Explicatii", 1,
								"02help.txt");
							deskTop->insert(helpWin);
						}
						else
						{
							suspend();
							textbackground(BLACK);
							clrscr();
							fn02h();
							while (!kbhit());
							clrscr();
							resume();
							redraw();
						}
						break;
					case cmd06:
						if (deskTop->execView(diag) == cmOK)
						{
							// explanation window
							TRect r(0, 0, 80, 23);
							helpWin = new HelpWin(r, "Explicatii", 1,
								"06help.txt");
							deskTop->insert(helpWin);
						}
						else
						{
							suspend();
							textbackground(BLACK);
							clrscr();
							fn06h();
							while (!kbhit());
							clrscr();
							resume();
							redraw();
						}
						break;
					case cmd09:
						if (deskTop->execView(diag) == cmOK)
						{
							// explanation window
							TRect r(0, 0, 80, 23);
							helpWin = new HelpWin(r, "Explicatii", 1,
								"09help.txt");
							deskTop->insert(helpWin);
						}
						else
						{
							suspend();
							textbackground(BLACK);
							clrscr();
							fn09h();
							while (!kbhit());
							clrscr();
							resume();
							redraw();
						}
						break;
					case cmd440c5f:
						if (deskTop->execView(diag) == cmOK)
						{
							// explanation window
							TRect r(0, 0, 80, 23);
							helpWin = new HelpWin(r, "Explicatii", 1,
								"440c1hlp.txt");
							deskTop->insert(helpWin);
						}
						else
						{
							suspend();
							textbackground(BLACK);
							clrscr();
							fn440c5fh();
							while (!kbhit());
							clrscr();
							resume();
							redraw();
						}
						break;
					case cmd440c7f:
						if (deskTop->execView(diag) == cmOK)
						{
							// explanation window
							TRect r(0, 0, 80, 23);
							helpWin = new HelpWin(r, "Explicatii", 1,
								"440c2hlp.txt");
							deskTop->insert(helpWin);
						}
						else
						{
							suspend();
							textbackground(BLACK);
							clrscr();
							fn440c7fh();
							while (!kbhit());
							clrscr();
							resume();
							redraw();
						}
						break;
					case cmdback:
						test = 1;
						break;
				}
				break;
			case cmd5:
				switch (deskTop->execView(men5))
				{
					case cmd19:
						if (deskTop->execView(diag) == cmOK)
						{
							// explanation window
							TRect r(0, 0, 80, 23);
							helpWin = new HelpWin(r, "Explicatii", 1,
								"19help.txt");
							deskTop->insert(helpWin);
						}
						else
						{
							suspend();
							textbackground(BLACK);
							clrscr();
							fn19h();
							while (!kbhit());
							clrscr();
							resume();
							redraw();
						}
						break;
					case cmd0e:
						if (deskTop->execView(diag) == cmOK)
						{
							// explanation window
							TRect r(0, 0, 80, 23);
							helpWin = new HelpWin(r, "Explicatii", 1,
								"0ehelp.txt");
							deskTop->insert(helpWin);
						}
						else
						{
							suspend();
							textbackground(BLACK);
							clrscr();
							fn0eh();
							while (!kbhit());
							clrscr();
							resume();
							redraw();
						}
						break;
					case cmd47:
						if (deskTop->execView(diag) == cmOK)
						{
							// explanation window
							TRect r(0, 0, 80, 23);
							helpWin = new HelpWin(r, "Explicatii", 1,
								"47help.txt");
							deskTop->insert(helpWin);
						}
						else
						{
							suspend();
							textbackground(BLACK);
							clrscr();
							fn47h();
							while (!kbhit());
							clrscr();
							resume();
							redraw();
						}
						break;
					case cmd3b:
						if (deskTop->execView(diag) == cmOK)
						{
							// explanation window
							TRect r(0, 0, 80, 23);
							helpWin = new HelpWin(r, "Explicatii", 1,
								"3bhelp.txt");
							deskTop->insert(helpWin);
						}
						else
						{
							suspend();
							textbackground(BLACK);
							clrscr();
							fn3bh();
							while (!kbhit());
							clrscr();
							resume();
							redraw();
						}
						break;
					case cmd36:
						if (deskTop->execView(diag) == cmOK)
						{
							// explanation window
							TRect r(0, 0, 80, 23);
							helpWin = new HelpWin(r, "Explicatii", 1,
								"36help.txt");
							deskTop->insert(helpWin);
						}
						else
						{
							suspend();
							textbackground(BLACK);
							clrscr();
							fn36h();
							while (!kbhit());
							clrscr();
							resume();
							redraw();
						}
						break;
					case cmd3305:
						if (deskTop->execView(diag) == cmOK)
						{
							// explanation window
							TRect r(0, 0, 80, 23);
							helpWin = new HelpWin(r, "Explicatii", 1,
								"3305help.txt");
							deskTop->insert(helpWin);
						}
						else
						{
							suspend();
							textbackground(BLACK);
							clrscr();
							fn3305h();
							while (!kbhit());
							clrscr();
							resume();
							redraw();
						}
						break;
					case cmdback:
						test = 1;
						break;
				}
				break;
			case cmd6:
				switch (deskTop->execView(men6))
				{
					case cmd3c:
						if (deskTop->execView(diag) == cmOK)
						{
							// explanation window
							TRect r(0, 0, 80, 23);
							helpWin = new HelpWin(r, "Explicatii", 1,
								"3chelp.txt");
							deskTop->insert(helpWin);
						}
						else
						{
							suspend();
							textbackground(BLACK);
							clrscr();
							fn3ch();
							while (!kbhit());
							clrscr();
							resume();
							redraw();
						}
						break;
					case cmd3d:
						if (deskTop->execView(diag) == cmOK)
						{
							// explanation window
							TRect r(0, 0, 80, 23);
							helpWin = new HelpWin(r, "Explicatii", 1,
								"3dhelp.txt");
							deskTop->insert(helpWin);
						}
						else
						{
							suspend();
							textbackground(BLACK);
							clrscr();
							fn3dh();
							while (!kbhit());
							clrscr();
							resume();
							redraw();
						}
						break;
					case cmd41:
						if (deskTop->execView(diag) == cmOK)
						{
							// explanation window
							TRect r(0, 0, 80, 23);
							helpWin = new HelpWin(r, "Explicatii", 1,
								"41help.txt");
							deskTop->insert(helpWin);
						}
						else
						{
							suspend();
							textbackground(BLACK);
							clrscr();
							fn41h();
							while (!kbhit());
							clrscr();
							resume();
							redraw();
						}
						break;
					case cmd3f:
						if (deskTop->execView(diag) == cmOK)
						{
							// explanation window
							TRect r(0, 0, 80, 23);
							helpWin = new HelpWin(r, "Explicatii", 1,
								"3fhelp.txt");
							deskTop->insert(helpWin);
						}
						else
						{
							suspend();
							textbackground(BLACK);
							clrscr();
							fn3fh();
							while (!kbhit());
							clrscr();
							resume();
							redraw();
						}
						break;
					case cmd40:
						if (deskTop->execView(diag) == cmOK)
						{
							// explanation window
							TRect r(0, 0, 80, 23);
							helpWin = new HelpWin(r, "Explicatii", 1,
								"40help.txt");
							deskTop->insert(helpWin);
						}
						else
						{
							suspend();
							textbackground(BLACK);
							clrscr();
							fn40h();
							while (!kbhit());
							clrscr();
							resume();
							redraw();
						}
						break;
					case cmdback:
						test = 1;
						break;
				}
				break;
			case cmd7:
				switch (deskTop->execView(men7))
				{
					case cmd30:
						if (deskTop->execView(diag) == cmOK)
						{
							// explanation window
							TRect r(0, 0, 80, 23);
							helpWin = new HelpWin(r, "Explicatii", 1,
								"30help.txt");
							deskTop->insert(helpWin);
						}
						else
						{
							suspend();
							textbackground(BLACK);
							clrscr();
							fn30h();
							while (!kbhit());
							clrscr();
							resume();
							redraw();
						}
						break;
					case cmd3305:
						if (deskTop->execView(diag) == cmOK)
						{
							// explanation window
							TRect r(0, 0, 80, 23);
							helpWin = new HelpWin(r, "Explicatii", 1,
								"3305help.txt");
							deskTop->insert(helpWin);
						}
						else
						{
							suspend();
							textbackground(BLACK);
							clrscr();
							fn3305h();
							while (!kbhit());
							clrscr();
							resume();
							redraw();
						}
						break;
					case cmd2a:
						if (deskTop->execView(diag) == cmOK)
						{
							// explanation window
							TRect r(0, 0, 80, 23);
							helpWin = new HelpWin(r, "Explicatii", 1,
								"2ahelp.txt");
							deskTop->insert(helpWin);
						}
						else
						{
							suspend();
							textbackground(BLACK);
							clrscr();
							fn2ah();
							while (!kbhit());
							clrscr();
							resume();
							redraw();
						}
						break;
					case cmd2c:
						if (deskTop->execView(diag) == cmOK)
						{
							// explanation window
							TRect r(0, 0, 80, 23);
							helpWin = new HelpWin(r, "Explicatii", 1,
								"2chelp.txt");
							deskTop->insert(helpWin);
						}
						else
						{
							suspend();
							textbackground(BLACK);
							clrscr();
							fn2ch();
							clrscr();
							resume();
							redraw();
						}
						break;
					case cmd19:
						if (deskTop->execView(diag) == cmOK)
						{
							// explanation window
							TRect r(0, 0, 80, 23);
							helpWin = new HelpWin(r, "Explicatii", 1,
								"19help.txt");
							deskTop->insert(helpWin);
						}
						else
						{
							suspend();
							textbackground(BLACK);
							clrscr();
							fn19h();
							while (!kbhit());
							clrscr();
							resume();
							redraw();
						}
						break;
					case cmd47:
						if (deskTop->execView(diag) == cmOK)
						{
							// explanation window
							TRect r(0, 0, 80, 23);
							helpWin = new HelpWin(r, "Explicatii", 1,
								"47help.txt");
							deskTop->insert(helpWin);
						}
						else
						{
							suspend();
							textbackground(BLACK);
							clrscr();
							fn47h();
							while (!kbhit());
							clrscr();
							resume();
							redraw();
						}
						break;
					case cmd35:
						if (deskTop->execView(diag) == cmOK)
						{
							// explanation window
							TRect r(0, 0, 80, 23);
							helpWin = new HelpWin(r, "Explicatii", 1,
								"35help.txt");
							deskTop->insert(helpWin);
						}
						else
						{
							suspend();
							textbackground(BLACK);
							clrscr();
							fn35h();
							while (!kbhit());
							clrscr();
							resume();
							redraw();
						}
						break;
					case cmdback:
						test = 1;
						break;
				}
				break;
		}
	}
	return;
}

int main()
{
	Main app;
	app.run();
	return 0;
}