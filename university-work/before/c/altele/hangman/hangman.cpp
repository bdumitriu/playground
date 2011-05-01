// Program de simulare a jocului Spanzuratoare.

#include <iostream.h>
#include <fstream.h>
#include <stdio.h>
#include <conio.h>
#include <string.h>
#include <stdlib.h>
#include <dos.h>

char *s = new char [20];
int draw = 0, nr = 0;
int pus[30];

void Draw()
{
	draw++;
	switch (draw)
	{
		case 1: {
		 gotoxy(50, 10);
		 cprintf("(((_)))");
		 gotoxy(21, 17);
		 break;
	      }
      case 2: {
		 gotoxy(50, 11);
		 cprintf("( o o )");
		 gotoxy(21, 17);
		 break;
	      }
      case 3: {
		 gotoxy(51, 12);
		 cprintf("( | )");
		 gotoxy(21, 17);
		 break;
	      }
		case 4: {
		 gotoxy(52, 13);
		 cprintf("~~~");
		 gotoxy(21, 17);
		 break;
			}
      case 5: {
		 gotoxy(53, 14);
		 cprintf("|");
		 gotoxy(53, 15); 
		 cprintf("|");
		 gotoxy(53, 16);
		 cprintf("|");
		 gotoxy(21, 17);
		 break;
	      }
      case 6: {
		 gotoxy(52, 14);
		 cprintf("/");
		 gotoxy(50, 15);
		 cprintf("/");
		 gotoxy(21, 17);
		 break;
	      }
      case 7: {
		 gotoxy(54, 14);
		 cprintf("\\");
		 gotoxy(56, 15);
		 cprintf("\\");
		 gotoxy(21, 17);
		 break;
	      }
      case 8: {
		 gotoxy(52, 17);
		 cprintf("/");
		 gotoxy(50, 18);
		 cprintf("/");
		 gotoxy(21, 17);
		 break;
	      }
		case 9: {
		 gotoxy(54, 17);
		 cprintf("\\");
		 gotoxy(56, 18);
		 cprintf("\\");
		 gotoxy(21, 17);
		 break;
	      }
   }

   return;
}

void Cauta(char c)
{
   int len = strlen(s), t = 0;

   pus[c-64] = 0;
   gotoxy((c-64)*2, 1);
   textbackground(RED);
	cprintf("%c", c);
	textbackground(BLACK);
	for (int i = 0; i < len; i++)
	{
		if (c == s[i])
		{
	 gotoxy(10 + 2*i, 20);
	 cprintf("%c", c);
	 nr++;
	 t = 1;
		}
	}
	if (t == 0)
		Draw();
	gotoxy(21, 17);

	return;
}

void main()
{
	FILE *f;
	char c;
	char pass[16] = "\0", RealPass[16] = "hg";

	for (int k = 1; k < 27; k++)
		pus[k] = 1;

	if ((f = fopen("dict.dat", "r")) == NULL)
	{
		cprintf("Error opening dictionary file.");
		getch();
		return;
	}

	s[0] = '\0';
	randomize();
	/*int zz = random(3548);
	for (k = 0; k < zz; k++)
		fgets(s, 20, f);
	*/
	fgets(s, 20, f);
	s[strlen(s)] = '\0';
	fclose(f);

	textbackground(BLACK);
	clrscr();

	randomize();
	textcolor(LIGHTGRAY);
	for (int i = 0; i < 170; i++)
	{
		gotoxy(random(80), random(25));
		cprintf("њ");
	}
	textbackground(BLUE);
	textcolor(CYAN);
	window(20, 9, 60, 15);
	clrscr();
	window(1, 1, 80, 25);

	gotoxy(20, 9);
	cprintf("к");
	for (i = 0; i < 39; i++)
		cprintf("Ф");
	cprintf("П");
	for (i = 10; i < 15; i++)
	{
		gotoxy(20, i);
		cprintf("Г");
		gotoxy(60, i);
		cprintf("Г");
	}
	gotoxy(20, 15);
	cprintf("Р");
	for (i = 1; i < 40; i++)
		cprintf("Ф");
	cprintf("й");

	textbackground(BLACK);
	textcolor(RED);
	gotoxy(22, 16);
	for (i = 1; i <= 40; i++)
	{
		cprintf("л");
	}
	for (i = 10; i < 16; i++)
   {
      gotoxy(61, i);
      cprintf("л");
   }

   textbackground(BLUE);
   textcolor(GREEN);
   gotoxy(30, 12);
   cprintf("Parola : ");
   textcolor(BLACK);
   cprintf("ллллллллллллллл");
   textbackground(BLACK);
   textcolor(LIGHTGREEN);
   gotoxy(39, 12);
   c = getch();
   i = 1;
   while ((c != 13) && (i < 15))
   {
      cprintf("*");
      char *aux, *aux1;
      aux1 = aux = new char;
      aux = &c;
      strcat(pass, aux);
      pass[i] = '\0';
      delete aux1;
		c = getch();
		i++;
	}
	i = 0;
	if (!strcmp(pass, RealPass))
	{
		textbackground(BLACK);
		textcolor(LIGHTGRAY);
		clrscr();
		cprintf(" A B C D E F G H I J K L M N O P Q R S T U V W X Y Z");
		int len = strlen(s);
		gotoxy(10, 20);
		for (i = 0; i < len; i++)
			printf("_ ");
      gotoxy(11, 16);
      printf("Please select a letter : ");
      gotoxy(21, 17);
      int tst = 0;
      do
      {
	 c = getch();
	 switch(c)
	 {
	    case 65 : if (pus[c-64]) Cauta('A'); break;
	    case 66 : if (pus[c-64]) Cauta('B'); break;
	    case 67 : if (pus[c-64]) Cauta('C'); break;
	    case 68 : if (pus[c-64]) Cauta('D'); break;
	    case 69 : if (pus[c-64]) Cauta('E'); break;
	    case 70 : if (pus[c-64]) Cauta('F'); break;
	    case 71 : if (pus[c-64]) Cauta('G'); break;
	    case 72 : if (pus[c-64]) Cauta('H'); break;
	    case 73 : if (pus[c-64]) Cauta('I'); break;
	    case 74 : if (pus[c-64]) Cauta('J'); break;
	    case 75 : if (pus[c-64]) Cauta('K'); break;
	    case 76 : if (pus[c-64]) Cauta('L'); break;
	    case 77 : if (pus[c-64]) Cauta('M'); break;
	    case 78 : if (pus[c-64]) Cauta('N'); break;
	    case 79 : if (pus[c-64]) Cauta('O'); break;
	    case 80 : if (pus[c-64]) Cauta('P'); break;
	    case 81 : if (pus[c-63]) Cauta('Q'); break;
	    case 82 : if (pus[c-64]) Cauta('R'); break;
	    case 83 : if (pus[c-64]) Cauta('S'); break;
	    case 84 : if (pus[c-64]) Cauta('T'); break;
	    case 85 : if (pus[c-64]) Cauta('U'); break;
	    case 86 : if (pus[c-64]) Cauta('V'); break;
	    case 87 : if (pus[c-64]) Cauta('W'); break;
	    case 88 : if (pus[c-64]) Cauta('X'); break;
	    case 89 : if (pus[c-64]) Cauta('Y'); break;
	    case 90 : if (pus[c-64]) Cauta('Z'); break;
	    case 97 : if (pus[c-96]) Cauta('A'); break;
	    case 98 : if (pus[c-96]) Cauta('B'); break;
	    case 99 : if (pus[c-96]) Cauta('C'); break;
	    case 100 : if (pus[c-96]) Cauta('D'); break;
	    case 101 : if (pus[c-96]) Cauta('E'); break;
	    case 102 : if (pus[c-96]) Cauta('F'); break;
	    case 103 : if (pus[c-96]) Cauta('G'); break;
	    case 104 : if (pus[c-96]) Cauta('H'); break;
	    case 105 : if (pus[c-96]) Cauta('I'); break;
	    case 106 : if (pus[c-96]) Cauta('J'); break;
	    case 107 : if (pus[c-96]) Cauta('K'); break;
	    case 108 : if (pus[c-96]) Cauta('L'); break;
	    case 109 : if (pus[c-96]) Cauta('M'); break;
	    case 110 : if (pus[c-96]) Cauta('N'); break;
	    case 111 : if (pus[c-96]) Cauta('O'); break;
	    case 112 : if (pus[c-96]) Cauta('P'); break;
	    case 113 : if (pus[c-96]) Cauta('Q'); break;
	    case 114 : if (pus[c-96]) Cauta('R'); break;
	    case 115 : if (pus[c-96]) Cauta('S'); break;
	    case 116 : if (pus[c-96]) Cauta('T'); break;
	    case 117 : if (pus[c-96]) Cauta('U'); break;
	    case 118 : if (pus[c-96]) Cauta('V'); break;
	    case 119 : if (pus[c-96]) Cauta('W'); break;
	    case 120 : if (pus[c-96]) Cauta('X'); break;
	    case 121 : if (pus[c-96]) Cauta('Y'); break;
	    case 122 : if (pus[c-96]) Cauta('Z'); break;
	 }
	 if (draw == 7)
	 {
	    clrscr();
	    gotoxy(20, 12);
	    textcolor(RED);
	    cprintf("You're DEAD !!! Ha ha ha.");
	    textcolor(LIGHTGRAY);
	    gotoxy(25, 14);
	    cprintf("The word was %s.", s);
	    getch();
	    tst = 1;
	 }
	 if (nr == len)
	 {
	    clrscr();
	    gotoxy(20, 12);
	    textcolor(RED);
	    cprintf("Bravo, bravo, you've won!");
	    textcolor(LIGHTGRAY);
	    gotoxy(20, 14);
	    cprintf("Indeed, the word was %s.", s);
	    getch();
	    tst = 1;
	 }
      }
      while (tst == 0);
   }
   else
   {
      textbackground(BLACK);
      textcolor(RED);
      clrscr();
      gotoxy(25, 12);
      cprintf("Aaa... WRONG PASSWORD !!!");
      gotoxy(10, 13);
      cprintf("So, you're trying to use this program without a licence.");
      gotoxy(11, 14);
      cprintf("In that case tomorrow you will have to pay a fee of $100.");
      gotoxy(11, 16);
      textcolor(GREEN);
      cprintf("And right now your computer will self-format drive C:\ in");
      gotoxy(30, 18);
      cprintf("seconds...");
      textcolor(YELLOW);
      for (int i = 5; i >= 0; i--)
      {
	 gotoxy(33, 17);
	 cprintf("%d", i);
	 if (i >= 0)
	    delay(1000);
      }
      clrscr();
      textcolor(LIGHTGRAY);
      gotoxy(25, 12);
      cprintf("Formatting drive C:\...");
      getch();
      getch();
      getch();
      getch();
      getch();
      getch();
      getch();
      getch();
      getch();
      getch();
   }

   delete [] s;
   textbackground(BLACK);
   textcolor(LIGHTGRAY);
   clrscr();
   return;
}