#include "commands.h"

#define CF			1	/* Carry flag */
#define ZF			64	/* Zero flag */

// various file attribute masks
#define Read_Only	1
#define Hidden		2
#define System		4
#define VolumeLabel	8
#define SubDir		16
#define	Archive		32

/*
 * Reads one of the values 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, A, B, C, D, E, F,
 * a, b, c, d, e, f from the user, not allowing him/her to input anything
 * else. Converts the to lower case. Then returns the value.
 */
char readHexValue()
{
	int test = 1;
	char c;
	do
	{
		c = getch();

		if ((c >= '0') && (c <= '9'))
			test = 0;
		if ((c >= 'A') && (c <= 'F'))
			test = 0;
		if ((c >= 'a') && (c <= 'f'))
			test = 0;
	}
	while (test);

	if ((c >= 'A') && (c <= 'F'))
		c = c + ('a' - 'A');

	printf("%c", c);

	return c;
}

/*
 * Converts a hexadecimal digit to its decimal counterpart.
 */
unsigned char hexToDec(char hexDigit)
{
	if ((hexDigit >= '0') && (hexDigit <= '9'))
		return hexDigit - '0';
	else
		switch (hexDigit)
		{
			case 'a':
			case 'A':
				return 10;
				//break;
			case 'b':
			case 'B':
				return 11;
				//break;
			case 'c':
			case 'C':
				return 12;
				//break;
			case 'd':
			case 'D':
				return 13;
				//break;
			case 'e':
			case 'E':
				return 14;
				//break;
			case 'f':
			case 'F':
				return 15;
				//break;
			default:
				return 0;
		}
}

/*
 * Reads up to limit characters from the user and puts them
 * into memory at the address pointed by memadr. Also adds an
 * endChar at the end of the read string.
 */
void readString(char *memadr, int limit, char endChar)
{
	int n = 0, ready = 0;

	do
	{
		if ((memadr[n] = getch()) == 0)
		{
			memadr[n] = getch();
			printf("%c", memadr[n]);
		}
		else if (memadr[n] == 13)
		{
			ready = 1;
			n--;
		}
		else if (memadr[n] == 8)
		{
			printf("%c", memadr[n]);
			n -= 2;
		}
		else
		{
			printf("%c", memadr[n]);
		}
		n++;
	}
	while ((n < limit) && (!ready));
	memadr[n] = endChar;
}

/*
 * Reads a Yes/No value. Returns 1 if Yes is read (by typing 'd' or 'D'
 * from Yes in Romanian) and 0 if No is read (by typing 'n' or 'N').
 */
readDN()
{
	int test = 1;
	char c;
	do
	{
		c = getch();
		if ((c == 'd') || (c == 'D') || (c == 'n') || (c == 'N'))
			test = 0;
	}
	while (test);

	printf("%c", c);
	if ((c == 'd') || (c == 'D'))
		return 1;
	else
		return 0;
}

void fn01h()
{
	printf("Tastati un caracter si observati cum apare pe ecran in urma\n");
	printf("preluarii sale prin intermediul functiei dos 01h. Dupa\n");
	printf("aceea apasati orice alta tasta pentru a reveni in program.");
	printf("\n\n");
	asm	mov ah, 01h;
	asm int 21h;
	asm cmp al, 0
	asm jne _end_
	asm mov ah, 01h
	asm int 21h
_end_:
}

void fn08h()
{
	printf("Tastati un caracter si cititi apoi caracterul al carui\n");
	printf("cod ascii se gaseste in registrul al dupa executia functiei\n");
	printf("dos 08h. Dupa aceea apasati orice alta tasta pentru a\n");
	printf("reveni in program.");
	printf("\n\n");

	union REGS regs;

	regs.h.ah = 8;
	int86(0x21, &regs, &regs);
	if (regs.h.al == 0)
	{
		regs.h.ah = 8;
		int86(0x21, &regs, &regs);
	}

	char c = regs.h.al;
	printf("Caracterul din al: %c", c);
}

void fn07h()
{
	printf("Tastati un caracter si cititi apoi caracterul al carui\n");
	printf("cod ascii se gaseste in registrul al dupa executia functiei\n");
	printf("dos 08h. Dupa aceea apasati orice alta tasta pentru a\n");
	printf("reveni in program.");
	printf("\n\n");

	union REGS regs;

	regs.h.ah = 0x07;
	int86(0x21, &regs, &regs);
	if (regs.h.al == 0)
	{
		regs.h.ah = 7;
		int86(0x21, &regs, &regs);
	}

	char c = regs.h.al;
	printf("Caracterul din al: %c", c);
}

void fn06h()
{
	printf("Introduceti prima data valoarea care doriti sa o inscrieti\n");
	printf("in registrul dl. Aceasta trebuie sa fie intre 00 si ff:\n");
	printf(" Valoarea: ");
	char c1 = readHexValue();
	char c2 = readHexValue();
	printf("\n\n");

	union REGS regs;

	regs.h.ah = 0x06;
	int x = hexToDec(c1) * 16 + hexToDec(c2);
	regs.h.dl = x;

	if ((c1 == 'f') && (c2 == 'f'))
	{
		printf("Pentru aceasta valoare se face citirea unui caracter deja disponibil\n");
		printf("de la intrarea standard. Pentru a putea pune in evidenta efectul\n");
		printf("functiei veti avea o pauza de 3 secunde in care puteti sau nu introduce\n");
		printf("un caracter de la tastatura. Daca veti introduce un astfel de caracter\n");
		printf("el va fi citit, altfel nu se va citi nimic. Apasati orice tasta pentru\n");
		printf("a incepe pauza de 3 secunde. (Caracterele introduse nu vor fi afisate).");

		if (!getch())
			getch();
		delay(3000);

		int86(0x21, &regs, &regs);

		if (regs.x.flags & ZF) // zero flag set
		{
			printf("\n\nNici un caracter nu a fost disponibil.");
		}
		else	// zero flag not set
		{
			printf("\n\nS-a citit caracterul... %c.", regs.h.al);

			while (kbhit())
				getch();
		}
	}
	else
	{
		printf("Pentru aceasta valoare se afiseaza la iesirea standard caracterul\n");
		printf("cu codul ascii corespunzator. Acesta este... ");
		int86(0x21, &regs, &regs);
	}

	printf("\n\nPuteti apasa orice tasta pentru a reveni in program.");
}

void fn0bh()
{
	printf("Se citieste starea intrarii standard pentru a afla daca\n");
	printf("exista sau nu vreun caracter disponibil pentru citire.\n");
	printf("Valoarea care se obtine in al in urma apelului functiei\n");
	printf("este... ");

	union REGS regs;

	regs.h.ah = 0x0b;
	int86(0x21, &regs, &regs);
	printf("%d", regs.h.al);

	printf("\n\nPuteti apasa orice tasta pentru a reveni in program.");
}

void fn0ch()
{
	printf("Registrul al a fost incarcat cu valoarea 01h corespunzatoare\n");
	printf("citirii unui caracter de la intrarea standard. Se face intai\n");
	printf("golirea zonei tampon a acesteia. Introduceti caracterul:");
	printf("\n\n");

	union REGS regs;

	regs.h.ah = 0x0c;
	regs.h.al = 0x01;
	int86(0x21, &regs, &regs);

	printf("\n\nPuteti apasa orice tasta pentru a reveni in program.");
}

void fn0ah()
{
	char memadr[13];
	int n;

	union REGS regs;
	struct SREGS sregs;

	printf("Pentru a testa functia 0ah va rog introduceti numarul maxim\n");
	printf("de caractere care doriti sa fie citite (intre 1 si 10).\n");
	do
	{
		printf("Numarul (intre 1 si 10): ");
		scanf("%d", &n);
	}
	while (!((n >= 1) && (n <= 10)));
	printf("Numarul dat de dvs. a fost scris in memorie la o adresa\n");
	printf("oarecare, iar offsetul adresei respective a fost scris\n");
	printf("in registrul dx si segmentul ei in registrul ds (dupa\n");
	printf("cum este necesar). Veti putea observa o data ce incepeti\n");
	printf("sa scrieti ca nu veti avea voie sa introduceti decat\n");
	printf("un numar de caractere egal cu numarul introdus mai sus.\n");
	printf("Puteti incepe sa scrieti. Pentru a termina, apasati\n");
	printf("tasta ENTER...\n\n");

	memadr[0] = n + 1;
	regs.h.ah = 0x0a;
	regs.x.dx = FP_OFF(&memadr);
	sregs.ds = FP_SEG(&memadr);
	int86x(0x21, &regs, &regs, &sregs);

	printf("\n\nPuteti apasa orice tasta pentru a reveni in program.");
}

void fn02h()
{
	printf("Pentru a testa functia 02h (scrierea unui caracter pe ecran\n");
	printf("introduceti va rog codul ascii al caracterului care vreti sa\n");
	printf("fie afisat.\n");
	printf("Codul ascii (intre 00 si ff): ");
	char c1 = readHexValue();
	char c2 = readHexValue();
	printf("\n");
	printf("Acest cod a fost scris in dl si s-a apelat functia 02h.\n");
	printf("Iata rezultatul:\n\n");
	printf("Caracterul cu codul ascii %c%c este ", c1, c2);

	union REGS regs;

	regs.h.ah = 0x02;
	regs.h.dl = hexToDec(c1) * 16 + hexToDec(c2);
	int86(0x21, &regs, &regs);

	printf(".\n\nPuteti apasa orice tasta pentru a reveni in program.");
}

void fn09h()
{
	char memadr[11];

	union REGS regs;
	struct SREGS sregs;

	printf("Pentru a testa functia 09h va rog introduceti un sir de\n");
	printf("maxim 10 caractere care urmeaza sa fie apoi afisat folosind\n");
	printf("functia 09h.\nIntroduceti va rog sirul: ");

	readString(memadr, 10, '$');

	printf("\nSirul dat de dvs. a fost scris in memorie la o adresa\n");
	printf("oarecare, iar offsetul adresei respective a fost scris\n");
	printf("in registrul dx si segmentul ei in registrul ds (dupa\n");
	printf("cum este necesar). Iar acum se apeleaza functia 09h.\n");
	printf("Iata rezultatul:\n\n");

	regs.h.ah = 0x09;
	regs.x.dx = FP_OFF(&memadr);
	sregs.ds = FP_SEG(&memadr);
	int86x(0x21, &regs, &regs, &sregs);

	printf("\n\nPuteti apasa orice tasta pentru a reveni in program.");
}

void fn05h()
{
	printf("Pentru a testa functia 05h (trimiterea unui caracter catre\n");
	printf("imprimanta) introduceti va rog codul ascii al caracterului\n");
	printf("care vreti sa fie tiparit.\n");
	printf("Codul ascii (intre 00 si ff): ");
	char c1 = readHexValue();
	char c2 = readHexValue();
	printf("\n");
	printf("Acest cod a fost scris in dl si s-a apelat functia 05h.\n");
	printf("In mod normal, daca aveti o imprimanta conectata la calcu-");
	printf("lator si este pornita, ar trebui sa se tipareasca caracterul\n");
	printf("cu codul ascii introdus de dvs.");

	union REGS regs;

	regs.h.ah = 0x05;
	regs.h.dl = hexToDec(c1) * 16 + hexToDec(c2);
	int86(0x21, &regs, &regs);

	printf(".\n\nPuteti apasa orice tasta pentru a reveni in program.");
}

void fn4406h()
{
	printf("Vom exemplifica functionarea functiei 4406h in modul\n");
	printf("urmator: vom scrie in registrul bx valoarea 0000h, valoare\n");
	printf("corespunzatoare fluxului de intrare (standard input).\n");
	printf("Veti avea o pauza de 3 secunde in care puteti introduce\n");
	printf("sau nu caractere aleatoare (nu vor apare pe ecran). La\n");
	printf("sfarsitul acestei perioade se va apela functia 4406h.\n");
	printf("In functie de raspunsul acestei functii veti vedea daca\n");
	printf("exista sau nu caractere disponibile pentru citire la\n");
	printf("intrarea standard. Apasati orice tasta dupa citirea acestui\n");
	printf("mesaj pentru a activa pauza de 3 secunde.\n\n");
	if (!getch())
		getch();

	union REGS regs;

	delay(3000);

	regs.x.ax = 0x4406;
	regs.x.bx = 0x0000;
	int86(0x21, &regs, &regs);

	if (regs.h.al)
		printf("Exista caractere disponibile.");
	else
		printf("Nu exista caractere disponibile.");

	while (kbhit())
		getch();

	printf("\n\nPuteti apasa orice tasta pentru a reveni in program.");
}

void fn4407h()
{
	int n;

	printf("Pentru a exemplifica functionarea functiei 4407h alegeti\n");
	printf("va rog care din urmatoarele dispozitive standard doriti\n");
	printf("sa-l interogati pentru a determina daca este sau nu gata\n");
	printf("pentru a primi date (in mod normal, toate ar trebui sa\n");
	printf("raspunda ca sunt gata). Dispozitivele sunt:\n");
	printf(" 1 - intrarea standard\n");
	printf(" 2 - iesirea standard\n");
	printf(" 3 - iesirea de eroare\n");
	printf(" 4 - dispozitivul auxiliar\n");
	printf(" 5 - imprimanta standard\n");

	do
	{
		printf("Alegerea dvs. (intre 1 si 5): ");
		scanf("%d", &n);
	}
	while (!((n >= 1) && (n <= 5)));

	union REGS regs;

	regs.x.ax = 0x4407;
	regs.x.bx = n - 1;
	int86(0x21, &regs, &regs);

	if (regs.h.al)
		printf("Dispozitivul e gata pentru a primi date.");
	else
		printf("Dispozitivul nu e gata pentru a primi date.");

	printf("\n\nPuteti apasa orice tasta pentru a reveni in program.");
}

void fn4408h()
{
	int n;

	printf("Pentru a exemplifica functia 4408h va rog alegeti unul\n");
	printf("dintre cele doua drive-uri ale calculatorului dvs.\n");
	printf(" 1 - drive-ul A\n");
	printf(" 2 - drive-ul C\n");

	do
	{
		printf("Alegerea dvs. (1 sau 2): ");
		scanf("%d", &n);
	}
	while ((n != 1) && (n != 2));

	printf("Codul drive-ului ales a fost incarcat in registrul bl\n");
	printf("si acum se apeleaza functia DOS 4408h. In urma apelului\n");
	printf("aflam ca...\n\n");
	union REGS regs;

	regs.x.ax = 0x4408;
	if (n == 1)
		regs.h.bl = 1;
	else
		regs.h.bl = 3;
	int86(0x21, &regs, &regs);

	if (regs.x.ax)
		printf("Drive-ul specificat nu foloseste medii de stocare portabile.");
	else
		printf("Drive-ul specificat foloseste medii de stocare portabile.");

	printf("\n\nPuteti apasa orice tasta pentru a reveni in program.");
}

void fn39h()
{
	char memadr[66];

	union REGS regs;
	struct SREGS sregs;

	printf("Pentru a testa functia 39h (crearea unui director) intro-\n");
	printf("duceti va rog numele directorului de creat, precedat sau nu\n");
	printf("de o cale. Daca nu va fi precedat de o cale, se va crea in\n");
	printf("directorul curent. Daca calea nu exista, directorul nu va\n");
	printf("fi creat. Din numele directorului se vor considera maxim\n");
	printf("8 caractere.\n");
	printf("Introduceti calea: (maxim 64 de caractere)\n");

	readString(memadr, 64, '\0');

	printf("\nSirul dat de dvs. a fost scris in memorie la o adresa\n");
	printf("oarecare, iar offsetul adresei respective a fost scris\n");
	printf("in registrul dx si segmentul ei in registrul ds (dupa\n");
	printf("cum este necesar). Iar acum se apeleaza functia 39h.\n");
	printf("Daca calea a fost introdusa corect, directorul ar trebui\n.");
	printf("sa fi fost creat.");

	regs.h.ah = 0x39;
	regs.x.dx = FP_OFF(&memadr);
	sregs.ds = FP_SEG(&memadr);
	int86x(0x21, &regs, &regs, &sregs);

	printf("\n\nPuteti apasa orice tasta pentru a reveni in program.");
}

void fn3ah()
{
	char memadr[66];

	union REGS regs;
	struct SREGS sregs;

	printf("Pentru a testa functia 3ah (stergerea unui director gol(!))\n");
	printf("introduceti va rog numele directorului de sters, precedat sau\n");
	printf("nu de o cale. Daca nu este precedat de o cale se cauta in\n");
	printf("directorul curent. Din numele directorului se vor considera\n");
	printf("maxim 8 caractere. Daca directorul nu este gol, nu va fi sters.\n");
	printf("Daca directorul este cel curent, nu va fi sters. Introduceti\n");
	printf("calea: (maxim 64 de caractere)\n");

	readString(memadr, 64, '\0');

	printf("\nSirul dat de dvs. a fost scris in memorie la o adresa\n");
	printf("oarecare, iar offsetul adresei respective a fost scris\n");
	printf("in registrul dx si segmentul ei in registrul ds (dupa\n");
	printf("cum este necesar). Iar acum se apeleaza functia 3ah.\n");
	printf("Daca calea a fost introdusa corect si directorul a fost gol,\n");
	printf("el ar trebui sa fi fost sters.");

	regs.h.ah = 0x3a;
	regs.x.dx = FP_OFF(&memadr);
	sregs.ds = FP_SEG(&memadr);
	int86x(0x21, &regs, &regs, &sregs);

	printf("\n\nPuteti apasa orice tasta pentru a reveni in program.");
}

void fn3ch()
{
	char memadr[66];

	union REGS regs;
	struct SREGS sregs;

	printf("Pentru a testa functia 3ch (crearea unui fisier) intro-\n");
	printf("duceti va rog numele fisierului de creat, precedat sau nu\n");
	printf("de o cale. Daca nu va fi precedat de o cale, se va crea in\n");
	printf("directorul curent. Daca calea nu exista, fisierul nu va\n");
	printf("fi creat. Daca un fisier cu acelasi nume exista deja,\n");
	printf("continutul sau va fi sters (cu exceptia cazului in care\n");
	printf("are atributul read-only setat). Din numele fisierului se\n");
	printf("vor considera maxim 8 caractere plus inca 3 pentru extensie.\n");
	printf("Introduceti numele fisierului: (maxim 64 de caractere)\n");

	readString(memadr, 64, '\0');

	printf("\nSirul dat de dvs. a fost scris in memorie la o adresa\n");
	printf("oarecare, iar offsetul adresei respective a fost scris\n");
	printf("in registrul dx si segmentul ei in registrul ds (dupa\n");
	printf("cum este necesar). Iar acum se apeleaza functia 3ch.\n");
	printf("Daca calea a fost introdusa corect si un fisier cu acelasi\n");
	printf("nume si cu atributul read-only setat nu exista, fisierul ar\n");
	printf("trebui sa fi fost creat.");

	regs.h.ah = 0x3c;
	regs.x.cx = 0;
	regs.x.dx = FP_OFF(&memadr);
	sregs.ds = FP_SEG(&memadr);
	int86x(0x21, &regs, &regs, &sregs);
	regs.x.bx = regs.x.ax;
	regs.h.ah = 0x3e;
	int86(0x21, &regs, &regs);

	printf("\n\nPuteti apasa orice tasta pentru a reveni in program.");
}

void fn41h()
{
	char memadr[66];

	union REGS regs;
	struct SREGS sregs;

	printf("Pentru a testa functia 41h (stergerea unui fisier) introdu-\n");
	printf("ceti va rog numele fisierului de sters, precedat sau nu de o\n");
	printf("cale. Daca nu este precedat de o cale se cauta in directorul\n");
	printf("curent. Din numele fisierului se vor considera maxim 8 ca-\n");
	printf("ractere plus inca 3 pentru extensie. Daca fisierul are setat\n");
	printf("atributul de read-only, nu va fi sters. Introduceti numele\n");
	printf("fisierului si eventual calea (maxim 64 de caractere)\n");

	readString(memadr, 64, '\0');

	printf("\nSirul dat de dvs. a fost scris in memorie la o adresa\n");
	printf("oarecare, iar offsetul adresei respective a fost scris\n");
	printf("in registrul dx si segmentul ei in registrul ds (dupa\n");
	printf("cum este necesar). Iar acum se apeleaza functia 41h.\n");
	printf("Daca calea a fost introdusa corect si fisierul nu avea\n");
	printf("atributul read-only setat, el ar trebui sa fi fost sters.");

	regs.h.ah = 0x41;
	regs.x.dx = FP_OFF(&memadr);
	sregs.ds = FP_SEG(&memadr);
	int86x(0x21, &regs, &regs, &sregs);

	printf("\n\nPuteti apasa orice tasta pentru a reveni in program.");
}

void fn56h()
{
	char memadr1[66];
	char memadr2[66];

	union REGS regs;
	struct SREGS sregs;

	printf("Pentru a testa functia 56h (redenumirea sau mutarea unui\n");
	printf("fisier) introduceti va rog numele fisierului de mutat.\n");
	printf("Acest nume poate sa contina si calea catre fisier. Daca\n");
	printf("calea este omisa, fisierul va fi cautat in directorul\n");
	printf("curent. Fisierul trebuie sa existe. Introduceti acum\n");
	printf("numele fisierului (maxim 64 de caractere)\n");

	readString(memadr1, 64, '\0');

	printf("\nSirul dat de dvs. a fost scris in memorie la o adresa\n");
	printf("oarecare, iar offsetul adresei respective a fost scris\n");
	printf("in registrul dx si segmentul ei in registrul ds (dupa\n");
	printf("cum este necesar). Acum introduceti va rog noul nume al\n");
	printf("fisierului (maxim 64 de caracter)\n");

	readString(memadr2, 64, '\0');

	printf("\nSirul dat de dvs. a fost scris in memorie la o adresa\n");
	printf("oarecare, iar offsetul adresei respective a fost scris\n");
	printf("in registrul di si segmentul ei in registrul es (dupa\n");
	printf("cum este necesar). Iar acum se apeleaza functia 56h.\n");
	printf("Daca totul a fost corect (vezi si explicatiile), fisierul ar\n");
	printf("trebui sa fi fost creat.");

	regs.h.ah = 0x56;
	regs.x.dx = FP_OFF(&memadr1);
	sregs.ds = FP_SEG(&memadr1);
	regs.x.di = FP_OFF(&memadr2);
	sregs.es = FP_SEG(&memadr2);
	int86x(0x21, &regs, &regs, &sregs);

	printf("\n\nPuteti apasa orice tasta pentru a reveni in program.");
}

void fn4300h()
{
	char memadr[66];

	union REGS regs;
	struct SREGS sregs;

	printf("Pentru a testa functia 4300h (aflarea atributelor unui fisier)\n");
	printf("introduceti va rog numele fisierului in cauza, precedat sau nu\n");
	printf("de o cale. Daca nu este precedat de o cale se cauta in direc-\n");
	printf("torul curent. Din numele fisierului se vor considera maxim 8\n");
	printf("caractere plus inca 3 pentru extensie. Introduceti numele\n");
	printf("fisierului si eventual calea (maxim 64 de caractere)\n");

	readString(memadr, 64, '\0');

	printf("\nSirul dat de dvs. a fost scris in memorie la o adresa\n");
	printf("oarecare, iar offsetul adresei respective a fost scris\n");
	printf("in registrul dx si segmentul ei in registrul ds (dupa\n");
	printf("cum este necesar). Iar acum se apeleaza functia 4300h.\n");
	printf("Rezultatul acesteia:\n\n");

	regs.x.ax = 0x4300;
	regs.x.dx = FP_OFF(&memadr);
	sregs.ds = FP_SEG(&memadr);
	int86x(0x21, &regs, &regs, &sregs);

	if (regs.x.flags & CF) // carry flag set
	{
		printf("Eroare! Verificati daca fisierul exista si calea e buna.\n");
	}
	else	// carry flag not set
	{
		printf("Fisierul are setate urmatoarele atribute:\n");
		if (regs.x.cx & Read_Only)
			printf(" - read-only\n");
		if (regs.x.cx & Hidden)
			printf(" - ascuns\n");
		if (regs.x.cx & System)
			printf(" - fisier sistem\n");
		if (regs.x.cx & Archive)
			printf(" - arhiva\n");
		if (regs.x.cx & SubDir)
			printf(" - subdirector\n");
		if (regs.x.cx & VolumeLabel)
			printf(" - radacina\n");
	}

	printf("\nPuteti apasa orice tasta pentru a reveni in program.");
}

void fn4301h()
{
	char memadr[66];
	int attr = 0;

	union REGS regs;
	struct SREGS sregs;

	printf("Pentru a testa functia 4301h (setarea atributelor unui fisier)\n");
	printf("introduceti va rog numele fisierului in cauza, precedat sau nu\n");
	printf("de o cale. Daca nu este precedat de o cale se cauta in direc-\n");
	printf("torul curent. Din numele fisierului se vor considera maxim 8\n");
	printf("caractere plus inca 3 pentru extensie. Introduceti numele\n");
	printf("fisierului si eventual calea (maxim 64 de caractere)\n");

	readString(memadr, 64, '\0');

	printf("\nSirul dat de dvs. a fost scris in memorie la o adresa\n");
	printf("oarecare, iar offsetul adresei respective a fost scris\n");
	printf("in registrul dx si segmentul ei in registrul ds (dupa\n");
	printf("cum este necesar). Acum specificati va rog prin da sau nu\n");
	printf("daca urmatorii biti doriti sa fie setati pentru fisierul\n");
	printf("dat.");
	printf("\nBitul de read-only? (D/N) ");
	if (readDN())
		attr = attr | Read_Only;
	printf("\nBitul de ascuns? (D/N) ");
	if (readDN())
		attr = attr | Hidden;
	printf("\nBitul de sistem? (D/N) ");
	if (readDN())
		attr = attr | System;
	printf("\nBitul de arhiva? (D/N) ");
	if (readDN())
		attr = attr | Archive;

	regs.x.ax = 0x4301;
	regs.x.dx = FP_OFF(&memadr);
	sregs.ds = FP_SEG(&memadr);
	regs.x.cx = attr;
	int86x(0x21, &regs, &regs, &sregs);

	printf("\n\nAcum s-a apelat functia 4301h cu valorile date de dvs.\n");
	printf("In mod normal, daca fisierul a fost introdus corect si el\n");
	printf("exista in sistem, atributele sale au fost modificate.\n");
	printf("\nPuteti apasa orice tasta pentru a reveni in program.");
}

void fn440c5fh()
{
	char memadr[20];

	union REGS regs;
	struct SREGS sregs;

	printf("Pentru a testa functia 440c5fh (setarea modului de afisare)\n");
	printf("apasati orice tasta pentru a schimba modul de afisare la\n");
	printf("50x80 de caractere.");

	if (!getch())
		getch();

	// set the IoctlDisplayModeRec (see TechHelp for details).
	memadr[0] = 0;
	memadr[1] = 0;
	memadr[2] = 0;
	memadr[3] = 0xf;
	memadr[4] = 0;
	memadr[5] = 1;
	memadr[6] = 1;
	memadr[7] = 0;
	memadr[8] = 1;
	memadr[9] = 0;
	memadr[10] = 0;
	memadr[11] = 0;
	memadr[12] = 0;
	memadr[13] = 0;
	memadr[14] = 5;
	memadr[15] = 0;
	memadr[16] = 3;
	memadr[17] = 2;

	regs.x.ax = 0x440c;
	regs.x.bx = 0x0001;
	regs.x.cx = 0x035f;
	regs.x.dx = FP_OFF(&memadr);
	sregs.ds = FP_SEG(&memadr);

	int86x(0x21, &regs, &regs, &sregs);

	if (regs.x.cflag)
	{
		printf("\n\nA avut loc o eroare la executia functiei. Codul de eroare \n");
		printf("a fost %x (valoare in hexazecimal).", regs.x.ax);
	}
	else
	{
		printf("\n\nSi acum apasati orice tasta pentru a-l schimba la loc.");

		memadr[14] = 5;
		memadr[15] = 0;
		memadr[16] = 1;
		memadr[17] = 9;

		regs.x.ax = 0x440c;
		regs.x.bx = 0x0001;
		regs.x.cx = 0x035f;
		regs.x.dx = FP_OFF(&memadr);
		sregs.ds = FP_SEG(&memadr);

		int86x(0x21, &regs, &regs, &sregs);

		if (!getch())
			getch();

		if (regs.x.cflag)
		{
			printf("\n\nA avut loc o eroare la executia functiei. Codul de eroare \n");
			printf("a fost %x (valoare in hexazecimal).", regs.x.ax);
		}
	}

	printf("\n\nPuteti apasa orice tasta pentru a reveni in program.");
}

void fn440c7fh()
{
	char memadr[20];
	union REGS regs;
	struct SREGS sregs;

	printf("Pentru a testa functia 440c7fh (aflarea modului de afisare)\n");
	printf("apasati orice tasta pentru a afla modul curent de afisare.");

	if (!getch())
		getch();

	regs.x.ax = 0x440c;
	regs.x.bx = 0x0001;
	regs.x.cx = 0x037f;
	regs.x.dx = FP_OFF(&memadr);
	sregs.ds = FP_SEG(&memadr);

	int86x(0x21, &regs, &regs, &sregs);

	if (regs.x.cflag)
	{
		printf("\n\nA avut loc o eroare la executia functiei. Codul de eroare \n");
		printf("a fost %x (valoare in hexazecimal).", regs.x.ax);
	}
	else
	{
		printf("\n\nModul curent de afisare este de %dx%d.",
			memadr[16] * 16 + memadr[17], memadr[14] * 16 + memadr[15]);
	}

	printf("\n\nPuteti apasa orice tasta pentru a reveni in program.");
}

void fn19h()
{
	union REGS regs;

	printf("Functia 19h intoarce drive-ul curent implicit. Pentru a rula\n");
	printf("functia apasati orice tasta.");

	if (!getch())
		getch();

	regs.h.ah = 0x19;

	int86(0x21, &regs, &regs);

	printf("\n\nDrive-ul dvs. implicit este... %c.", regs.h.al + 65);
	printf("\n\nPuteti apasa orice tasta pentru a reveni in program.");
}

void fn0eh()
{
	union REGS regs;
	char c;

	printf("Functia 0eh seteaza drive-ul DOS implicit. Introduceti va\n");
	printf("rog litera drive-ului care doriti sa devina implicit.");
	printf("\nLitera (A-Z, a-z): ");
	do
	{
		if (!(c = getch()))
			c = getch();
	}
	while (!(((c >= 'A') && (c <= 'Z')) || ((c >= 'a') && (c <= 'z'))));
	if ((c >= 'a') && (c <= 'z'))
		c = c - ('a' - 'A');
	printf("%c", c);

	printf("\nValoarea introdusa de dvs. a fost scrisa in registrul dl\n");
	printf("dupa cum se cere si s-a apelat functia 0eh. In mod normal,\n");
	printf("daca drive-ul a fost unul valid, el trebuie sa fi fost setat\n");
	printf("ca drive implicit. Puteti verifica acest lucru ruland exemplul\n");
	printf("de la functia 19h.");

	regs.h.ah = 0x0e;
	regs.h.dl = c - 'A';

	int86(0x21, &regs, &regs);

	printf("\n\nPuteti apasa orice tasta pentru a reveni in program.");
}

void fn47h()
{
	union REGS regs;
	struct SREGS sregs;
	char memadr[66];
	char c;

	printf("Functia 47h intoarce directorul implicit pentru un drive speci-\n");
	printf("ficat. Introduceti va rog litera drive-ului al carui director\n");
	printf("implicit doriti sa-l aflati.");
	printf("\nLitera (A-Z, a-z): ");
	do
	{
		if (!(c = getch()))
			c = getch();
	}
	while (!(((c >= 'A') && (c <= 'Z')) || ((c >= 'a') && (c <= 'z'))));
	if ((c >= 'a') && (c <= 'z'))
		c = c - ('a' - 'A');
	printf("%c", c);

	printf("\nValoarea introdusa de dvs. a fost scrisa in registrul dl\n");
	printf("dupa cum se cere si s-a apelat functia 47h. Rezultatul ei a\n");
	printf("fost...\n\n");

	regs.h.ah = 0x47;
	regs.h.dl = c - 'A' + 1;
	regs.x.si = FP_OFF(&memadr);
	sregs.ds = FP_SEG(&memadr);

	int86x(0x21, &regs, &regs, &sregs);

	if (regs.x.cflag)
	{
		printf("A avut loc o eroare la executia functiei. Probabil drive-ul\n");
		printf("specificat a fost unul eronat.");
	}
	else
	{
		printf("Directorul curent: %c:\\%s", c, memadr);
	}

	printf("\n\nPuteti apasa orice tasta pentru a reveni in program.");
}

void fn3bh()
{
	union REGS regs;
	struct SREGS sregs;
	char memadr[66];

	printf("Functia 3bh seteaza directorul implicit pentru un drive.\n");
	printf("Introduceti va rog directorul implicit pe care doriti sa-l\n");
	printf("setati. Daca il veti precede si cu un identificator de drive\n");
	printf("(ex. d:) el va fi setat pentru drive-ul specificat. Altfel,\n");
	printf("va fi setat pentru drive-ul implicit. Atentie! Nu introduceti\n");
	printf("caracterul '\' la sfarsit, altfel executia functiei va esua.\n");
	printf("Directorul:\n");

	readString(memadr, 64, '\0');

	printf("\nSirul dat de dvs. a fost scris in memorie la o adresa\n");
	printf("oarecare, iar offsetul adresei respective a fost scris\n");
	printf("in registrul dx si segmentul ei in registrul ds (dupa\n");
	printf("cum este necesar). Acum s-a apelat functia 3bh. Puteti\n");
	printf("verifica setarea directorului ruland exemplul pentru\n");
	printf("functia 47h (daca nu primiti in continuare un mesaj de\n");
	printf("eroare.");

	regs.h.ah = 0x3b;
	regs.x.dx = FP_OFF(&memadr);
	sregs.ds = FP_SEG(&memadr);

	int86x(0x21, &regs, &regs, &sregs);

	if (regs.x.cflag)
	{
		printf("\n\nA avut loc o eroare la executia functiei. Probabil\n");
		printf("drive-ul sau directorul specificat a fost unul eronat.");
	}

	printf("\n\nPuteti apasa orice tasta pentru a reveni in program.");
}

void fn36h()
{
	union REGS regs;
	char c;
	unsigned long sp;

	printf("Functia 36h intoarce spatiul total si disponibli de pe un\n");
	printf("drive. Introduceti va rog litera drive-ului despre care doriti\n");
	printf("sa aflati informatii.");
	printf("\nLitera (A-Z, a-z): ");
	do
	{
		if (!(c = getch()))
			c = getch();
	}
	while (!(((c >= 'A') && (c <= 'Z')) || ((c >= 'a') && (c <= 'z'))));
	if ((c >= 'a') && (c <= 'z'))
		c = c - ('a' - 'A');
	printf("%c", c);

	printf("\nValoarea introdusa de dvs. a fost scrisa in registrul dl\n");
	printf("dupa cum se cere si s-a apelat functia 36h. Rezultatul:\n\n");

	regs.h.ah = 0x36;
	regs.h.dl = c - 'A' + 1;

	int86(0x21, &regs, &regs);

	sp = regs.x.ax * regs.x.cx;
	sp *= regs.x.bx;
	printf("Spatiul disponibil: %lu octeti.\n", sp);
	sp = regs.x.ax * regs.x.cx;
	sp *= regs.x.dx;
	printf("Spatiul total disponibil: %lu octeti.", sp);

	printf("\n\nPuteti apasa orice tasta pentru a reveni in program.");
}

void fn3305h()
{
	union REGS regs;

	printf("Functia 3305h intoarce drive-ul de pe care a fost incarcat\n");
	printf("sistemul. Pentru a rula functia apasati orice tasta.");

	if (!getch())
		getch();

	regs.x.ax = 0x3305;

	int86(0x21, &regs, &regs);

	printf("\n\nDrive-ul in cauza este... %c.", regs.h.dl + 64);
	printf("\n\nPuteti apasa orice tasta pentru a reveni in program.");
}

void fn3dh()
{
	char memadr[66];

	union REGS regs;
	struct SREGS sregs;

	printf("Pentru a testa functia 3dh (deschiderea unui fisier) intro-\n");
	printf("duceti va rog numele fisierului de deschis, precedat sau nu\n");
	printf("de o cale. Daca nu va fi precedat de o cale, se va cauta in\n");
	printf("directorul curent. Daca calea nu exista, fisierul nu va\n");
	printf("fi deschis. Din numele fisierului se vor considera maxim 8\n");
	printf("caractere plus inca 3 pentru extensie. Introduceti numele\n");
	printf("fisierului (maxim 64 de caractere):\n");

	readString(memadr, 64, '\0');

	printf("\nSirul dat de dvs. a fost scris in memorie la o adresa\n");
	printf("oarecare, iar offsetul adresei respective a fost scris\n");
	printf("in registrul dx si segmentul ei in registrul ds (dupa\n");
	printf("cum este necesar). Iar acum se apeleaza functia 3dh.\n\n");

	regs.h.ah = 0x3d;
	regs.h.al = 0;
	regs.x.dx = FP_OFF(&memadr);
	sregs.ds = FP_SEG(&memadr);
	int86x(0x21, &regs, &regs, &sregs);

	if (regs.x.cflag)
	{
		printf("A aparut o eroare in executie. Verificati existenta\n");
		printf("fisierului si drepturile de acces catre acesta.");
	}
	else
	{
		printf("Pentru a demonstra deschiderea cu succes a fisierului,\n");
		printf("iata primii 50 octeti din acesta:\n\n");

		regs.x.bx = regs.x.ax;
		regs.h.ah = 0x3f;					// read from file
		regs.x.cx = 50;
		regs.x.dx = FP_OFF(&memadr);
		sregs.ds = FP_SEG(&memadr);
		int86x(0x21, &regs, &regs, &sregs);

		if (regs.x.cflag)
		{
			printf("A aparut o eroare la citirea din fisier.");
		}
		else
		{
			memadr[regs.x.ax] = '\0';
			printf("%s", memadr);
		}

		regs.h.ah = 0x3e;					// close file
		regs.x.bx = regs.x.bx;
		int86x(0x21, &regs, &regs, &sregs);
	}

	printf("\n\nPuteti apasa orice tasta pentru a reveni in program.");
}

void fn3fh()
{
	char memadr[257];

	union REGS regs;
	struct SREGS sregs;

	printf("Pentru a testa functia 3fh (citirea din fisier) intro-\n");
	printf("duceti va rog numele fisierului din care doriti sa cititi,\n");
	printf("precedat sau nu de o cale. Daca nu va fi precedat de o cale,\n");
	printf("se va cauta in directorul curent. Daca calea nu exista,\n");
	printf("fisierul nu va fi deschis pentru citire. Din numele fisieru-\n");
	printf("lui se vor considera maxim 8 caractere plus inca 3 pentru\n");
	printf("extensie. Introduceti numele fisierului (maxim 64 de caractere):\n");

	readString(memadr, 64, '\0');

	printf("\nIntroduceti acum numarul de octeti pe care doriti sa-i cititi\n");
	printf("(2 cifre in hexazecimal, intre 00h si ffh).\n");
	printf("Numarul de octeti: ");

	char c1 = readHexValue();
	char c2 = readHexValue();
	printf("\n\n");

	regs.h.ah = 0x3d;
	regs.h.al = 0;
	regs.x.dx = FP_OFF(&memadr);
	sregs.ds = FP_SEG(&memadr);
	int86x(0x21, &regs, &regs, &sregs);

	if (regs.x.cflag)
	{
		printf("A aparut o eroare la deschiderea fisierului. Verificati\n");
		printf("existenta fisierului si drepturile de acces la acesta.");
	}
	else
	{
		printf("Octetii cititi:\n\n");

		regs.x.bx = regs.x.ax;
		regs.h.ah = 0x3f;					// read from file
		regs.x.cx = hexToDec(c1) * 16 + hexToDec(c2);
		regs.x.dx = FP_OFF(&memadr);
		sregs.ds = FP_SEG(&memadr);
		int86x(0x21, &regs, &regs, &sregs);

		if (regs.x.cflag)
		{
			printf("A aparut o eroare la citirea din fisier.");
		}
		else
		{
			memadr[regs.x.ax] = '\0';
			printf("%s", memadr);
		}

		regs.h.ah = 0x3e;					// close file
		regs.x.bx = regs.x.bx;
		int86x(0x21, &regs, &regs, &sregs);
	}

	printf("\n\nPuteti apasa orice tasta pentru a reveni in program.");
}

void fn40h()
{
	char memadr[257];

	union REGS regs;
	struct SREGS sregs;

	printf("Pentru a testa functia 40h (scriere in fisier) intro-\n");
	printf("duceti va rog numele fisierului in care doriti sa scrieti,\n");
	printf("precedat sau nu de o cale. Daca nu va fi precedat de o cale,\n");
	printf("se va cauta in directorul curent. Daca calea nu exista,\n");
	printf("fisierul nu va fi deschis pentru scriere. Din numele fisieru-\n");
	printf("lui se vor considera maxim 8 caractere plus inca 3 pentru\n");
	printf("extensie. Introduceti numele fisierului (maxim 64 de caractere):\n");

	readString(memadr, 64, '\0');

	printf("\nIntroduceti acum numarul de octeti pe care doriti sa-i scrieti\n");
	printf("(2 cifre in hexazecimal, intre 00h si ffh).\n");
	printf("Numarul de octeti: ");

	char c1 = readHexValue();
	char c2 = readHexValue();
	printf("\n\n");

	regs.h.ah = 0x3d;
	regs.h.al = 1;
	regs.x.dx = FP_OFF(&memadr);
	sregs.ds = FP_SEG(&memadr);
	int86x(0x21, &regs, &regs, &sregs);

	if (regs.x.cflag)
	{
		printf("A aparut o eroare la deschiderea fisieruui. Verificati\n");
		printf("existenta fisierului si drepturile de acces la acesta.");
	}
	else
	{
		printf("Acum introduceti pana la %d caractere.", hexToDec(c1) * 16 +
			hexToDec(c2));
		printf("\n");

		readString(memadr, hexToDec(c1) * 16 + hexToDec(c2), '\0');

		printf("\nSirul dat de dvs. a fost scris in memorie la o adresa\n");
		printf("oarecare, iar offsetul adresei respective a fost scris\n");
		printf("in registrul dx si segmentul ei in registrul ds (dupa\n");
		printf("cum este necesar). Iar acum se apeleaza functia 40h.\n\n");

		regs.x.bx = regs.x.ax;
		regs.h.ah = 0x40;					// write to file
		regs.x.cx = hexToDec(c1) * 16 + hexToDec(c2);
		regs.x.dx = FP_OFF(&memadr);
		sregs.ds = FP_SEG(&memadr);
		int86x(0x21, &regs, &regs, &sregs);

		if (regs.x.cflag)
		{
			printf("A aparut o eroare la scriere in fisier.");
		}
		else
		{
			printf("Datele au fost scrise in fisier.");
		}

		regs.h.ah = 0x3e;					// close file
		regs.x.bx = regs.x.bx;
		int86x(0x21, &regs, &regs, &sregs);
	}

	printf("\n\nPuteti apasa orice tasta pentru a reveni in program.");
}

void fn30h()
{
	union REGS regs;

	printf("Functia 30h intoarce versiunea de DOS folosita de dvs. Pentru a\n");
	printf("rula functia apasati orice tasta.");

	if (!getch())
		getch();

	regs.h.ah = 0x30;

	int86(0x21, &regs, &regs);

	printf("\n\nVersiunea de DOS folosita de dvs. este... %d.%d.",
		regs.h.al, regs.h.ah);
	printf("\n\nPuteti apasa orice tasta pentru a reveni in program.");
}

void fn2ah()
{
	union REGS regs;

	printf("Functia 2ah intoarce data curenta a sistemului dvs. Pentru a\n");
	printf("rula functia apasati orice tasta.");

	if (!getch())
		getch();

	regs.h.ah = 0x2a;

	int86(0x21, &regs, &regs);

	printf("\n\nData sistemului dvs. este... %d.%d.%d.",
		regs.h.dl, regs.h.dh, regs.x.cx);
	printf("\n\nPuteti apasa orice tasta pentru a reveni in program.");
}

void fn2ch()
{
	union REGS regs;

	printf("Functia 2ch intoarce ora curenta a sistemului dvs. Pentru a\n");
	printf("rula functia apasati orice tasta.");

	if (!getch())
		getch();

	printf("\n\nOra sistemului dvs. este... ");
	int x = wherex();
	int y = wherey();
	_setcursortype(_NOCURSOR);
	printf("\n\nPuteti apasa orice tasta pentru a reveni in program.");
	do
	{
		gotoxy(x, y);
		regs.h.ah = 0x2c;
		int86(0x21, &regs, &regs);
		printf("%d:%d:%d.%d.", regs.h.ch, regs.h.cl, regs.h.dh, regs.h.dl);
	}
	while (!kbhit());
	_setcursortype(_NORMALCURSOR);
}

void fn35h()
{
	union REGS regs;
	struct SREGS sregs;

	printf("Functia 35h intoarce adresa vectorului de tratare a unei intre-\n");
	printf("ruperi. Introduceti va rog numarul intreuperii al carei vector\n");
	printf("de intrerupere doriti sa-l aflati (2 cifre in hexazecimal).\n");
	printf("Intreruperea (intre 00h si ffh): ");
	char c1 = readHexValue();
	char c2 = readHexValue();

	regs.h.ah = 0x35;
	regs.h.al = 16 * hexToDec(c1) + hexToDec(c2);

	int86x(0x21, &regs, &regs, &sregs);

	printf("\n\nAdresa vectorului de intrerupere %c%c... %x:%x.",
		c1, c2, sregs.es, regs.x.bx);
	printf("\n\nPuteti apasa orice tasta pentru a reveni in program.");
}
