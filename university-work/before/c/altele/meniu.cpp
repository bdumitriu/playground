#include <stdio.h>
#include <iostream.h>
#include <conio.h>
#include <string.h>

FILE *f;

main()
{
	char *dim, *ami, *sea, *intre, *dat;

	clrscr();
	cout << "\n";
	cout << " Data este: ";
	cin >> dat;
	cout << " Ia spune tu baietas, ce-ai mancat dimineata?\n  ";
	do
	{
		cin >> dim;
	}
	while (dim[strlen(dim)] != '\n');
	cout << " Acum sa trecem la amiaza:\n  ";
	cin >> ami;
	cout << " Si in sfarsit am ajuns si la seara...\n  ";
	cin >> sea;
	cout << " Si daca-ai mancat ceva si-ntre mese, nu uita sa mentionezi!\n  ";
	cin >> intre;
	if ((f = fopen(/*"e:\\every\\all\\meniu.txt"*/"c:\\meniu.txt", "a")) == NULL)
	{
		printf(" Eroare la deschiderea fisierului!");
		getch();
		return 0;
	}
	fprintf(f, " %s\n  Dimineata: %s\n  Amiaza: %s\n  Seara: %s\n  Intre mese: %s", dat, dim, ami, sea, intre);
	fclose(f);

	getch();
}