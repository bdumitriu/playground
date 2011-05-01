/* Interschimabarea a doua siruri cu ajutorul unui sir de pointeri*/
#include <stdio.h>
#include <conio.h>

main()
{
int a[2], b[2], i, aux;
int *c[2];
clrscr();
printf("\n");
for (i=0;i<2;i++)
	{
	printf("Elementul %i al sirului a : ",i+1);
	scanf("%i",&a[i]);
	}
for (i=0;i<2;i++)
	{
	printf("Elementul %i al sirului b : ",i+1);
	scanf("%i",&b[i]);
	}
c[0]=a;
c[1]=b;
for (i=0;i<2;i++)
	{
	aux=*(c[0]+i);
	*(c[0]+i)=*(c[1]+i);
	*(c[1]+i)=aux;
	}
printf("\n");
printf("Dupa interschimbarea celor doua siruri ele vor arata in felul urm. :");
printf("\n");
printf(" sirul a : ");
for (i=0;i<2;i++)
	printf("%i ",*(c[0]+i));
printf("\n");
printf(" sirul b : ");
for (i=0;i<2;i++)
	printf("%i ",*(c[1]+i));
getch();
return 0;
}