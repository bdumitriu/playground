/* Inlocuirea si afisarea elementelor unei matrici folosind functii ce au
ca parametrii diferiti pointeri. */
#include <stdio.h>
#include <conio.h>

m1(float (*t)[3], float t31, float t32, float t33);
m2(float (*t)[3], float t11, float t22, float t33);

main()
{
float t[3][3] = {{1,0,0},{0,1,0},{0,0,1}};
float r[1][3] = {5,4,2};
float t11, t22, t31, t32, t33, aux;
int i, j, k;
clrscr();
printf("\n");
for (i=1;i<=3;i++)
	{
	printf("Introduceti elementul ce va fi pe pozitia 3,%i : ",i);
	if (i==1) scanf("%f",&t31);
	else if (i==2) scanf("%f",&t32);
	     else scanf("%f",&t33);
	}
m1(t,t31,t32,t33);
printf("\n");
printf("Matricea T dupa adaugarea elementelor 3,1 3,2 si 3,3 este : \n");
for (i=0;i<3;i++)
	{
	printf("\n");
	for (j=0;j<3;j++)
		printf("%7.2f",t[i][j]);
	}
printf("\n");
printf("\n");
printf("Matricea R dupa inmultirea cu noua matrice T este : \n");
i=0;
for (j=0;j<3;j++)
	{
	aux=0;
	for (k=0;k<3;k++)
		aux+=r[i][k]*t[k][j];
	r[i][j]=aux;
	}
printf("\n");
i=0;
for (j=0;j<3;j++)
	printf("%7.2f",r[i][j]);
printf("\n");
printf("\n");
for (i=1;i<=3;i++)
	{
	printf("Introduceti elementul ce va fi pe pozitia %i,%i : ",i,i);
	if (i==1) scanf("%f",&t11);
	else if (i==2) scanf("%f",&t22);
	     else scanf("%f",&t33);
	}
printf("\n");
m2(t,t11,t22,t33);
printf("Matricea T dupa adaugarea elementelor de pe diagonala principala : ");
printf("\n");
for (i=0;i<3;i++)
	{
	printf("\n");
	for (j=0;j<3;j++)
		printf("%7.2f",t[i][j]);
	}
printf("\n");
printf("\n");
printf("Matricea R dupa inmultirea cu noua matrice T este : \n");
printf("\n");
i=0;
for (j=0;j<3;j++)
	{
	aux=0;
	for (k=0;k<3;k++)
		aux+=r[i][k]*t[k][j];
	r[i][j]=aux;
	}
i=0;
for (j=0;j<3;j++)
	printf("%7.2f",r[i][j]);
getch();
return 0;
}

m1(float (*t)[3], float t31, float t32, float t33)
{
t[2][0] = t31;
t[2][1] = t32;
t[2][2] = t33;
return 0;
}

m2(float (*t)[3], float t11, float t22, float t33)
{
t[0][0] = t11;
t[1][1] = t22;
t[2][2] = t33;
return 0;
}