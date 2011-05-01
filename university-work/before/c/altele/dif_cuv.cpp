#include <stdio.h>
#include <conio.h>
#include <string.h>
#include <stdlib.h>

int min(int value1, int value2)
{
   return ( (value1 < value2) ? value1 : value2);
}

int max(int value1, int value2)
{
   return ( (value1 > value2) ? value1 : value2);
}

main()
{
char str1[30], str2[30];
int x[30];
int min_a, max_a, i, j, k, dif;
clrscr();
str1[0]='\0';
str2[0]='\0';
for (i=0;i<30;i++)
	x[i]=0;
printf("\n");
printf(" Introduceti primul cuvant : ");
scanf("%s",str1);
printf(" Introduceti al doilea cuvant : ");
scanf("%s",str2);
min_a=min(strlen(str1),strlen(str2));
//max_a=max(strlen(str1),strlen(str2));
for (i=0;i<min_a;i++)
	if (str1[i]==str2[i]) x[i]=2;
/*j=0;
for (i=0;i<30;i++)
	{
	if (x[i]==2) y[j]=1;
	j++;
	}*/
for (i=0;i<strlen(str1);i++)
	if (x[i]!=2)
		for (j=0;j<strlen(str2);j++)
			if ((x[j]!=2) && (str1[i]==str2[j]))
				x[i]=1;
k=strlen(str1);
dif=0;
for (i=0;i<k;i++)
	if (x[i]==0)
		{
		for (j=i;j<k;j++)
				{
				str1[j]=str1[j+1];
				x[j]=x[j+1];
				dif++;
				}
		k=strlen(str1);
		}
for (i=0;i<min_a;i++)
	if (str1[i]==str2[i]) x[i]=2;
getch();
return 0;
}