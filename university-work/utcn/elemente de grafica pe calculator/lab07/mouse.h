#ifndef __MOUSE_H_
#define __MOUSE_H_

typedef struct
{
	int x, y;
} POINT;

/*
 * Intoarce starea curenta a dispozitivului mouse. Starea este:
 *		0 - daca mouse-ul nu exista sau nu este instalat.
 *		diferita de 0 - daca mouse-ul este instalat.
 * Functia initializeaza driverul mouse. Argumentul de tip intreg
 * va contine numarul de butoane ale mouse-ului.
 */
int mouse_reset(int *nr_of_buttons);

/*
 * Afiseaza cursorul pe ecran.
 */
void mouse_on();

/*
 * Ascunde cursorul de pe ecran.
 */
void mouse_off();

/*
 * Intoarce starea butoanelor stanga si dreapta ca un intreg,
 * totodata stocand in variabila position coordonatele curente
 * ale cursorului. Bitii intregului intors au semnificatia:
 *		bitul 0 - reprezinta butonul din stanga.
 *		bitul 1 - reprezinta butonul din dreapta.
 * Bitii au valorile:
 *		1 - pentru buton apasat.
 * 		0 - pentru buton eliberat.
 */
int mouse_status(POINT *position);

/*
 * Pune programul in asteptare pana cand utilizatorul apasa un buton
 * mouse specificat ca argument. Parametrul button este numarul butonului
 * (1 - stanga, 2 - dreapta), iar position va contine pozitia cursorului
 * in care s-a actionat butonul specificat.
 */
void mouse_wait_for_press(int button, POINT *position);

/*
 * Pune programul in asteptare pana cand utilizatorul elibereaza un buton
 * mouse specificat ca argument. Parametrul button este numarul butonului
 * (1 - stanga, 2 - dreapta), iar position va contine pozitia cursorului
 * in care s-a actionat butonul specificat.
 */
void mouse_wait_for_release(int button, POINT *position);

/*
 * Specifica modelul grafic de 16x16 biti care defineste forma cursorului
 * mouse de 16x16 pixeli. Modelul grafic picture contine 8x2 intregi
 * (16 cuvinte x 16 biti) pentru masca ecran si 8x2 intregi (16 cuvinte x
 * 16 biti) pentru masca cursor).
 *
 * Punctul de referinta al cursorului este cel care va indica punctul
 * actual de pe ecran. Punctul de coordonate (0,0) este coltul din stanga
 * sus al dreptunghiului de 16x16 pixeli care da forma cursorului.
 * Coordonatele punctului de referinta pot sa varieze in intervalul [-16,16]
 * (negative => pc. de referinta este in exteriorul dreptunghiului).
 * Rezultatul aplicarii mastii ecran si a mastii cursor:
 *
 * +-----------------------------------------------------------------------+
 * |bitul din masca ecran | bitul din masca cursor | culoare punct pe ecran|
 * +-----------------------------------------------------------------------+
 *	0                       0                        negru
 * 	0                       1                        alb
 *  1                       0                        culoarea originala
 *  1                       1                        inversa cul. originale
 */
void mouse_set_cursor(int picture[16][2]);

/*
 * Specifica domeniul coordonatei orizontale in care se
 * poate deplasa cursorul.
 */
void mouse_set_horizontal_pos(int minpos, int maxpos);

/*
 * Specifica domeniul coordonatei verticale in care se
 * poate deplasa cursorul.
 */
void mouse_set_vertical_pos(int minpos, int maxpos);

/*
 * Restrange deplasarea cursorului la dreptunghiul cu coltul stanga
 * sus om (minx,miny) si cu coltul dreapta jos in (maxx,maxy).
 */
void mouse_restrict(int minx, int miny, int maxx, int maxy);

/*
 * Elibereaza cursorul, permitandu-se deplasarea intre (0,0) si
 * (maxx, maxy).
 */
void mouse_free(int maxx, int maxy);

/*
 * Pozitioneaza cursorul la pozitia specificata.
 */
void mouse_set_position(POINT *position);

#endif