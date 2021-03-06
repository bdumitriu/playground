PROGRAM FOLOSIT_PENTRU_A_INVATA_UNITATEA_GRAPH;
USES CRT, GRAPH, UTILG;
VAR I:INTEGER;
    PALETA:PALETTETYPE;

PROCEDURE CULORI;
{TYPE PALETTETYPE=RECORD
                 SIZE:BYTE;
                 COLORS:ARRAY[0..15] OF SHORTINT;
                 END;}
BEGIN
 FOR I:=1 TO 16 DO
  BEGIN
   SETFILLSTYLE(1,I);
   BAR(0,I*30-30,200,I*30);
  END;
END;

PROCEDURE SCHIMBAREA_PALETEI_DE_CULORI;
BEGIN
 FOR I:=0 TO 15 DO SETPALETTE(I,I);
 FOR I:=0 TO 15 DO SETRGBPALETTE(I,0,0,4*I);
END;

PROCEDURE DREPTUNGHI;
BEGIN
 SETFILLSTYLE(1,15);
 BAR(100,100,500,400);
END;

PROCEDURE CERC;
BEGIN
 CIRCLE(50,50,20);
 ARC(100,100,0,270,20);
 ELLIPSE(150,150,0,270,10,30);
END;

PROCEDURE STATISTICA_MAGAZIN;
{SE CONSIDERA O FIRMA CU N MAGAZINE.SE RAPORTEAZA VANZARILE EFECTUATE IN CELE
N MAGAZINE.SE REPREZINTA PROCENTUAL FATA DE FIRMA ACESTE REALIZARI}
VAR I,N,S,U_I,U_F:INTEGER;
    REALIZARI:ARRAY[1..15] OF INTEGER;
BEGIN
 CLOSEGRAPH;
 WRITE('N=');READLN(N);
 S:=0;
 FOR I:=1 TO N DO
  BEGIN
   WRITE('REALIZARILE MAGAZINULUI ',I,' ');
   READLN(REALIZARI[I]);
   S:=S+REALIZARI[I];
  END;
 U_I:=0;U_F:=0;
 INI;
 FOR I:=1 TO N DO
  BEGIN
   IF I=N THEN U_F:=360
          ELSE U_F:=U_F+ROUND(REALIZARI[I]/S*360);
   SETFILLSTYLE(SOLIDFILL,I);
   PIESLICE(GETMAXX DIV 2, GETMAXY DIV 2, U_I, U_F, 100);
   U_I:=U_F;
  END;
END;

PROCEDURE TEXT;
BEGIN
 SETCOLOR(YELLOW);
 SETTEXTSTYLE(TRIPLEXFONT,HORIZDIR,10);
 SETTEXTJUSTIFY(CENTERTEXT,BOTTOMTEXT);
 OUTTEXTXY(GETMAXY DIV 2, GETMAXX DIV 2, 'text');
 SETTEXTSTYLE(TRIPLEXFONT,VERTDIR,10);
 OUTTEXTXY(GETMAXX DIV 2, GETMAXY DIV 2, 'text');
END;

PROCEDURE DESEN(X,Y,L:INTEGER);
BEGIN
 MOVETO(X,Y);
 LINETO(X+L,Y);
 LINETO(X+L,Y+L);
 LINETO(X,Y+L);
 LINETO(X,Y);
END;

PROCEDURE ANIMATIE1;
BEGIN
 SETWRITEMODE(1);  SETBKCOLOR(MAGENTA);SETCOLOR(RED);
 FOR I:=1 TO 100 DO
  BEGIN
   DESEN(I,10,30);
   DELAY(50);
   DESEN(I,5,30);
  END;
END;

PROCEDURE VISUAL_ACTIVE_PAGE;
VAR D,M:INTEGER;
BEGIN
 D:=VGA;
 M:=VGALO;
 INITGRAPH(D,M,'C:\TP\BGI');
 SETVISUALPAGE(0);
 MOVETO(1,1);
 LINETO(GETMAXY,GETMAXY);
 READKEY;
 SETACTIVEPAGE(1);
 MOVETO(1,1);
 LINETO(300,300);
 READKEY;
 SETVISUALPAGE(1);
 READKEY;
END;

BEGIN
 {INI;}
 {CULORI;
 READKEY;}

 {SCHIMBAREA_PALETEI_DE_CULORI;
 FOR I:=0 TO 15 DO
  BEGIN
   SETFILLSTYLE(1,I);
   BAR(0,I*30-30,200,I*30);
  END;
 READKEY;}

 {SCHIMBAREA_PALETEI_DE_CULORI;
 DREPTUNGHI;
 GETPALETTE(PALETA);
 FOR I:=14 DOWNTO 0 DO
  BEGIN
   DELAY(150);
   SETPALETTE(15,PALETA.COLORS[I]);
  END;
 READKEY;}

 {CERC;
 READKEY;}

 {STATISTICA_MAGAZIN;
 READKEY;}

 {TEXT;
 READKEY;}

 {ANIMATIE1;
 READKEY;}

 VISUAL_ACTIVE_PAGE;
END.
