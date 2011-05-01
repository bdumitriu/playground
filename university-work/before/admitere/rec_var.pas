uses crt;
type studii = (fara, medii, sup);
     om = record
             nume: string[30];
             case st: studii of
                fara: ();
                medii: (bac: real);
                sup: (ba: real; fac: real);
           end;