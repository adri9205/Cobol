       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. REPORTESPROG.
       AUTHOR.
           PROG.ADRIANACORTES
       DATE-WRITTEN.
           27/MAY/2015
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.
           IBM-370.
           OBJECT-COMPUTER.
           IBM-370.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *-----------------------
       FILE-CONTROL.
        SELECT ARCH-AI-ARCHIVO-I ASSIGN W000-UT-S-DIRECCION-LECTURA
                                 ORGANIZATION IS LINE SEQUENTIAL.
        SELECT REPO-R1-REPORTE-O ASSIGN  W000-UT-S-DIRECCION-SALIDA.
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------
       FD ARCH-AI-ARCHIVO-I
           LABEL RECORD STANDARD.
      *     RECORD CONTAINS 46 CHARACTERS.
       01 RI-REPORTE.
           05 RI-GPO                     PIC XX.
           05 RI-EMP                     PIC XXX.
           05 RI-PTA                     PIC XXX.
           05 RI-BCO                     PIC XXX.
           05 RI-NO-CTA                  PIC X(8).
           05 RI-NO-CHEQUE               PIC X(12).
           05 RI-VALOR-CHEQUE            PIC S9(12)V99.

       FD REPO-R1-REPORTE-O
           RECORD CONTAINS 90 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01 R1-REPORTE-SALIDA               PIC X(90).

       WORKING-STORAGE SECTION.
      *-----------------------
      **SWITCHES
       01 S000-ESTADOS.
           05 S000-FINARCH                PIC 9.
      *ESTADO DEL ARCHIVO
           05 S000-ESTADOARCH             PIC X.
           05 S000-ESTADOREPO             PIC X.


       01 W000-CTES.
           05 W000-PROG                   PIC X(8)       VALUE "CHE120".
           05 W000-UT-S-DIRECCION-LECTURA PIC X(30)      VALUE
       "ReporteEntrada.dat".
           05 W000-UT-S-DIRECCION-SALIDA  PIC X(30)      VALUE
       "REPORTESALIDA1.TXT".

      *FECHA
       01 W000-FECHA.
           05 W000-YEAR                   PIC 99.
           05 W000-MONTH                   PIC 99.
           05 W000-DAY                     PIC 99.
      *IDENTIDAD DE PROCEDIMIENTO
       01 I010-ID-PROC.
           05 I010-ID-PROC-NO-CTA.
               10 I010-ID-PROC-BCO.
                   15 I010-ID-PROC-PTA.
                    20 I010-GPO           PIC XX.
                    20 I010-EMP           PIC XXX.
                    20 I010-PTA           PIC XXX.
                   15 I010-BCO            PIC XXX.
               10 I010-NO-CTA              PIC X(8).

      *IDENTIDADES ANTERIOR Y LEI
       01 I040-AI-ID-ANT                  PIC X(31).

       01 I040-AI-ID-LEI.
           05 I040-AI-ID-NO-CTA.
               10 I040-AI-ID-PTA.
                   15 I040-AI-GPO         PIC XX.
                   15 I040-AI-EMP         PIC XXX.
                   15 I040-AI-PTA         PIC XXX.
               10 I040-AI-BCO             PIC XXX.
               10 I040-AI-NO-CTA          PIC X(8).
           05 I040-AI-NO-CHEQUE           PIC X(12).

      * ACUMULADORES
       01 A010-ACUM-PTA.
           05 A010-ACUM-CHEQUES-PTA       PIC S9(16)V99 .
       01 A020-ACUM-NO-CTA.
           05 A020-ACUM-CHEQUES-NO-CTA    PIC S9(12)V99 .


      * CIFRAS DE CONTROL
       01 A990-CIFRAS-CONTROL.
           05 A990-PROC-LEIDO             PIC S9(9) COMP.
           05 A990-REGS-PROC              PIC S9(9) COMP.
           05 A990-PTA-PROC               PIC S9(9) COMP.
           05 A990-NO-CTA-PROC            PIC S9(9) COMP.

      *TABLA
       01  T000-MONTHS-TABLE              PIC X(36)      VALUE
       'ENEFEBMARABRMAYJUNJULAGOSEPOCTNOVDIC'.
       01  T000-MONTH-TABLE          REDEFINES T000-MONTHS-TABLE.
           05  T000-MONTH             OCCURS 12
                                          PIC XXX.
       01  T000-VARS.
           05  T000-I                     PIC 99.
           05  T000-NUM-ELEM              PIC S9(9)      COMP.
           05  T000-MAX-ELEM              PIC S9(9)      COMP.

      *VARIABLES QUE GUARDAN DATOS
       01 R1-VARS.
           05 R1-050-NUM-HOJA             PIC S9(9)       VALUE 0.
           05 R1-050-NUM-LIN              PIC S9(9).
           05 R1-050-MAX-LIN              PIC S9(9)       VALUE +50.

      *LINEAS DE TITULO

       01 R1-05-ENCABEZADOS-PRIMERA-LINEA.
           05 FILLER                      PIC X           VALUE SPACES.
           05 R1-05-CONSPRO               PIC X(8).
           05 FILLER                      PIC X(18)       VALUE SPACES.
           05 FILLER                      PIC X(27)       VALUE
           "REPORTE DE CHEQUES COBRADOS".
           05 FILLER                      PIC X(18)       VALUE SPACES..
           05 FILLER                      PIC X(7)        VALUE
           "FECHA: ".
           05 R1-05-FECHA.
               10 R1-05-DAY               PIC X(2).
               10 FILLER                  PIC X           VALUE "/".
               10 R1-05-MONTH             PIC X(3).
               10 FILLER                  PIC X           VALUE "/".
               10 R1-05-YEAR              PIC X(2).

       01 R1-10-ENCABEZADOS-SEGUNDA-LINEA.
           05 FILLER                      PIC X(24)       VALUE
           "ITS-DIVISION DESARROLLO".
           05 FILLER                      PIC X(9).
           05 FILLER                      PIC X(22)       VALUE
           "MOVIMIENTOS EFECTUADOS".
           05 FILLER                      PIC X(26)       VALUE SPACES.
           05 FILLER                      PIC X(5)        VALUE "HOJA ".
           05 R1-10-PAGNU                 PIC Z(4).

       01 R1-15-ENCABEZADOS-TERCERA-LINEA.
           05 FILLER                      PIC X           VALUE SPACES.
           05 FILLER                      PIC X(12)       VALUE
           "CONTABILIDAD".
           05 FILLER                      PIC X(77)       VALUE SPACES.
       01 R1-20-ENCABEZADOS-CUARTA-LINEA.
           05 FILLER                      PIC X(4)        VALUE "GPO".
           05 FILLER                      PIC X(3)        VALUE SPACE.
           05 FILLER                      PIC X(3)        VALUE "EMP".
           05 FILLER                      PIC X(3)        VALUE SPACE.
           05 FILLER                      PIC X(3)        VALUE "PTA".
           05 FILLER                      PIC X(3)        VALUE SPACE.
           05 FILLER                      PIC X(3)        VALUE "BCO".
           05 FILLER                      PIC X(3)        VALUE SPACE.
           05 FILLER                      PIC X(8)        VALUE "NOCTA".
           05 FILLER                      PIC X(3)        VALUE SPACE.
           05 FILLER                      PIC X(12)       VALUE
           "NOCHEQUE".
           05 FILLER                      PIC X(3)        VALUE SPACE.
           05 FILLER                      PIC X(18)       VALUE
           "VALORCHEQUE".
           05 FILLER                      PIC X(27)       VALUE SPACES.

      * IMPRIME EL DETALLE QUE HAY EN EL ARHIVO
       01 R1-25-LINEADETALLE.
           05 FILLER                      PIC X(2)        VALUE SPACES.
           05 R1-25-PRINT-GPO             PIC X(2).
           05 FILLER                      PIC X(3)        VALUE SPACES.
           05 R1-25-PRINT-EMP             PIC X(3).
           05 FILLER                      PIC X(3)        VALUE SPACES.
           05 R1-25-PRINT-PTA             PIC X(3).
           05 FILLER                      PIC X(3)        VALUE SPACES.
           05 R1-25-PRINT-BCO             PIC X(3).
           05 FILLER                      PIC X(3)        VALUE SPACES.
           05 R1-25-PRINT-NO-CTA          PIC X(8).
           05 FILLER                      PIC X(3)        VALUE SPACES.
           05 R1-25-PRINT-NO-CHEQUE       PIC X(12).
           05 FILLER                      PIC X(3)        VALUE SPACES.
           05 R1-25-PRINT-VALOR-CHEQUE    PIC ZZZ,ZZZ,ZZZ,ZZZ.99.
           05 FILLER                      PIC X(28)       VALUE SPACES.

      *IMPRIME LA LINEA TOTAL POR NUMERO DE CUENTA
       01  R1-30-TOTAL-NO-CTA.
           05  FILLER                     PIC X(6)        VALUE SPACES.
           05  FILLER                     PIC X(22)       VALUE
           "T O T A L  C U E N T A".
           05  FILLER                     PIC X(19)       VALUE SPACES.
           05  R1-30-TOTAL-CUENTA         PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZZ.99.
           05  FILLER                     PIC X(24)       VALUE SPACES.

       01  R1-35-TOTAL-PTA.
           05  FILLER                     PIC X(6)        VALUE SPACES.
           05  FILLER                     PIC X(22)       VALUE
           "T O T A L  P L A N T A".
           05  FILLER                     PIC X(17)       VALUE SPACES.
           05  R1-35-TOTAL-PLANTA         PIC Z,ZZZ,ZZZ,ZZZ,ZZZ,ZZZ.99.
           05  FILLER                     PIC X(24)       VALUE SPACES.
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       000-CONTROL SECTION.
       000-0100-INICIO.
           OPEN INPUT ARCH-AI-ARCHIVO-I.
           MOVE 0 TO S000-ESTADOARCH.
           MOVE 0 TO S000-FINARCH.
           OPEN OUTPUT REPO-R1-REPORTE-O.
           MOVE 0 TO S000-ESTADOREPO.
           MOVE 0 TO R1-050-NUM-HOJA.
           MOVE W000-PROG TO R1-05-CONSPRO.
           MOVE FUNCTION CURRENT-DATE(3:6) TO W000-FECHA.
           MOVE W000-DAY TO R1-05-DAY.
           MOVE W000-YEAR TO R1-05-YEAR.
           SET  T000-I TO W000-MONTH.
           MOVE T000-MONTH(T000-I) TO R1-05-MONTH.
           MOVE ZEROS TO A990-CIFRAS-CONTROL.
           MOVE LOW-VALUES TO I040-AI-ID-LEI.
           PERFORM 040-LEE-REG.
       000-0200-PROC-PTA.
           PERFORM 010-PROC-PTA UNTIL S000-FINARCH =1.
       000-0300-TERMINA.
           CLOSE ARCH-AI-ARCHIVO-I.
           MOVE 1 TO S000-ESTADOARCH.
           CLOSE REPO-R1-REPORTE-O.
           MOVE 1 TO S000-ESTADOREPO.
           PERFORM 990-CIFRAS-DE-CONTROL.
       000-0990-FIN.
           GOBACK.
       010-PROC-PTA SECTION.
       010-0100-INICIO.
           MOVE I040-AI-ID-PTA TO I010-ID-PROC-PTA.
           MOVE R1-050-MAX-LIN TO R1-050-NUM-LIN.
           MOVE 0 TO A010-ACUM-CHEQUES-PTA.
       010-0200-PROC-PTA.
           PERFORM 020-PROC-NO-CTA
               UNTIL I010-ID-PROC-PTA NOT = I040-AI-ID-PTA.
       010-0300-ESCRIBE-PTA.
           MOVE A010-ACUM-PTA TO R1-35-TOTAL-PLANTA.
           IF (R1-050-NUM-LIN + 1)>R1-050-MAX-LIN THEN
               PERFORM 050-ENCABEZADOS
               END-IF.
*******               WRITE R1-REPORTESALIDA AFTER 2.
      *        ADD 2 TO R1-050-NUMLIN.

           WRITE R1-REPORTE-SALIDA FROM R1-35-TOTAL-PTA AFTER 1.
           ADD 1 TO R1-050-NUM-LIN.
       010-0400-ACT-CIFRAS-DE-CONTROL.
           ADD 1 TO A990-PTA-PROC.
       010-0990-TERMINA.
       EXIT.
       020-PROC-NO-CTA SECTION.
       020-0100-INICIO.
           MOVE I040-AI-ID-NO-CTA TO I010-ID-PROC-NO-CTA.
           MOVE 0 TO A020-ACUM-CHEQUES-NO-CTA.
       020-0200-PROC-NO-CTA.
           PERFORM  030-PROC-REG
               UNTIL I010-ID-PROC-NO-CTA NOT EQUAL TO I040-AI-ID-NO-CTA.
       020-0300-ESCRIBE-NO-CTA.
           MOVE A020-ACUM-NO-CTA TO R1-30-TOTAL-CUENTA.
           IF (R1-050-NUM-LIN + 2)>R1-050-MAX-LIN THEN
               PERFORM 050-ENCABEZADOS
               END-IF.
           WRITE R1-REPORTE-SALIDA FROM R1-30-TOTAL-NO-CTA AFTER 2.
           ADD 2 TO R1-050-NUM-LIN.


       020-0400-ESCRIBE-LINEAS.
           IF (R1-050-NUM-LIN + 2) > R1-050-MAX-LIN
           THEN
               PERFORM 050-ENCABEZADOS
               END-IF.
           WRITE R1-REPORTE-SALIDA FROM ' ' AFTER 2.
           ADD 2 TO R1-050-NUM-LIN.

       020-0500-ACTUALIZA-REGISTROS.
           ADD A020-ACUM-CHEQUES-NO-CTA TO A010-ACUM-CHEQUES-PTA.
           ADD 1 TO A990-NO-CTA-PROC.
       020-0990-TERMINA.
           EXIT.

       030-PROC-REG SECTION.
       030-0100-INICIO.
      *MODULO QUE VA A SUMAR LO QUE TIENE VALOR POR CADA CUENTA.
      *MUEVE LOS VALORES DE LAS VARIABLES LEIDAS A LAS VARIABLES
      *QUE UTILIZARA PARA IMPRIMIR EL DETALLE

           MOVE I040-AI-GPO TO R1-25-PRINT-GPO.
           MOVE I040-AI-EMP TO R1-25-PRINT-EMP.
           MOVE I040-AI-PTA TO R1-25-PRINT-PTA.
           MOVE I040-AI-BCO TO R1-25-PRINT-BCO.
           MOVE I040-AI-NO-CTA TO R1-25-PRINT-NO-CTA.
      *     DISPLAY R1-030-PRINTNOCTA
           MOVE I040-AI-NO-CHEQUE TO R1-25-PRINT-NO-CHEQUE.
           MOVE RI-VALOR-CHEQUE TO R1-25-PRINT-VALOR-CHEQUE.
      *     DISPLAY R1-030-PRINTVALORCHEQUE
       030-0200-ESCRIBE-REG.
           IF (R1-050-NUM-LIN + 1)>R1-050-MAX-LIN THEN
               PERFORM 050-ENCABEZADOS
               END-IF.
               
           MOVE R1-25-LINEADETALLE TO R1-REPORTE-SALIDA.
           WRITE R1-REPORTE-SALIDA BEFORE ADVANCING 1.
           ADD RI-VALOR-CHEQUE TO A020-ACUM-CHEQUES-NO-CTA.
           PERFORM 040-LEE-REG.
           ADD 1 TO R1-050-NUM-LIN.
       030-0300-ACT-CIFRAS-DE-CONTROL.
           ADD 1 TO A990-REGS-PROC.

       030-0990-TERMINA.
           EXIT.
           
       040-LEE-REG SECTION.
       040-0100-INICIO.
           MOVE I040-AI-ID-LEI TO I040-AI-ID-ANT.
       040-0200-LEE.
           READ ARCH-AI-ARCHIVO-I AT END MOVE 1 TO S000-FINARCH.

           IF S000-FINARCH = 1 THEN
               MOVE HIGH-VALUES TO I040-AI-ID-LEI
               ELSE
           MOVE RI-GPO TO I040-AI-GPO
           MOVE RI-EMP TO I040-AI-EMP
           MOVE RI-PTA TO I040-AI-PTA
           MOVE RI-BCO TO I040-AI-BCO
           MOVE RI-NO-CTA TO I040-AI-NO-CTA
           MOVE RI-NO-CHEQUE TO I040-AI-NO-CHEQUE
           ADD 1 TO A990-PROC-LEIDO.



       040-0400-ABORTA.
           IF I040-AI-ID-ANT > I040-AI-ID-LEI THEN
          PERFORM 980-ABORTA.
       040-0990-TERMINA.
           EXIT.
           
       050-ENCABEZADOS SECTION.
       050-0100-INICIO.
           ADD 1 TO R1-050-NUM-HOJA.

           MOVE R1-050-NUM-HOJA TO R1-10-PAGNU.
       050-0200-ESCRIBE.
           WRITE R1-REPORTE-SALIDA FROM R1-05-ENCABEZADOS-PRIMERA-LINEA
           AFTER PAGE.
           WRITE R1-REPORTE-SALIDA FROM R1-10-ENCABEZADOS-SEGUNDA-LINEA
           AFTER 2.
           WRITE R1-REPORTE-SALIDA FROM R1-15-ENCABEZADOS-TERCERA-LINEA
           AFTER 1.
           WRITE R1-REPORTE-SALIDA FROM R1-20-ENCABEZADOS-CUARTA-LINEA
           AFTER 2.
           ADD 6 TO R1-050-NUM-LIN.

       050-0990-TERMINA.
           EXIT.
       980-ABORTA SECTION.
       980-0100-INICIO.
      ****************CONDICION DE SI EL ARCHIVO ESTA CERRADO***********
           IF S000-ESTADOARCH=1 THEN
           CLOSE ARCH-AI-ARCHIVO-I.
           MOVE 0 TO S000-ESTADOARCH.
          
      *    *********CONDICION DE SI EL ARCHIVO REPO ESTA CERRADO***********
           IF S000-ESTADOREPO=1 THEN
           CLOSE REPO-R1-REPORTE-O.
           MOVE 0 TO S000-ESTADOREPO.
          
      *    **************LLAMADA AL MODULO CIFRAS DE CONTROL***********
       980-0200-ACT-CIFRAS-DE-CONTROL.
           PERFORM 990-CIFRAS-DE-CONTROL.
      *    **************MENSAJE DE TERMINACION FALLIDA***********
           DISPLAY "TERMINACIÓN ANORMAL".
       980-0990-TERMINA.
           EXIT.
       990-CIFRAS-DE-CONTROL SECTION.
       990-0100-INICIO.
       DISPLAY W000-PROG '  REG.S LEIDOS                           '
      -    '           ' A990-PROC-LEIDO   .

       DISPLAY W000-PROG '  REG.S PROCESADOS                       '
      -    '            ' A990-REGS-PROC.

       DISPLAY W000-PROG '  PLANTAS PROCESADAS                     '
      -    '           ' A990-PTA-PROC.

       DISPLAY W000-PROG '  CUENTAS PROCESADAS                     '
      -    '           ' A990-NO-CTA-PROC.
       990-0990-TERMINA.
           EXIT.
