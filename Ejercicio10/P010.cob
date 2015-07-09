       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. REPORTE-COMPARATIVO.
       AUTHOR.
           PROG.ADRIANACORTES
       DATE-WRITTEN.
           01/JULY/2015
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SOURCE-COMPUTER.
           IBM-370.
       OBJECT-COMPUTER.
           IBM-370.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *-----------------------
      *-----------------------
      *Entrada
      *----
           SELECT ARCH-AC-ARCHIVO-CONSUMO ASSIGN W000-UT-S-DIR-CONSUMO
                                           ORGANIZATION IS LINE
                                           SEQUENTIAL.

           SELECT ARCH-AD-ARCHIVO-DEVOLUCION ASSIGN
                                           W000-UT-S-DIR-DEVOLUCION
                                           ORGANIZATION IS LINE
                                           SEQUENTIAL.

           SELECT ARCH-AT-ARCHIVO-TABLA ASSIGN W000-UT-S-DIR-TABLA
                                           ORGANIZATION IS LINE
                                           SEQUENTIAL.

           SELECT ARCH-AP-ARCHIVO-PRODUCTO ASSIGN W000-UT-S-DIR-PRODUCTO
                                           ORGANIZATION IS LINE
                                           SEQUENTIAL.
           SELECT ARCH-AF-ARCHIVO-FECHA    ASSIGN W000-UT-S-DIR-FECHA
                                           ORGANIZATION IS LINE
                                           SEQUENTIAL.
      *----
      *Salida
      *----
           SELECT REPO-R1-REPORTE ASSIGN TO
           W000-UT-S-DIR-SALIDA-REPO.
      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
       FD  ARCH-AC-ARCHIVO-CONSUMO
           LABEL RECORD STANDARD.
       01 AI-ARCHIVO-CONSUMO.
           05 RI-C-CVE                   PIC XX.
           05 RI-C-PTA                   PIC XXX.
           05 RI-C-DPTO                  PIC X(6).
           05 RI-C-CODIGO                PIC X(6).
           05 RI-C-CANT-CONSUMO          PIC 9(8).

       FD  ARCH-AD-ARCHIVO-DEVOLUCION
           LABEL RECORD STANDARD.
       01 AI-ARCHIVO-DEVOLUCION.
           05 RI-D-PTA                   PIC XXX.
           05 RI-D-DPTO                  PIC X(6).
           05 RI-D-CODIGO                PIC X(6).
           05 RI-D-CANT-DEVUELTA         PIC 9(8).

       FD  ARCH-AT-ARCHIVO-TABLA
           LABEL RECORD STANDARD.
       01 AI-ARCHIVO-TABLA.
           05 RI-T-CVE                   PIC XXX.
           05 RI-T-PTA                   PIC XXX.
           05 RI-T-DPTO                  PIC X(6).
           05 RI-T-ESPACIOS              PIC X(11).
           05 RI-T-DESCRIPCION           PIC X(30).

       FD  ARCH-AP-ARCHIVO-PRODUCTO
           LABEL RECORD STANDARD.
       01 AI-ARCHIVO-PRODUCTO.
           05 RI-P-CODIGO                PIC X(6).
           05 RI-P-DESCRIPCION           PIC X(30).
           05 RI-P-CTO                   PIC 9(3).

       FD  ARCH-AF-ARCHIVO-FECHA
           LABEL RECORD STANDARD.
       01 AI-ARCHIVO-FECHA.
          05 RI-F-CVE-TARJ               PIC XXX.
          05 RI-F-ACCESO                 PIC XXX.
          05 RI-F-COMP-YEAR              PIC 99.
          05 RI-F-YEAR                   PIC 99.
          05 RI-F-MONTH                  PIC 99.
          05 RI-F-DAY                    PIC 99.

       FD  REPO-R1-REPORTE
           RECORD CONTAINS 120 CHARACTERS
           LABEL RECORD STANDARD.
       01  R1-COMPARACION                PIC X(120).

       WORKING-STORAGE SECTION.
      *---
      *Switches
      *---
       01  S000-ESTADOS.
           05  S000-ESTADOARCHCONSUMO    PIC X.
           05  S000-FINARCHCONSUMO       PIC X.
           05  S000-ESTADOARCHDEVOLUCION PIC X.
           05  S000-FINARCHDEVOLUCION    PIC X.
           05  S000-ESTADOARCHTABLA      PIC X  VALUE '0'.
           05  S000-FINARCHTABLA         PIC X.
           05  S000-ESTADOARCHPRODUCTO   PIC X  VALUE '0'.
           05  S000-FINARCHPRODUCTO      PIC X.
           05  S000-FINARCHFECHA         PIC X.
           05  S000-ESTADOARCHFECHA      PIC X.
           05  S000-ESTADOREPO           PIC X.


      *---
      *Constantes
      *---
       01  W000-CTES.
           05  W000-PROG                 PIC X(8)
                                         VALUE 'P-COS720'.
           05  W000-UT-S-DIR-CONSUMO     PIC X(20)
                                         VALUE 'ArchivoConsumo.dat'.
           05  W000-UT-S-DIR-DEVOLUCION  PIC X(25)
                                         VALUE 'ArchivoDevolucion.dat'.
           05  W000-UT-S-DIR-TABLA       PIC X(20)
                                         VALUE 'ArchivoTabla.dat'.
           05  W000-UT-S-DIR-PRODUCTO     PIC X(20)
                                         VALUE 'ArchivoProducto.dat'.
           05  W000-UT-S-DIR-FECHA        PIC X(10)
                                         VALUE 'Fecha.dat'.
           05  W000-UT-S-DIR-SALIDA-REPO PIC X(30)
                                         VALUE 'ReporteComparacion.txt'.
      *---
      *   Fecha
      *---
       01 W000-FECHA.
           05 W000-FIRST-TWO-DIGIT-YEAR   PIC 99.
           05 W000-YEAR                   PIC 99.
           05 W000-MONTH                  PIC 99.
           05 W000-DAY                    PIC 99.
      *---
      *Costo por unidad
      *---
       01 W050-COSTO                      PIC 999.
      *---
      *   Identidades
      *---
       01 I040-ID-PROC-CONSUMO.
           05 I040-ID-PROC-CODIGO-CONS.
               10 I040-ID-PROC-DPTO-CONS.
                   15 I040-ID-PROC-PTA-CONS.
                    20 I040-PTA           PIC XXX.
                   15 I040-DPTO           PIC X(6).
               10 I040-CODIGO             PIC X(6).

       01 I070-AC-ID-ANT                  PIC X(15).
       01 I070-AC-ID-LEI-CONSUMO.
           05 I070-AC-ID-CODIGO-CONS.
               10 I070-AC-ID-DPTO-CONS.
                   15 I070-AC-ID-PTA-CONS.
                    20 I070-PTA           PIC XXX.
                   15 I070-DPTO           PIC X(6).
               10 I070-CODIGO             PIC X(6).

       01 I080-AT-ID-ANT                  PIC X(12).
       01 I080-AT-ID-LEI-TABLA.
           05 I080-AT-ID-LLAVE.
               10 I080-CLAVE              PIC XXX.
               10 I080-AT-ID-DPTO.
                 15 I080-PTA              PIC XXX.
                 15 I080-DPTO             PIC X(6).

       01 I100-AP-ID-ANT                  PIC X(6).
       01 I100-AP-ID-LEI-PRODUCTO.
           05 I100-AP-ID-CODIGO.
               10 I100-CODIGO             PIC X(6).

       01 I110-AD-ID-ANT                  PIC X(15).
       01 I110-AD-ID-LEI-DEVOLUCION.
           05 I110-AD-ID-CODIGO-DEV.
               10 I110-AD-ID-DPTO-DEV.
                   15 I110-AD-ID-PTA-DEV.
                    20 I110-PTA           PIC XXX.
                   15 I110-DPTO           PIC X(6).
               10 I110-CODIGO             PIC X(6).

      *---
      *   Acumuladores
      *---
       01  A050-ACUMULADOR-RA.
           05  A050-ACUM-A-FAVOR-RA       PIC S9(8)V99.
           05  A050-ACUM-TOTAL-IMPORTE-RA PIC S9(8)V99.

       01  A060-ACUMULADOR-RA-REG.
           05  A060-ACUM-REG-RA-IMPORTE   PIC S9(8)V99.
           05  A060-ACUM-REG-RA-CONSUMO   PIC S9(8).

       01  A050-ACUMULADOR-RP.
           05  A050-ACUM-A-FAVOR-RP       PIC S9(8)V99.
           05  A050-ACUM-TOTAL-IMPORTE-RP PIC S9(8)V99.

       01  A060-ACUMULADOR-RP-REG.
           05  A060-ACUM-REG-RP-IMPORTE   PIC S9(8)V99.
           05  A060-ACUM-REG-RP-CONSUMO   PIC S9(8).

       01  A060-ACUMULADOR-DIFERENCIA.
           05  A060-ACUM-DIF-IMPORTE      PIC 9(8)V99.
           05  A060-ACUM-DIF-CONSUMO      PIC 9(8).

       01  A060-ACUMULADOR-DEVUELTO.
           05 A060-ACUM-DEV               PIC S9(8).
      *---
      *Cifras control
      *---
       01 A990-CIFRAS-CONTROL.
           05 A990-PROC-LEIDO-CONSUMO     PIC S9(4).
           05 A990-PROC-LEIDO-DEVOLUCION  PIC S9(4).
           05 A990-PROC-LEIDO-TABLA       PIC S9(4).
           05 A990-PROC-LEIDO-PRODUCTO    PIC S9(4).
           05 A990-PROC-PTA               PIC S9(4).
           05 A990-PROC-DPTO              PIC S9(4).
           05 A990-PROC-REG               PIC S9(4).
           05 A990-PROC-A-FAVOR-RA        PIC S9(4).
           05 A990-PROC-A-FAVOR-RP        PIC S9(4).
           05 A990-PROC-EMPATE            PIC S9(4).
      *---
      * Tabla
      *---
       01  T010-MONTHS-TABLE              PIC X(36)      VALUE
       'ENEFEBMARABRMAYJUNJULAGOSEPOCTNOVDIC'.
       01  T010-MONTH-TABLE          REDEFINES T010-MONTHS-TABLE.
           05  T010-MONTH             OCCURS 12
                                          PIC XXX.
       01  T010-VARS.
           05  T010-I                     PIC 99.
           05  T010-NUM-ELEM              PIC S9(9)      COMP.
           05  T010-MAX-ELEM              PIC S9(9)      COMP.
       01 T020-TABLA-ARCH-T04.
           05 T020-DETALLES-T04      OCCURS 30 TIMES INDEXED
           BY T020-I-T04-I.
               10 T020-PTA-T04            PIC XXX.
               10 T020-DESCRIPCION-T04    PIC X(30).
       01  T020-VARS-T04.
           05  T020-I-T04                 PIC 99.
           05  T020-NUM-ELEM-T04          PIC S9(2).
           05  T020-MAX-ELEM-T04          PIC S9(2) VALUE 30.

       01 T020-TABLA-ARCH-T05.
           05 T020-DETALLES-T05      OCCURS 30 TIMES INDEXED
           BY T020-I-T05-I.
               10 T020-CLAVE.
                   15 T020-PTA-T05        PIC XXX.
                   15 T020-DPTO-T05       PIC X(9).
               10 T020-DESCRIPCION-T05    PIC X(30).
       01  T020-VARS-T05.
           05  T020-I-T05                 PIC 99.
           05  T020-NUM-ELEM-T05          PIC S9(2).
           05  T020-MAX-ELEM-T05          PIC S9(2) VALUE 30.
       01 T030-TABLA-PRODUCTOS.
           05 T030-DETALLES-PRODUCTOS OCCURS 30 TIMES INDEXED
           BY T030-I-PRODUCTO.
               10 T030-CODIGO             PIC X(6).
               10 T030-DESCRIPCION        PIC X(30).
               10 T030-CTO                PIC 9(3).
       01  T030-VARS-PRODUCTO.
           05  T030-I-PR                  PIC 99.
           05  T030-NUM-ELEM-PR           PIC S9(2).
           05  T030-MAX-ELEM-PR           PIC S9(2) VALUE 30.
      *---
      *Variables del reporte
      *---
       01 R1-VARS.
           05 R1-NUM-HOJA                 PIC S9(9).
           05 R1-NUM-LIN                  PIC S9(9).
           05 R1-MAX-LIN                  PIC S9(9)       VALUE 200.
      *---
      *Lineas de titulo
      *---
       01 R1-05-ENCABEZADOS-PRIMERA-LINEA.
           05 FILLER                      PIC X           VALUE SPACES.
           05 R1-05-CONSPRO               PIC X(8).
           05 FILLER                      PIC X(2)       VALUE SPACES.
           05 FILLER                      PIC X(60)       VALUE
          "R E P O R T E C O M P A R A T I V O   D E   C O N S U M O S".
           05 FILLER                      PIC X(3)       VALUE SPACES..
           05 FILLER                      PIC X(7)        VALUE
           "FECHA: ".
           05 R1-05-FECHA.
               10 R1-05-DAY               PIC X(2).
               10 FILLER                  PIC X           VALUE SPACES.
               10 R1-05-MONTH             PIC X(3).
               10 FILLER                  PIC X           VALUE SPACES.
               10 R1-05-YEAR              PIC X(2).
               
       01 R1-10-ENCABEZADOS-SEGUNDA-LINEA.
           05 FILLER                      PIC X(24)       VALUE
           "DIV. PAQUETES".
           05 FILLER                      PIC X(10).
           05 FILLER                      PIC X(15)       VALUE
           "P L A N T A".
           05 FILLER                      PIC X(5)        VALUE SPACES.
           05 R1-10-DESCRIPCION-PTA       PIC X(30).
           05 FILLER                      PIC X(22).
           05 FILLER                      PIC X(5)        VALUE "HOJA ".
           05 R1-10-PAGNU                 PIC Z(4).

       01 R1-15-ENCABEZADOS-TERCERA-LINEA.
           05 FILLER                      PIC X           VALUE SPACES.
           05 FILLER                      PIC X(12)       VALUE
           "CONTABILIDAD".
           05 FILLER                      PIC X(77)       VALUE SPACES.

       01 R1-20-ENCABEZADOS-CUARTA-LINEA.
           05 FILLER                      PIC X           VALUE SPACES.
           05 FILLER                      PIC X(5)       VALUE
           "DPTO".
           05 FILLER                      PIC X(5).
           05 R1-20-DPTO                  PIC X(6).
           05 FILLER                      PIC XX         VALUE SPACES.
           05 R1-20-DESCRIPCION-DPTO-ENCA PIC X(30).


       01 R1-25-ENCABEZADOS-QUINTA-LINEA.
           05 FILLER                      PIC X(15)       VALUE
           "P R O D U C T O".
           05 FILLER                      PIC X(30)       VALUE SPACE.
           05 FILLER                      PIC X(15)       VALUE
           "REPORTO ALMACEN".
           05 FILLER                      PIC X(5)       VALUE SPACE.
           05 FILLER                      PIC X(18)       VALUE
           "REPORTO PRODUCCION".
           05 FILLER                      PIC X(5)       VALUE SPACE.
           05 FILLER                      PIC X(20)       VALUE
           "DIF. ENTRE REPORTES".

       01 R1-30-ENCABEZADOS-SEXTA-LINEA.
           05 FILLER                      PIC X(6)        VALUE
           "CODIGO".
           05 FILLER                      PIC X(2)        VALUE SPACES.
           05 FILLER                      PIC X(15)       VALUE
           "DESCRIPCION".
           05 FILLER                      PIC X(20)       VALUE SPACE.
           05 FILLER                      PIC X(7)        VALUE
           "CONSUMO".
           05 FILLER                      PIC X(2)       VALUE SPACES.
           05 FILLER                      PIC X(8)       VALUE
           "IMPORTE".
           05 FILLER                      PIC X(2)       VALUE SPACES.
           05 FILLER                      PIC X(7)        VALUE
           "CONSUMO".
           05 FILLER                      PIC X(2)       VALUE SPACES.
           05 FILLER                      PIC X(8)       VALUE
           "IMPORTE".
           05 FILLER                      PIC X(10)       VALUE SPACES.
           05 FILLER                      PIC X(7)        VALUE
           "CONSUMO".
           05 FILLER                      PIC X(3)       VALUE SPACES.
           05 FILLER                      PIC X(8)       VALUE
           "IMPORTE".
           05 FILLER                      PIC X(2)       VALUE SPACES.
           05 FILLER                      PIC X(10)       VALUE
           "A FAVOR DE".

      *---
      *Linea detalle
      *---
       01 R1-35-LINEADETALLE.
           05 R1-35-PRINT-CODIGO          PIC X(6).
           05 FILLER                      PIC X(1)        VALUE SPACES.
           05 R1-35-PRINT-DESCRIPCION     PIC X(30).
           05 FILLER                      PIC X(1)        VALUE SPACES.
           05 R1-35-PRINT-COMSUMO-RA      PIC +ZZ,ZZZ,ZZ9.
           05 FILLER                      PIC X(1)        VALUE SPACES.
           05 R1-35-PRINT-IMPORTE-RA      PIC +ZZ,ZZZ,ZZ9.
           05 FILLER                      PIC X(1)        VALUE SPACES.
           05 R1-35-PRINT-COMSUMO-RP      PIC +ZZ,ZZZ,ZZ9.
           05 FILLER                      PIC X(1)        VALUE SPACES.
           05 R1-35-PRINT-IMPORTE-RP      PIC +ZZ,ZZZ,ZZ9.
           05 FILLER                      PIC X(1)        VALUE SPACES.
           05 R1-35-PRINT-COMSUMO-DIF     PIC ZZ,ZZZ,ZZ9.
           05 FILLER                      PIC X(1)        VALUE SPACES.
           05 R1-35-PRINT-IMPORTE-DIF     PIC ZZ,ZZZ,ZZ9.
           05 FILLER                      PIC X(6)        VALUE SPACES.
           05 R1-35-PRINT-MENSAJE-A-FAVOR PIC X(10).

      *---
      *Linea total de Dpto
      *---
       01 R1-40-TOTAL-POR-DPTO.
           05 FILLER                      PIC X           VALUE SPACES.
           05 FILLER                      PIC X(15)       VALUE
           "TOTAL DEL DPTO".
           05 FILLER                      PIC X(5).
           05 R1-40-DPTO                  PIC X(6).
           05 FILLER                      PIC XXX         VALUE SPACES.
           05 R1-40-DESCRIPCION-DPTO      PIC X(30).
           05 FILLER                      PIC X(77)       VALUE SPACES.
      *---
      *Linea total de almacen
      *---
       01 R1-45-TOTAL-POR-RA.
           05 FILLER                      PIC X           VALUE SPACES.
           05 FILLER                      PIC X(10)       VALUE
           "ALMACEN".
           05 FILLER                      PIC X(5).
           05 FILLER                      PIC X(15)       VALUE
           "IMPORTE".
           05 FILLER                      PIC X(3)        VALUE SPACES.
           05 R1-45-PRINT-IMPORTE-RA      PIC ZZ,ZZZ,ZZ9.99.
           05 FILLER                      PIC X(3)        VALUE SPACES.
           05 FILLER                      PIC X(20)       VALUE
           "DIFERENCIA A FAVOR".
           05 FILLER                      PIC X(3)        VALUE SPACES.
           05 R1-45-PRINT-A-FAVOR-RA      PIC ZZ,ZZZ,ZZ9.99.
           05 FILLER                      PIC X(20)       VALUE SPACES.
      *---
      *Linea total de produccion
      *---

       01 R1-50-TOTAL-POR-RP.
           05 FILLER                      PIC X           VALUE SPACES.
           05 FILLER                      PIC X(10)       VALUE
           "PRODUCCION".
           05 FILLER                      PIC X(5).
           05 FILLER                      PIC X(15)       VALUE
           "IMPORTE".
           05 FILLER                      PIC X(3)        VALUE SPACES.
           05 R1-50-PRINT-IMPORTE-RP      PIC ZZ,ZZZ,ZZ9.99.
           05 FILLER                      PIC X(3)        VALUE SPACES.
           05 FILLER                      PIC X(20)       VALUE
           "DIFERENCIA A FAVOR".
           05 FILLER                      PIC X(3)        VALUE SPACES.
           05 R1-50-PRINT-A-FAVOR-RP      PIC ZZ,ZZZ,ZZ9.99.
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       000-CONTROL SECTION.
       000-0100-INICIO.
           OPEN INPUT ARCH-AC-ARCHIVO-CONSUMO.
           MOVE 0 TO S000-FINARCHCONSUMO.
           MOVE 1 TO S000-ESTADOARCHCONSUMO.

           OPEN INPUT ARCH-AD-ARCHIVO-DEVOLUCION.
           MOVE 0 TO S000-FINARCHDEVOLUCION.
           MOVE 1 TO S000-ESTADOARCHDEVOLUCION.

           OPEN OUTPUT REPO-R1-REPORTE.
           MOVE 1 TO S000-ESTADOREPO.

           MOVE ZEROS TO A990-CIFRAS-CONTROL.
           MOVE W000-PROG TO R1-05-CONSPRO.
       000-0200-REALIZA-FECHA.
           PERFORM 010-FECHA.
       000-0300-CARGA-TABLA-ARCH-TABLA.
           PERFORM 020-CARGA-TABLA-ARCH-TABLA.
       000-0400-CARGA-TABLA-ARCH-PROD.
           PERFORM 030-CARGA-TABLA-ARCH-PRODUCTO.
       000-0500-LEE-CONSUMO-DEVOLUCION.
           MOVE LOW-VALUES TO I070-AC-ID-LEI-CONSUMO.
           MOVE LOW-VALUES TO I110-AD-ID-LEI-DEVOLUCION.
           PERFORM 070-LEE-REG-CONSUMO.
           PERFORM 110-LEE-REG-DEVOLUCION.
       000-0600-PROCESA-PTA.

           PERFORM 040-PROC-PTA UNTIL (S000-FINARCHCONSUMO NOT = 0 AND
               S000-FINARCHDEVOLUCION NOT = 0).
       000-0700-CIFRAS-CONTROL.
           PERFORM 990-CIFRAS-CONTROL.
       000-0800-TERMINA.
            CLOSE ARCH-AC-ARCHIVO-CONSUMO.
            MOVE 0 TO S000-ESTADOARCHCONSUMO.
            CLOSE ARCH-AD-ARCHIVO-DEVOLUCION.
            MOVE 0 TO S000-ESTADOARCHDEVOLUCION.
            CLOSE REPO-R1-REPORTE.
            MOVE 0 TO S000-ESTADOREPO.
       000-FIN.
           GOBACK.


       010-FECHA SECTION.
       010-0100-INICIO.
           OPEN INPUT ARCH-AF-ARCHIVO-FECHA.
           MOVE 0 TO S000-FINARCHFECHA.
           MOVE 1 TO S000-ESTADOARCHFECHA.

           READ ARCH-AF-ARCHIVO-FECHA AT END MOVE 1
           TO S000-FINARCHFECHA.
           IF S000-FINARCHFECHA = 1 THEN
               MOVE FUNCTION CURRENT-DATE(1:8) TO W000-FECHA
           ELSE
           IF RI-F-ACCESO NOT = 'F01' THEN
               DISPLAY '          TERMINACION ANORMAL, CODIGO 16  '
               PERFORM 980-ABORTA
               END-IF.

       010-0200-VALIDA-FECHA.
           IF ((RI-F-COMP-YEAR>=0) AND (RI-F-MONTH>=1 AND
               RI-F-MONTH<=12) AND (RI-F-DAY>=1
               AND
               (RI-F-MONTH = 2 AND ((((FUNCTION REM(RI-F-YEAR,400)=0 OR
               (FUNCTION REM(RI-F-YEAR,4)=0 AND FUNCTION
               REM(RI-F-YEAR,100) NOT =0))
               AND
               RI-F-DAY<30) OR ((FUNCTION REM(RI-F-YEAR,400) NOT =0 OR
               (FUNCTION REM(RI-F-YEAR,4)NOT =0 AND FUNCTION
               REM(RI-F-YEAR,100) =0)) AND RI-F-DAY <29))))))
              THEN
               MOVE RI-F-YEAR TO W000-YEAR
               MOVE RI-F-MONTH TO W000-MONTH
               MOVE RI-F-DAY TO W000-DAY
               ELSE IF ((RI-F-COMP-YEAR>=0) AND (RI-F-MONTH>= 1 AND
                   RI-F-MONTH<=12) AND (RI-F-DAY>=1 AND
                   ((RI-F-MONTH = 04 OR RI-F-MONTH =06 OR RI-F-MONTH
                   =09 OR RI-F-MONTH = 11) AND
                 RI-F-DAY<=30 )))
                 MOVE RI-F-COMP-YEAR TO W000-YEAR
                 MOVE RI-F-MONTH TO W000-MONTH
                 MOVE RI-F-DAY TO W000-DAY
                 ELSE IF ((RI-F-COMP-YEAR>=0) AND (RI-F-MONTH>= 1 AND
                   RI-F-MONTH<=12) AND (RI-F-DAY>=1 AND
                   ((RI-F-MONTH = 01 OR RI-F-MONTH =03 OR RI-F-MONTH
                   =05 OR RI-F-MONTH = 07 OR RI-F-MONTH = 08
                   OR RI-F-MONTH = 10 OR RI-F-MONTH = 12) AND
                 RI-F-DAY<=31 )))
                 MOVE RI-F-COMP-YEAR TO W000-YEAR
                 MOVE RI-F-MONTH TO W000-MONTH
                 MOVE RI-F-DAY TO W000-DAY
                 ELSE
                   DISPLAY '          TERMINACION ANORMAL, CODIGO 16  '
                   DISPLAY '          FECHA INVALIDA                  '
                     PERFORM 980-ABORTA
                 END-IF.

            MOVE W000-DAY TO R1-05-DAY.
            MOVE W000-YEAR TO R1-05-YEAR.
            SET  T010-I TO W000-MONTH.
            MOVE T010-MONTH(T010-I) TO R1-05-MONTH.
            CLOSE ARCH-AF-ARCHIVO-FECHA.
            MOVE 0 TO S000-ESTADOARCHFECHA.
       010-0990-FIN.
           EXIT.
           
           
       020-CARGA-TABLA-ARCH-TABLA SECTION.
           OPEN INPUT ARCH-AT-ARCHIVO-TABLA.
           MOVE 0 TO S000-FINARCHTABLA.
           MOVE 1 TO S000-ESTADOARCHTABLA.
           MOVE 0 TO T020-NUM-ELEM-T04
                     T020-NUM-ELEM-T05.
           MOVE LOW-VALUES TO I080-AT-ID-LEI-TABLA.

       020-0200-CHECA-CLAVE.
           IF I080-CLAVE < 'T04' THEN
               PERFORM 080-LEE-REG-TABLA

           ELSE IF I080-CLAVE = 'T04' THEN
               PERFORM 020-0300-T04-LEE
               PERFORM 080-LEE-REG-TABLA


           ELSE IF I080-CLAVE = 'T05' THEN
               PERFORM 020-0400-T05-LEE
               PERFORM 080-LEE-REG-TABLA
               
           ELSE
               PERFORM 020-0600-FINAL
               END-IF.
       020-0300-T04-LEE.

               ADD 1 TO T020-NUM-ELEM-T04
               IF T020-NUM-ELEM-T04 > T020-MAX-ELEM-T04 THEN
                   PERFORM 980-ABORTA
                   ELSE
                   SET T020-I-T04 TO T020-NUM-ELEM-T04
                   MOVE I080-PTA TO T020-PTA-T04(T020-NUM-ELEM-T04)
                   MOVE RI-T-DESCRIPCION TO
                   T020-DESCRIPCION-T04(T020-NUM-ELEM-T04)
                   END-IF.

       020-0400-T05-LEE.
               ADD 1 TO T020-NUM-ELEM-T05
               IF T020-NUM-ELEM-T05 > T020-MAX-ELEM-T05 THEN
                   PERFORM 980-ABORTA
                   ELSE
                   SET T020-I-T05 TO T020-NUM-ELEM-T05
                   MOVE I080-PTA TO T020-PTA-T05(T020-I-T05)
                   MOVE I080-DPTO TO T020-DPTO-T05(T020-I-T05)
                   MOVE RI-T-DESCRIPCION TO
                   T020-DESCRIPCION-T05(T020-I-T05)
                   END-IF.
       020-0500-LEE-TABLA.

           PERFORM 020-0200-CHECA-CLAVE UNTIL S000-FINARCHTABLA = 1.
       020-0600-FINAL.
           CLOSE ARCH-AT-ARCHIVO-TABLA.
           MOVE 0 TO S000-ESTADOARCHTABLA.
      *    SET I-T05 TO 1.
      * SEARCH T000-DETALLES-T05
      *   AT END DISPLAY 'M NOT FOUND IN TABLE'
      *   WHEN T000-CLAVE(I-T05)='DDDSSWWAA'
      *   DISPLAY 'ENCONTRADO!'
      * END-SEARCH.

       020-990-FIN.
           EXIT.

       030-CARGA-TABLA-ARCH-PRODUCTO SECTION.
       030-0100-INICIO.
           OPEN INPUT ARCH-AP-ARCHIVO-PRODUCTO.
           MOVE 0 TO S000-FINARCHPRODUCTO.
           MOVE 1 TO S000-ESTADOARCHPRODUCTO.
           MOVE 0 TO T030-NUM-ELEM-PR.
           MOVE LOW-VALUES TO I100-AP-ID-LEI-PRODUCTO.

       030-0200-COPIA.

           ADD 1 TO T030-NUM-ELEM-PR
               IF T030-NUM-ELEM-PR > T030-MAX-ELEM-PR THEN
                   PERFORM 980-ABORTA
                   ELSE
                   SET T030-I-PR TO T030-NUM-ELEM-PR

                   MOVE I100-CODIGO TO T030-CODIGO(T030-NUM-ELEM-PR)
                   MOVE RI-P-CTO TO T030-CTO(T030-NUM-ELEM-PR)
                   MOVE RI-P-DESCRIPCION TO
                   T030-DESCRIPCION(T030-NUM-ELEM-PR)
                   
                   END-IF.
                   PERFORM 100-LEE-REG-PRODUCTO.

               
       030-0300-LEE-TABLA.

           PERFORM 030-0200-COPIA UNTIL S000-FINARCHPRODUCTO = 1.
       030-0400-TERMINA.
           CLOSE ARCH-AP-ARCHIVO-PRODUCTO.
           MOVE 0 TO S000-ESTADOARCHPRODUCTO.


       030-FIN.
           EXIT.

       040-PROC-PTA SECTION.
       040-0100-INICIO.
           IF I070-AC-ID-PTA-CONS < I110-AD-ID-PTA-DEV THEN
               MOVE I070-AC-ID-PTA-CONS TO I040-ID-PROC-PTA-CONS
               ELSE
                   MOVE I110-AD-ID-PTA-DEV TO I040-ID-PROC-PTA-CONS
                   END-IF.
       040-0200-BUSCA-EN-TABLA.
          SET T020-I-T04-I TO 1.
          SEARCH T020-DETALLES-T04
         AT END MOVE SPACES TO R1-10-DESCRIPCION-PTA
         WHEN T020-PTA-T04(T020-I-T04-I)=I040-ID-PROC-PTA-CONS
         MOVE T020-DESCRIPCION-T04(T020-I-T04-I) TO
         R1-10-DESCRIPCION-PTA
       END-SEARCH.
       040-0300-INICIA-FOLIO.
           MOVE 0 TO R1-NUM-HOJA.
       040-0400-PROCESA-DPTO.
           PERFORM 050-PROC-DPTO UNTIL ((I040-ID-PROC-PTA-CONS NOT =
               I070-AC-ID-PTA-CONS) AND (I040-ID-PROC-PTA-CONS NOT =
               I110-AD-ID-PTA-DEV)).
       040-0500-CIFRA-CONTROL.
           ADD 1 TO A990-PROC-PTA.
       040-FIN.
           EXIT.
       050-PROC-DPTO SECTION.
       050-0100-INICIO.
           IF I070-AC-ID-DPTO-CONS < I110-AD-ID-DPTO-DEV THEN
               MOVE I070-AC-ID-DPTO-CONS TO I040-ID-PROC-DPTO-CONS
               ELSE
                   MOVE I110-AD-ID-DPTO-DEV TO I040-ID-PROC-DPTO-CONS
                   END-IF.
                   MOVE I040-DPTO TO R1-40-DPTO.
                   MOVE I040-DPTO TO R1-20-DPTO.
       050-0200-BUSCA-EN-TABLA.
           
          SET T020-I-T05-I TO 1.
          SEARCH T020-DETALLES-T05
         AT END MOVE SPACES TO R1-20-DESCRIPCION-DPTO-ENCA
         MOVE SPACES TO R1-40-DESCRIPCION-DPTO
         WHEN T020-CLAVE(T020-I-T05-I)=I040-ID-PROC-DPTO-CONS
         MOVE T020-DESCRIPCION-T05(T020-I-T05-I) TO
         R1-20-DESCRIPCION-DPTO-ENCA
         MOVE T020-DESCRIPCION-T05(T020-I-T05-I) TO
         R1-40-DESCRIPCION-DPTO
       END-SEARCH.
       050-0300-INICIALIZA-ACUM.
           MOVE R1-MAX-LIN TO R1-NUM-LIN.
           MOVE ZEROES TO A050-ACUMULADOR-RA.
           MOVE ZEROES TO A050-ACUMULADOR-RP.
       050-0400-PROCESA-REGISTRO.
           PERFORM 060-PROC-REG UNTIL ((I040-ID-PROC-DPTO-CONS NOT =
               I070-AC-ID-DPTO-CONS) AND (I040-ID-PROC-DPTO-CONS NOT =
               I110-AD-ID-DPTO-DEV)).
       050-0500-ENCABEZADOS.
           IF (R1-NUM-LIN + 7) > R1-MAX-LIN THEN
               PERFORM 090-ENCABEZADOS
               ADD 7 TO R1-NUM-LIN
               END-IF.
       050-0600-ESCRIBE-TOTAL-POR-DPTO.

              MOVE A050-ACUM-TOTAL-IMPORTE-RA TO R1-45-PRINT-IMPORTE-RA.
              MOVE A050-ACUM-TOTAL-IMPORTE-RP TO R1-50-PRINT-IMPORTE-RP.
              MOVE A050-ACUM-A-FAVOR-RA TO R1-45-PRINT-A-FAVOR-RA.
              MOVE A050-ACUM-A-FAVOR-RP TO R1-50-PRINT-A-FAVOR-RP.
              WRITE R1-COMPARACION FROM R1-40-TOTAL-POR-DPTO AFTER 3.
              WRITE R1-COMPARACION FROM R1-45-TOTAL-POR-RA AFTER 2.
              WRITE R1-COMPARACION FROM R1-50-TOTAL-POR-RP AFTER 2.
              ADD 7 TO R1-NUM-LIN.
              ADD 1 TO A990-PROC-DPTO.

       050-FIN.
           EXIT.
       060-PROC-REG SECTION.
       060-0100-INICIO.
           MOVE ZEROES TO A060-ACUMULADOR-RA-REG.
           MOVE ZEROES TO A060-ACUMULADOR-RP-REG.
           MOVE ZEROES TO A060-ACUMULADOR-DEVUELTO.
           MOVE ZEROES TO A060-ACUMULADOR-DIFERENCIA.
       060-0200-DECIDE-PRODUCTO.
           IF I070-AC-ID-CODIGO-CONS < I110-AD-ID-CODIGO-DEV THEN
               MOVE I070-AC-ID-CODIGO-CONS TO I040-ID-PROC-CODIGO-CONS
               ELSE
                   MOVE I110-AD-ID-CODIGO-DEV TO
                   I040-ID-PROC-CODIGO-CONS
                   END-IF.
       060-0300-BUSCA-PRODUCTO.
          SET T030-I-PRODUCTO TO 1.

          SEARCH T030-DETALLES-PRODUCTOS
          AT END DISPLAY '          TERMINACION ANORMAL, CODIGO 16  '
          DISPLAY '          PRODUCTO NO ENCONTRADO         '
          PERFORM 980-ABORTA

           WHEN T030-CODIGO (T030-I-PRODUCTO)=I040-CODIGO
           MOVE T030-CODIGO(T030-I-PRODUCTO) TO R1-35-PRINT-CODIGO
           MOVE T030-DESCRIPCION(T030-I-PRODUCTO)TO
           R1-35-PRINT-DESCRIPCION
           MOVE T030-CTO(T030-I-PRODUCTO) TO W050-COSTO
       END-SEARCH.

       060-0400-RA-RP.
           IF RI-C-CVE = 'RA' THEN
               ADD RI-C-CANT-CONSUMO TO A060-ACUM-REG-RA-CONSUMO.
               IF RI-C-CVE = 'RP' THEN
                   ADD RI-C-CANT-CONSUMO TO A060-ACUM-REG-RP-CONSUMO
                   END-IF.
                   PERFORM 070-LEE-REG-CONSUMO.
       060-0500-PROCESA-PRODUCTO-CONS.
                PERFORM 060-0400-RA-RP
               UNTIL I040-ID-PROC-CODIGO-CONS NOT =
               I070-AC-ID-CODIGO-CONS.
               
       060-0600-CALCULA-DEV.
           ADD RI-D-CANT-DEVUELTA TO A060-ACUM-DEV.
           PERFORM 110-LEE-REG-DEVOLUCION.

       060-0700-DEVOLUCION.
               PERFORM 060-0600-CALCULA-DEV UNTIL
                   I040-ID-PROC-CODIGO-CONS NOT = I110-AD-ID-CODIGO-DEV.

       060-0800-CALCULA-REG.

           SUBTRACT A060-ACUM-DEV FROM A060-ACUM-REG-RA-CONSUMO.

           MULTIPLY W050-COSTO BY A060-ACUM-REG-RA-CONSUMO
           GIVING A060-ACUM-REG-RA-IMPORTE.
           ADD A060-ACUM-REG-RA-IMPORTE TO A050-ACUM-TOTAL-IMPORTE-RA.
           MOVE A060-ACUM-REG-RA-CONSUMO TO R1-35-PRINT-COMSUMO-RA.
           MOVE A060-ACUM-REG-RA-IMPORTE TO R1-35-PRINT-IMPORTE-RA.
           MULTIPLY W050-COSTO BY A060-ACUM-REG-RP-CONSUMO
           GIVING A060-ACUM-REG-RP-IMPORTE.
           ADD A060-ACUM-REG-RP-IMPORTE TO A050-ACUM-TOTAL-IMPORTE-RP.
           MOVE A060-ACUM-REG-RP-CONSUMO TO R1-35-PRINT-COMSUMO-RP.
           MOVE A060-ACUM-REG-RP-IMPORTE TO R1-35-PRINT-IMPORTE-RP.
           SUBTRACT A060-ACUM-REG-RP-IMPORTE FROM
               A060-ACUM-REG-RA-IMPORTE GIVING A060-ACUM-DIF-IMPORTE.
           SUBTRACT A060-ACUM-REG-RP-CONSUMO FROM
               A060-ACUM-REG-RA-CONSUMO GIVING A060-ACUM-DIF-CONSUMO.
           MOVE A060-ACUM-DIF-IMPORTE TO R1-35-PRINT-IMPORTE-DIF.
           MOVE A060-ACUM-DIF-CONSUMO TO R1-35-PRINT-COMSUMO-DIF.

           IF A060-ACUM-REG-RA-CONSUMO > A060-ACUM-REG-RP-CONSUMO THEN
               ADD A060-ACUM-DIF-IMPORTE TO A050-ACUM-A-FAVOR-RA
               MOVE 'ALMACEN' TO R1-35-PRINT-MENSAJE-A-FAVOR
               
               ADD 1 TO A990-PROC-A-FAVOR-RA
               ELSE IF A060-ACUM-REG-RA-CONSUMO <
                   A060-ACUM-REG-RP-CONSUMO THEN
                   ADD A060-ACUM-DIF-IMPORTE TO A050-ACUM-A-FAVOR-RP
                   ADD 1 TO A990-PROC-A-FAVOR-RP
                   MOVE 'PRODUCCION' TO R1-35-PRINT-MENSAJE-A-FAVOR
                   ELSE
                       MOVE SPACES TO R1-35-PRINT-MENSAJE-A-FAVOR
                       ADD 1 TO A990-PROC-EMPATE
                       END-IF.
               IF (R1-NUM-LIN + 1) > R1-MAX-LIN THEN
                 PERFORM 090-ENCABEZADOS
                 ADD 1 TO R1-NUM-LIN
                 END-IF.

       060-0900-ESCRIBE-DETALLE.
               WRITE R1-COMPARACION FROM R1-35-LINEADETALLE AFTER 1.
               ADD 1 TO R1-NUM-LIN.
               ADD 1 TO A990-PROC-REG.
       060-0990-FIN.
           EXIT.

       070-LEE-REG-CONSUMO SECTION.
       070-0100-INICIO.
           MOVE I070-AC-ID-LEI-CONSUMO TO I070-AC-ID-ANT.

       070-0200-LEE-ARCHIVO.
           READ ARCH-AC-ARCHIVO-CONSUMO AT END MOVE 1
           TO S000-FINARCHCONSUMO.
           IF S000-FINARCHCONSUMO = 1
           THEN
               MOVE HIGH-VALUES TO I070-AC-ID-LEI-CONSUMO
           ELSE
               MOVE RI-C-PTA TO I070-PTA
               MOVE RI-C-DPTO TO I070-DPTO
               MOVE RI-C-CODIGO TO I070-CODIGO.
               ADD 1 TO A990-PROC-LEIDO-CONSUMO.

       070-0300-ABORTA.
           IF I070-AC-ID-LEI-CONSUMO < I070-AC-ID-ANT
           THEN
               MOVE 16 TO RETURN-CODE
               DISPLAY '            ARCHIVO CONSUMO               '
      -        'FUERA DE SECUENCIA     '
               DISPLAY '          ID. ANT. ('  I070-AC-ID-ANT '  )'
               DISPLAY
               '          ID. LEI. ('  I070-AC-ID-LEI-CONSUMO '  )'
               DISPLAY '          TERMINACION ANORMAL, CODIGO 16  '
               PERFORM 980-ABORTA.
       070-990-FIN.
           EXIT.

       080-LEE-REG-TABLA SECTION.
       080-0100-INICIO.
           MOVE I080-AT-ID-LEI-TABLA TO I080-AT-ID-ANT.

       080-0200-LEE-ARCHIVO.
           READ ARCH-AT-ARCHIVO-TABLA AT END MOVE 1
           TO S000-FINARCHTABLA.
           IF S000-FINARCHTABLA = 1
           THEN
               MOVE HIGH-VALUES TO I080-AT-ID-LEI-TABLA
           ELSE
               MOVE RI-T-CVE TO I080-CLAVE
               MOVE RI-T-PTA TO I080-PTA
               MOVE RI-T-DPTO TO I080-DPTO
               ADD 1 TO A990-PROC-LEIDO-TABLA.

       080-0300-ABORTA.
           IF I080-AT-ID-LEI-TABLA < I080-AT-ID-ANT
           THEN
               MOVE 16 TO RETURN-CODE
               DISPLAY '            ARCHIVO TABLA                 '
      -        'FUERA DE SECUENCIA     '
               DISPLAY '          ID. ANT. ('  I080-AT-ID-ANT '  ) '
               DISPLAY
               '          ID. LEI. ('  I080-AT-ID-LEI-TABLA '  )  '
               DISPLAY '          TERMINACION ANORMAL, CODIGO 16  '
               PERFORM 980-ABORTA.
       080-990-FIN.
           EXIT.

       090-ENCABEZADOS SECTION.
       090-0100-INICIO.
           ADD 1 TO R1-NUM-HOJA.
           MOVE R1-NUM-HOJA TO R1-10-PAGNU.

           WRITE R1-COMPARACION FROM R1-05-ENCABEZADOS-PRIMERA-LINEA
           AFTER PAGE.
           WRITE R1-COMPARACION FROM R1-10-ENCABEZADOS-SEGUNDA-LINEA
           AFTER 2.
           WRITE R1-COMPARACION FROM R1-15-ENCABEZADOS-TERCERA-LINEA
           AFTER 1.
           WRITE R1-COMPARACION FROM R1-20-ENCABEZADOS-CUARTA-LINEA
           AFTER 2.
           WRITE R1-COMPARACION FROM R1-25-ENCABEZADOS-QUINTA-LINEA
           AFTER 2.
           WRITE R1-COMPARACION FROM R1-30-ENCABEZADOS-SEXTA-LINEA
           AFTER 1.
           WRITE R1-COMPARACION FROM ' ' AFTER 1.
           MOVE 10 TO R1-NUM-LIN.
       090-990-FIN.
           EXIT.
       
       100-LEE-REG-PRODUCTO SECTION.
       100-0100-INICIO.
           MOVE I100-AP-ID-LEI-PRODUCTO TO I100-AP-ID-ANT.

       100-0200-LEE-ARCHIVO.
           READ ARCH-AP-ARCHIVO-PRODUCTO AT END MOVE 1
           TO S000-FINARCHPRODUCTO.
           IF S000-FINARCHPRODUCTO = 1
           THEN
               MOVE HIGH-VALUES TO I100-AP-ID-LEI-PRODUCTO
           ELSE
               MOVE RI-P-CODIGO TO I100-CODIGO
               ADD 1 TO A990-PROC-LEIDO-PRODUCTO.

       100-0300-ABORTA.
           IF I100-AP-ID-LEI-PRODUCTO < I100-AP-ID-ANT
           THEN
               MOVE 16 TO RETURN-CODE
               DISPLAY '            ARCHIVO PRODUCTO              '
      -        'FUERA DE SECUENCIA     '
               DISPLAY '          ID. ANT. ('  I100-AP-ID-ANT '  )'
               DISPLAY
               '          ID. LEI. ('  I100-AP-ID-LEI-PRODUCTO '  )'
               DISPLAY '          TERMINACION ANORMAL, CODIGO 16  '
               PERFORM 980-ABORTA.
       100-990-FIN.
           EXIT.

       110-LEE-REG-DEVOLUCION SECTION.
       110-0100-INICIO.
           MOVE I110-AD-ID-LEI-DEVOLUCION TO I110-AD-ID-ANT.

       110-0200-LEE-ARCHIVO.
           READ ARCH-AD-ARCHIVO-DEVOLUCION AT END MOVE 1 
           TO S000-FINARCHDEVOLUCION.
           IF S000-FINARCHDEVOLUCION = 1
           THEN
               MOVE HIGH-VALUES TO I110-AD-ID-LEI-DEVOLUCION
           ELSE
               MOVE RI-D-PTA TO I110-PTA
               MOVE RI-D-DPTO TO I110-DPTO
               MOVE RI-D-CODIGO TO I110-CODIGO.
               ADD 1 TO A990-PROC-LEIDO-DEVOLUCION.

       110-0300-ABORTA.
           IF I110-AD-ID-LEI-DEVOLUCION < I110-AD-ID-ANT
           THEN
               MOVE 16 TO RETURN-CODE
               DISPLAY '            ARCHIVO DEVOLUCION            '
      -        'FUERA DE SECUENCIA     '
               DISPLAY '          ID. ANT. ('  I110-AD-ID-ANT '  )'
               DISPLAY 
               '          ID. LEI. ('  I110-AD-ID-LEI-DEVOLUCION '  )'
               DISPLAY '          TERMINACION ANORMAL, CODIGO 16  '
               PERFORM 980-ABORTA.
       110-990-FIN.
           EXIT.


       980-ABORTA SECTION.
              980-0100-INICIO.
           IF S000-ESTADOARCHCONSUMO NOT = 0
           THEN
               CLOSE ARCH-AC-ARCHIVO-CONSUMO
               MOVE 1 TO S000-ESTADOARCHCONSUMO.
           IF S000-ESTADOARCHDEVOLUCION NOT = 0
           THEN
               CLOSE ARCH-AD-ARCHIVO-DEVOLUCION
               MOVE 1 TO S000-ESTADOARCHDEVOLUCION.
           IF S000-ESTADOARCHPRODUCTO NOT = 0
               THEN
               CLOSE ARCH-AP-ARCHIVO-PRODUCTO
               MOVE 1 TO S000-ESTADOARCHPRODUCTO.
            IF S000-ESTADOARCHTABLA NOT = 0
               THEN
               CLOSE ARCH-AT-ARCHIVO-TABLA
               MOVE 1 TO S000-ESTADOARCHTABLA.
           IF S000-ESTADOARCHFECHA NOT = 0
           THEN
               CLOSE ARCH-AF-ARCHIVO-FECHA
               MOVE 0 TO S000-ESTADOARCHFECHA.
           IF S000-ESTADOREPO NOT = 0
               THEN
               CLOSE REPO-R1-REPORTE
               MOVE 1 TO S000-ESTADOREPO.
           PERFORM 990-CIFRAS-CONTROL.
           MOVE 16 TO RETURN-CODE.
       980-FIN.
           GOBACK.

       990-CIFRAS-CONTROL SECTION.
       DISPLAY W000-PROG '  CON.S LEIDOS                           '
      -    '           ' A990-PROC-LEIDO-CONSUMO   .
       DISPLAY W000-PROG '  DEV.S LEIDOS                           '
      -    '           ' A990-PROC-LEIDO-DEVOLUCION   .
       DISPLAY W000-PROG '  PLANTAS PROCESADAS                     '
      -    '           ' A990-PROC-PTA.
       DISPLAY W000-PROG '  DEPARTAMENTOS PROCESADOS               '
      -    '           ' A990-PROC-DPTO.
       DISPLAY W000-PROG '  PROCESOS PROCESADOS                    '
      -    '           ' A990-PROC-REG.
       DISPLAY W000-PROG '  REG.S A FAVOR DE ALMACEN               '
      -    '           ' A990-PROC-A-FAVOR-RA.
       DISPLAY W000-PROG '  REG.S A FAVOR DE PRODUCCION            '
      -    '           ' A990-PROC-A-FAVOR-RP.
       DISPLAY W000-PROG '  REG.S A FAVOR DE NINGUNO               '
      -    '           ' A990-PROC-EMPATE.
       990-FIN.
           EXIT.
