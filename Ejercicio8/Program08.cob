       IDENTIFICATION DIVISION.
       PROGRAM-ID. REPORTEAPLICAMOVIMIENTOS.
       AUTHOR.
           PROG.ADRIANACORTES
       DATE-WRITTEN.
           24/JUNE/2015
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SOURCE-COMPUTER.
           IBM-370.
       OBJECT-COMPUTER.
           IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *-----------------------
      *    ENTRADA
      *----
           SELECT ARCH-AM-ARCHIVO-MOV ASSIGN W000-UT-S-DIR-MOV
                                           ORGANIZATION IS LINE
                                           SEQUENTIAL.

           SELECT ARCH-AE-ARCHIVO-EMP ASSIGN W000-UT-S-DIR-EMP
                                           ORGANIZATION IS LINE
                                           SEQUENTIAL.
      *----
      *    SALIDA
      *----
           SELECT REPO-R1-REPORTE-MOV ASSIGN TO
           W000-UT-S-DIR-SALIDA-MOV.

           SELECT ARCH-AAE-ARCHIVO-ACT-EMP ASSIGN TO
           W000-UT-S-DIR-SALIDA-EMP.


       DATA DIVISION.
       FILE SECTION.
       FD  ARCH-AM-ARCHIVO-MOV
           LABEL RECORD STANDARD.

       01 RI-REPORTE.
           05 RI-CVE-MOV                 PIC X.
           05 RI-NO-TRAB                 PIC X(6).
           05 RI-GPO                     PIC XX.
           05 RI-EMP                     PIC XXX.
           05 RI-PTA                     PIC XXX.
           05 RI-DPTO                    PIC X(6).
           05 RI-CVEO-E                  PIC X.
           05 RI-NOMBRE                  PIC X(30).
           05 RI-SALBASE                 PIC S9(7)V99.
           05 RI-FECHA-ING               PIC 9(6).

       FD  ARCH-AE-ARCHIVO-EMP
           LABEL RECORD STANDARD.

       01 AI-ARCHIVO-EMPLEADO.
           05 AI-NO-TRAB                 PIC X(6).
           05 AI-GPO                     PIC XX.
           05 AI-EMP                     PIC XXX.
           05 AI-PTA                     PIC XXX.
           05 AI-DPTO                    PIC X(6).
           05 AI-CVEO-E                  PIC X.
           05 AI-NOMBRE                  PIC X(30).
           05 AI-SALBASE                 PIC S9(7)V99.
           05 AI-FECHA-ING               PIC 9(6).

       FD  REPO-R1-REPORTE-MOV
          RECORD CONTAINS 90 CHARACTERS
           LABEL RECORD STANDARD.

       01  R1-MOVIMIENTOS                PIC X(90).

       FD  ARCH-AAE-ARCHIVO-ACT-EMP
          RECORD CONTAINS 90 CHARACTERS
           LABEL RECORD STANDARD.

       01  A1-ACTUALIZA                  PIC X(90).


       WORKING-STORAGE SECTION.
      *---
      *   Switches
      *---
       01  S000-ESTADOS.
           05  S000-ESTADOARCHMOV         PIC X.
           05  S000-FINARCHMOV            PIC X.
           05  S000-ESTADOARCHEMP         PIC X.
           05  S000-FINARCHEMP            PIC X.
           05  S000-ESTADOREPOMOV         PIC X.
           05  S000-ESTADOARCHACTEMP      PIC X.

      *---
      *   Constantes
      *---
       01  W000-CTES.
           05  W000-PROG                  PIC X(8)
                                      VALUE 'P-CHE120'.
           05  W000-UT-S-DIR-MOV          PIC X(20)
                                      VALUE 'ArchMovimientos.dat'.
           05  W000-UT-S-DIR-EMP          PIC X(20)
                                      VALUE 'ArchEmpleados.dat'.
           05  W000-UT-S-DIR-SALIDA-MOV   PIC X(20)
                                      VALUE 'ReporteMov.txt'.
           05  W000-UT-S-DIR-SALIDA-EMP   PIC X(20)
                                      VALUE 'ArchActEmp.txt'.

      *---
      *   Identidades
      *---
       01 I060-AM-ID-ANT                  PIC X(6).

       01 I060-AM-ID-LEI.
           05 I060-AM-ID-NO-TRAB.
               10 I060-AM-NO-TRAB         PIC X(6).

       01 I070-AE-ID-ANT                  PIC X(6).

       01 I070-AE-ID-LEI.
           05 I070-AE-ID-NO-TRAB.
               10 I070-AE-NO-TRAB         PIC X(6).
      *---
      *Cifras de control
      *---

       01 A990-CIFRAS-CONTROL.
           05 A990-PROC-MOV-LEIDO         PIC S9(9).
           05 A990-PROC-EMP-LEIDO         PIC S9(9).
           05 A990-PROC-EMP-COPIADO       PIC S9(9).
           05 A990-MOV-PROC               PIC S9(9).
           05 A990-ALT-PROC               PIC S9(9).
           05 A990-BAJ-PROC               PIC S9(9).
           05 A990-CAM-PROC               PIC S9(9).
           05 A990-ALT-FALLIDA-PROC       PIC S9(9).
           05 A990-BAJ-FALLIDA-PROC       PIC S9(9).
           05 A990-CAM-FALLIDA-PROC       PIC S9(9).

      *---
      *Variables
      *--

       01 R1-VARS.
           05 R1-NUM-HOJA                 PIC S9(9).
           05 R1-NUM-LIN                  PIC S9(9).
           05 R1-MAX-LIN                  PIC S9(9)       VALUE 30.
           05 R1-MENSAJE                  PIC X(20).

      *---
      *Líneas de encabezado
      *---
       01 R1-05-ENCABEZADOS-PRIMERA-LINEA.
           05 FILLER                      PIC X           VALUE SPACES.
           05 R1-05-CONSPRO               PIC X(8).
           05 FILLER                      PIC X(18)       VALUE SPACES.
           05 FILLER                      PIC X(38)       VALUE
           "ACTUALIZACION DEL ARCHIVO DEL PERSONAL".
           05 FILLER                      PIC X(77)       VALUE SPACES.

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
           "PERSONAL".
           05 FILLER                      PIC X(77)       VALUE SPACES.

       01 R1-20-ENCABEZADOS-CUARTA-LINEA.
           05 FILLER                      PIC X(10)       VALUE
           "NO.TRAB.".
           05 FILLER                      PIC X(3)        VALUE SPACE.
           05 FILLER                      PIC X(20)       VALUE
           "MOVIMIENTO EFECTUADO".
           05 FILLER                      PIC X(27)       VALUE SPACES.

      *---
      *Imprime detalle
      *---
       01 R1-25-DETALLE.
           05 FILLER                      PIC X(2)        VALUE
           SPACES.
           05 R1-25-NO-TRAB               PIC X(6).
           05 FILLER                      PIC X(5)        VALUE
           SPACES.
           05 R1-25-MENSAJE               PIC X(20).
           05 FILLER                      PIC X(27)       VALUE
           SPACES.

       01 A1-05-DETALLE.
           05 A1-05-NO-TRAB               PIC X(6).
           05 FILLER                      PIC X(1)       VALUE
           SPACES.
           05 A1-05-GPO                   PIC XX.
           05 FILLER                      PIC X(1)       VALUE
           SPACES.
           05 A1-05-EMP                   PIC XXX.
           05 FILLER                      PIC X(1)       VALUE
           SPACES.
           05 A1-05-PTA                   PIC XXX.
           05 FILLER                      PIC X(1)       VALUE
           SPACES.
           05 A1-05-DPTO                  PIC X(6).
           05 FILLER                      PIC X(1)       VALUE
           SPACES.
           05 A1-05-CVEOE                 PIC X.
           05 FILLER                      PIC X(1)       VALUE
           SPACES.
           05 A1-05-NOMBRE                PIC X(30).
           05 FILLER                      PIC X(1)       VALUE
           SPACES.
           05 A1-05-SAL-BASE              PIC S9(7)V99.
           05 FILLER                      PIC X(1)       VALUE
           SPACES.
           05 A1-05-FECHA                 PIC 9(6).
           05 FILLER                      PIC X(1)       VALUE
           SPACES.


       PROCEDURE DIVISION.
       000-CONTROL SECTION.
       000-0100-INICIO.
      *---
      *Abre archivos
      *--
           OPEN INPUT ARCH-AM-ARCHIVO-MOV.
           MOVE 0 TO S000-FINARCHMOV.
           MOVE 0 TO S000-ESTADOARCHMOV.

           OPEN INPUT ARCH-AE-ARCHIVO-EMP.
           MOVE 0 TO S000-FINARCHEMP.
           MOVE 0 TO S000-ESTADOARCHEMP.

           OPEN OUTPUT ARCH-AAE-ARCHIVO-ACT-EMP.
           MOVE 0 TO S000-ESTADOARCHACTEMP.

           OPEN OUTPUT REPO-R1-REPORTE-MOV.
           MOVE 0 TO S000-ESTADOREPOMOV.

           MOVE 0 TO R1-NUM-HOJA.
           MOVE ZEROS TO A990-CIFRAS-CONTROL.
           MOVE LOW-VALUES TO I060-AM-ID-LEI.
           MOVE LOW-VALUES TO I070-AE-ID-LEI.

           MOVE W000-PROG TO R1-05-CONSPRO.

           MOVE R1-MAX-LIN TO R1-NUM-LIN.

       000-0200-LEE-MOVIMIENTO.
           PERFORM 070-LEE-MOVIMIENTO.


       000-0300-LEE-EMPLEADO.
           PERFORM 080-LEE-EMP.

       000-0400-PROCESA-EMPLEADO.
           PERFORM 020-PROCESA-EMPLEADO UNTIL (S000-FINARCHMOV=1
               AND S000-FINARCHEMP=1).

       000-0500-TERMINA.
           CLOSE ARCH-AM-ARCHIVO-MOV.
           MOVE 1 TO S000-ESTADOARCHMOV.
           CLOSE ARCH-AE-ARCHIVO-EMP.
           MOVE 1 TO S000-ESTADOARCHEMP.
           CLOSE ARCH-AAE-ARCHIVO-ACT-EMP.
           MOVE 1 TO S000-ESTADOARCHACTEMP.
           CLOSE REPO-R1-REPORTE-MOV.
           MOVE 1 TO S000-ESTADOREPOMOV.
           PERFORM 990-CIFRAS-CONTROL.

       000-9900-FIN.
           GOBACK.

       010-COPIA-EMPLEADO SECTION.
       010-0100-INICIO.
           PERFORM 090-CARGA-DATOS.

       010-0200-ESCRIBE-DETALLE.
           WRITE A1-ACTUALIZA FROM A1-05-DETALLE AFTER 1.
           ADD 1 TO A990-PROC-EMP-COPIADO.

       010-0300-LEE-EMPLEADO.
           PERFORM 080-LEE-EMP.

       010-9900-FIN.
       EXIT.

       020-PROCESA-EMPLEADO SECTION.
       020-0100-INICIO.
           IF (I060-AM-ID-NO-TRAB>I070-AE-ID-NO-TRAB) THEN
               PERFORM 010-COPIA-EMPLEADO
               ELSE
                   PERFORM 030-PROCESA-PETICION
                   END-IF.

       020-9900-FIN.
       EXIT.

       030-PROCESA-PETICION SECTION.
       030-0100-INICIO.
       030-0200-PROCESA-MOVIMIENTO.
           IF RI-CVE-MOV = 'A' THEN
               PERFORM 040-ALTA
               ELSE IF RI-CVE-MOV = 'B' THEN
                   PERFORM 050-BAJA
                   ELSE
                       PERFORM 060-CAMBIO
                       END-IF.

       030-0300-ESCRIBE-DETALLE-MOV.
           IF (R1-NUM-LIN + 2)>R1-MAX-LIN THEN
               PERFORM 110-ENCABEZADOS.
               MOVE I060-AM-NO-TRAB TO R1-25-NO-TRAB.
               MOVE R1-MENSAJE TO R1-25-MENSAJE.
               WRITE R1-MOVIMIENTOS FROM R1-25-DETALLE AFTER 2.
               ADD 2 TO R1-NUM-LIN.
               ADD 1 TO A990-MOV-PROC.

       030-0400-LEE-MOVIMIENTO.
           PERFORM 070-LEE-MOVIMIENTO.

       030-9900-FIN.
       EXIT.

       040-ALTA SECTION.
       040-0100-INICIO.
       040-0200-PROCESA-ALTA.
           IF I060-AM-ID-NO-TRAB = I070-AE-ID-NO-TRAB THEN
               MOVE 'ALTA YA EXISTE' TO R1-MENSAJE
               ADD 1 TO A990-ALT-FALLIDA-PROC
               PERFORM 010-COPIA-EMPLEADO
               ELSE
                   MOVE RI-NO-TRAB TO A1-05-NO-TRAB
                   MOVE ZEROS TO A1-05-GPO
                   MOVE ZEROS TO A1-05-EMP
                   MOVE ZEROS TO A1-05-PTA
                   MOVE ZEROS TO A1-05-DPTO
                   MOVE 0 TO A1-05-CVEOE
                   MOVE SPACES TO A1-05-NOMBRE
                   MOVE ZERO TO A1-05-SAL-BASE
                   MOVE FUNCTION CURRENT-DATE (3:6) TO A1-05-FECHA
                   PERFORM 100-CAMBIA-CAMPOS
                   WRITE A1-ACTUALIZA FROM A1-05-DETALLE AFTER 1
                   MOVE 'ALTA' TO R1-MENSAJE
                   ADD 1 TO A990-ALT-PROC
                   END-IF.

       040-9900-FIN.
       EXIT.

       050-BAJA SECTION.
       050-0100-INICIO.
       050-0200-PROCESA-BAJA.
           IF I060-AM-ID-NO-TRAB = I070-AE-ID-NO-TRAB THEN
               MOVE 'BAJA' TO R1-MENSAJE
               ADD 1 TO A990-BAJ-PROC
               PERFORM 080-LEE-EMP
               ELSE
                   MOVE 'BAJA NO EXISTE' TO R1-MENSAJE
                   ADD 1 TO A990-BAJ-FALLIDA-PROC
                   END-IF.
       050-9900-FIN.
       EXIT.

       060-CAMBIO SECTION.
       060-0100-INICIO.
       060-0200-PROCESA-CAMBIO.
           IF I060-AM-ID-NO-TRAB = I070-AE-ID-NO-TRAB THEN
               MOVE 'CAMBIO' TO R1-MENSAJE
               PERFORM 090-CARGA-DATOS
               PERFORM 100-CAMBIA-CAMPOS
               WRITE A1-ACTUALIZA FROM A1-05-DETALLE AFTER 1
               PERFORM 080-LEE-EMP
               ADD 1 TO A990-CAM-PROC
               ELSE
                   MOVE 'CAMBIO NO EXISTE' TO R1-MENSAJE
                   ADD 1 TO A990-CAM-FALLIDA-PROC
                   END-IF.

       060-9900-FIN.
       EXIT.

       070-LEE-MOVIMIENTO SECTION.
       070-0100-INICIO.
           MOVE I060-AM-ID-LEI TO I060-AM-ID-ANT.
       070-0200-LEE-ARCHIVO.
           READ ARCH-AM-ARCHIVO-MOV AT END MOVE 1 TO S000-FINARCHMOV.
           IF S000-FINARCHMOV = 1 THEN
               MOVE HIGH-VALUES TO I060-AM-ID-LEI
               ELSE
                   MOVE RI-NO-TRAB TO I060-AM-NO-TRAB.
                   ADD 1 TO A990-PROC-MOV-LEIDO.
       070-0300-ABORTA.
           IF I060-AM-ID-LEI < I060-AM-ID-ANT
           THEN
               MOVE 16 TO RETURN-CODE
               DISPLAY W000-PROG '  ARCHIVO REPORTE DE MOVIMIENTOS'
      -        'FUERA DE SECUENCIA     '
               DISPLAY '          ID. ANT. ('  I060-AM-ID-ANT '  )'
               DISPLAY '          ID. LEI. ('  I060-AM-ID-LEI '  )'
               DISPLAY W000-PROG '  TERMINACION ANORMAL, CODIGO 16'
               PERFORM 980-ABORTA.
       070-9900-FIN.
       EXIT.

       080-LEE-EMP SECTION.
       080-0100-INICIO.
           MOVE I070-AE-ID-LEI TO I070-AE-ID-ANT.
       080-0200-LEE-ARCHIVO.
           READ ARCH-AE-ARCHIVO-EMP AT END MOVE 1 TO S000-FINARCHEMP.
           IF S000-FINARCHEMP = 1 THEN
               MOVE HIGH-VALUES TO I070-AE-ID-LEI
               ELSE
                   MOVE AI-NO-TRAB TO I070-AE-NO-TRAB.
                   ADD 1 TO A990-PROC-EMP-LEIDO.
       080-0300-ABORTA.
           IF I070-AE-ID-LEI < I070-AE-ID-ANT
           THEN
               MOVE 16 TO RETURN-CODE
               DISPLAY W000-PROG '  ARCHIVO EMPLEADO ACTUALIZADO  '
      -        'FUERA DE SECUENCIA     '
               DISPLAY '          ID. ANT. ('  I070-AE-ID-ANT '  )'
               DISPLAY '          ID. LEI. ('  I070-AE-ID-LEI '  )'
               DISPLAY W000-PROG '  TERMINACION ANORMAL, CODIGO 16'
               PERFORM 980-ABORTA.
       080-9900-FIN.
       EXIT.

       090-CARGA-DATOS SECTION.
       090-0100-INICIO.
       090-0200-CARGA-DATOS.
           MOVE AI-NO-TRAB TO A1-05-NO-TRAB.
           MOVE AI-GPO TO A1-05-GPO.
           MOVE AI-EMP TO A1-05-EMP.
           MOVE AI-PTA TO A1-05-PTA.
           MOVE AI-DPTO TO A1-05-DPTO.
           MOVE AI-CVEO-E TO A1-05-CVEOE.
           MOVE AI-NOMBRE TO A1-05-NOMBRE.
           MOVE AI-SALBASE TO A1-05-SAL-BASE.
           MOVE AI-FECHA-ING TO A1-05-FECHA.

       090-9900-FIN.
       EXIT.

       100-CAMBIA-CAMPOS SECTION.
       100-0100-INICIO.
       100-0200-CAMBIA-CAMPOS.
           IF RI-GPO NOT = SPACES THEN
               MOVE RI-GPO TO A1-05-GPO.
           IF RI-EMP NOT = SPACES THEN
               MOVE RI-EMP TO A1-05-EMP.
           IF RI-PTA NOT = SPACES THEN
               MOVE RI-PTA TO A1-05-PTA.
           IF RI-DPTO NOT = SPACES THEN
               MOVE RI-DPTO TO A1-05-DPTO.
           IF RI-CVEO-E NOT = SPACES THEN
               MOVE RI-CVEO-E TO A1-05-CVEOE.
           IF RI-NOMBRE NOT = SPACES THEN
               MOVE RI-NOMBRE TO A1-05-NOMBRE.
           IF RI-SALBASE NOT = SPACES THEN
               MOVE RI-SALBASE TO A1-05-SAL-BASE.
           IF RI-FECHA-ING NOT = A1-05-FECHA THEN
               MOVE RI-FECHA-ING TO A1-05-FECHA.
       100-9900-FIN.
       EXIT.

       110-ENCABEZADOS SECTION.
       110-0100-INICIO.
           ADD 1 TO R1-NUM-HOJA.
           MOVE R1-NUM-HOJA TO R1-10-PAGNU.
       110-0200-ESCRIBE-ENCABEZADOS.
           WRITE R1-MOVIMIENTOS FROM R1-05-ENCABEZADOS-PRIMERA-LINEA
           AFTER PAGE.
           WRITE R1-MOVIMIENTOS FROM R1-10-ENCABEZADOS-SEGUNDA-LINEA
           AFTER 2.
           WRITE R1-MOVIMIENTOS FROM R1-15-ENCABEZADOS-TERCERA-LINEA
           AFTER 1.
           WRITE R1-MOVIMIENTOS FROM R1-20-ENCABEZADOS-CUARTA-LINEA
           AFTER 2.
           MOVE 6 TO R1-NUM-LIN.
       110-9900-FIN.
       EXIT.


       
       980-ABORTA SECTION.
       980-0100-INICIO.
          IF S000-ESTADOARCHMOV NOT = 1
           THEN
               CLOSE ARCH-AM-ARCHIVO-MOV
               MOVE 1 TO S000-ESTADOARCHMOV.

           IF S000-ESTADOREPOMOV NOT = 1
           THEN
               CLOSE REPO-R1-REPORTE-MOV
               MOVE 1 TO S000-ESTADOREPOMOV.
           IF S000-ESTADOARCHEMP NOT = 1
               THEN
               CLOSE ARCH-AE-ARCHIVO-EMP
               MOVE 1 TO S000-ESTADOARCHEMP.
           IF S000-ESTADOARCHACTEMP NOT = 1
               THEN
               CLOSE ARCH-AAE-ARCHIVO-ACT-EMP
               MOVE 1 TO S000-ESTADOARCHACTEMP.

           PERFORM 990-CIFRAS-CONTROL.
       980-9900-FIN.
           GOBACK.



       990-CIFRAS-CONTROL SECTION.
       990-0100-INICIO.
       DISPLAY W000-PROG '  MOV.S LEIDOS                           '
      -    '           ' A990-PROC-MOV-LEIDO   .

       DISPLAY W000-PROG '  EMP.S LEIDOS                           '
      -    '           ' A990-PROC-EMP-LEIDO   .

       DISPLAY W000-PROG '  MOV.S PROCESADOS                       '
      -    '           ' A990-MOV-PROC         .

       DISPLAY W000-PROG '  ALTAS PROCESADAS                       '
      -    '           ' A990-ALT-PROC         .

       DISPLAY W000-PROG '  BAJAS PROCESADAS                       '
      -    '           ' A990-BAJ-PROC         .

       DISPLAY W000-PROG '  CAMBIOS PROCESADOS                     '
      -    '           ' A990-CAM-PROC         .
       
       DISPLAY W000-PROG '  ALTAS FALLIDAS EN PROCESAR             '
      -    '           ' A990-ALT-FALLIDA-PROC         .

       DISPLAY W000-PROG '  BAJAS FALLIDAS EN PROCESAR             '
      -    '           ' A990-BAJ-FALLIDA-PROC         .

       DISPLAY W000-PROG '  CAMBIOS FALLIDOS EN PROCESAR           '
      -    '           ' A990-CAM-FALLIDA-PROC         .
       
       DISPLAY W000-PROG '  EMPLEADOS COPIADOS                     '
      -    '           ' A990-PROC-EMP-COPIADO .
       990-9900-FIN.
           EXIT.
