       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARCHIVOD.
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
      *Entrada
      *----
           SELECT ARCH-AA-ARCHIVO-A ASSIGN W000-UT-S-DIR-A
                                           ORGANIZATION IS LINE
                                           SEQUENTIAL.

           SELECT ARCH-AB-ARCHIVO-B ASSIGN W000-UT-S-DIR-B
                                           ORGANIZATION IS LINE
                                           SEQUENTIAL.

           SELECT ARCH-AC-ARCHIVO-C ASSIGN W000-UT-S-DIR-C
                                           ORGANIZATION IS LINE
                                           SEQUENTIAL.
       *----
      *Salida
      *----
           SELECT ARCH-AD-ARCHIVO-D ASSIGN TO
           W000-UT-S-DIR-SALIDA-D.

       DATA DIVISION.
       FILE SECTION.
       FD  ARCH-AA-ARCHIVO-A
           LABEL RECORD STANDARD.
       01 AI-ARCHIVO-A.
           05 RI-CLAVE-A                 PIC 9(3).
           05 RI-A1                      PIC 9(3).
           05 RI-A2                      PIC 9(3).
           05 RI-A3                      PIC 9(3).

       FD  ARCH-AB-ARCHIVO-B
           LABEL RECORD STANDARD.
       01 AI-ARCHIVO-B.
           05 RI-CLAVE-B                 PIC 9(3).
           05 RI-B1                      PIC 9(3).
           05 RI-B2                      PIC 9(3).
           05 RI-B3                      PIC 9(3).
           05 RI-B4                      PIC 9(3).

       FD  ARCH-AC-ARCHIVO-C
           LABEL RECORD STANDARD.
       01 AI-ARCHIVO-C.
           05 RI-CLAVE-C                 PIC 9(3).
           05 RI-C1                      PIC 9(3).
           05 RI-C2                      PIC 9(3).

       FD  ARCH-AD-ARCHIVO-D
          RECORD CONTAINS 90 CHARACTERS
           LABEL RECORD STANDARD.
       01  AO-D                          PIC X(90).


       WORKING-STORAGE SECTION.
      *---
      *Switches
      *---
       01  S000-ESTADOS.
           05  S000-ESTADOARCHA         PIC X.
           05  S000-FINARCHA            PIC X.
           05  S000-ESTADOARCHB         PIC X.
           05  S000-FINARCHB            PIC X.
           05  S000-ESTADOARCHC         PIC X.
           05  S000-FINARCHC            PIC X.
           05  S000-ESTADOARCHD         PIC X.


      *---
      *Constantes
      *---
       01  W000-CTES.
           05  W000-UT-S-DIR-A        PIC X(20)
                                      VALUE 'ArchivoA.dat'.
           05  W000-UT-S-DIR-B        PIC X(20)
                                      VALUE 'ArchivoB.dat'.
           05  W000-UT-S-DIR-C        PIC X(20)
                                      VALUE 'ArchivoC.dat'.
           05  W000-UT-S-DIR-SALIDA-D PIC X(20)
                                      VALUE 'ArchivoD.txt'.
      *---
      *   Identidades
      *---
       01 I030-AA-ID-ANT              PIC S9(3).

       01 I030-AA-ID-LEI.
           05 I030-AA-ID-CLAVE-A.
               10 I030-AA-CLAVE-A     PIC S9(3).

       01 I040-AB-ID-ANT              PIC S9(3).

       01 I040-AB-ID-LEI.
           05 I040-AB-ID-CLAVE-B.
               10 I040-AB-CLAVE-B     PIC S9(3).

       01 I050-AC-ID-ANT              PIC S9(3).

       01 I050-AC-ID-LEI.
           05 I050-AC-ID-CLAVE-C.
               10 I050-AC-CLAVE-C     PIC S9(3).

       01 I060-ID-PROC.
           05 I060-ID-PROC-CLAVE.
               10 I060-PROC-CLAVE     PIC S9(3).


      *---
      *   Acumuladores
      *---
       01 A010-ACUM-A.
           05 A010-ACUMULADOR-A       PIC S9(4).

       01 A010-ACUM-B.
           05 A010-ACUMULADOR-B       PIC S9(4).

       01 A060-ACUM-C.
           05 A060-ACUMULADOR-C1      PIC S9(4).
           05 A010-ACUMULADOR-C2      PIC S9(4).

       01 A010-ACUM-D.
           05 A010-ACUMULADOR-D3      PIC S9(4).
      *---
      *Cifras de control
      *---
       
       01 A990-CIFRAS-CONTROL.
           05 A990-PROC-LEIDO-A       PIC S9(9).
           05 A990-PROC-LEIDO-B       PIC S9(9).
           05 A990-PROC-LEIDO-C       PIC S9(9).
           05 A990-PROC-REG-D1        PIC S9(9).
           05 A990-PROC-REG-D2        PIC S9(9).
           05 A990-PROC-REG-D3        PIC S9(9).

      *---
      *Imprime detalle
      *---
       01 A1-05-DETALLE.
           05 FILLER                  PIC XX   VALUE SPACES.
           05 A1-05-LLAVE             PIC XXX.
           05 FILLER                  PIC XXX  VALUE SPACES.
           05 A1-05-D1                PIC X.
           05 FILLER                  PIC XXX  VALUE SPACES.
           05 A1-05-D2                PIC X(4).
           05 FILLER                  PIC XXX  VALUE SPACES.
           05 A1-05-D3                PIC X(4).


       PROCEDURE DIVISION.
       000-CONTROL SECTION.
       000-0100-INICIO.
           OPEN INPUT ARCH-AA-ARCHIVO-A.
           MOVE 0 TO S000-FINARCHA.
           MOVE 0 TO S000-ESTADOARCHA.

           OPEN INPUT ARCH-AB-ARCHIVO-B.
           MOVE 0 TO S000-FINARCHB.
           MOVE 0 TO S000-ESTADOARCHB.

           OPEN INPUT ARCH-AC-ARCHIVO-C.
           MOVE 0 TO S000-FINARCHC.
           MOVE 0 TO S000-ESTADOARCHC.

           OPEN OUTPUT ARCH-AD-ARCHIVO-D.
           MOVE 0 TO S000-ESTADOARCHD.

           MOVE ZEROS TO A990-CIFRAS-CONTROL.

           MOVE LOW-VALUES TO I030-AA-ID-LEI.
           MOVE LOW-VALUES TO I040-AB-ID-LEI.
           MOVE LOW-VALUES TO I050-AC-ID-LEI.

       000-0200-LEE-REGISTOS-DE-A.
           PERFORM 030-LEE-REG-A.
       000-0300-LEE-REGISTROS-DE-B.
           PERFORM 040-LEE-REG-B.
       000-0400-LEE-REGISTROS-DE-C.
           PERFORM 050-LEE-REG-C.

       000-0500-PROCESA-REGISTROS.
           PERFORM 010-PROCESA-REGISTROS UNTIL (S000-FINARCHA = 1 AND
               S000-FINARCHB = 1 AND
                S000-FINARCHC = 1).

       000-0600-TERMINA.
           CLOSE ARCH-AA-ARCHIVO-A.
           MOVE 1 TO S000-ESTADOARCHA.
           CLOSE ARCH-AB-ARCHIVO-B.
           MOVE 1 TO S000-ESTADOARCHB.
           CLOSE ARCH-AC-ARCHIVO-C.
           MOVE 1 TO S000-ESTADOARCHC.
           CLOSE ARCH-AD-ARCHIVO-D.
           MOVE 1 TO S000-ESTADOARCHD.
           PERFORM 990-CIFRAS-CONTROL.

       000-9900-FIN.
           GOBACK.

       010-LEE SECTION.
       PERFORM 030-LEE-REG-A.
       PERFORM 040-LEE-REG-B.
       PERFORM 050-LEE-REG-C.

       010-FIN.
           EXIT.
       
       010-PROCESA-REGISTROS SECTION.
       010-0100-INICIO.
           MOVE 0 TO A010-ACUMULADOR-A.
           MOVE 0 TO A010-ACUMULADOR-B.
           MOVE 0 TO A010-ACUMULADOR-C2.
       010-0200-PROCESA-REG.
           IF (I030-AA-ID-CLAVE-A<I040-AB-ID-CLAVE-B
               AND I030-AA-ID-CLAVE-A < I050-AC-ID-CLAVE-C) THEN
               PERFORM 080-A
               ELSE IF (I040-AB-ID-CLAVE-B<I030-AA-ID-CLAVE-A
                   AND I040-AB-ID-CLAVE-B<I050-AC-ID-CLAVE-C)
                   PERFORM 090-B
                   ELSE IF (I050-AC-ID-CLAVE-C<I030-AA-ID-CLAVE-A AND
                       I050-AC-ID-CLAVE-C<I040-AB-ID-CLAVE-B)
                       PERFORM 100-C
                       ELSE IF (I030-AA-ID-CLAVE-A=I040-AB-ID-CLAVE-B
                           AND I030-AA-ID-CLAVE-A< I050-AC-ID-CLAVE-C)

                           PERFORM 110-A-B
                           ELSE IF (I040-AB-ID-CLAVE-B
                               =I050-AC-ID-CLAVE-C AND
                               I050-AC-ID-CLAVE-C<I030-AA-ID-CLAVE-A)
                               PERFORM 130-B-C
                               ELSE IF (I030-AA-ID-CLAVE-A=
                                   I050-AC-ID-CLAVE-C AND
                                   I030-AA-ID-CLAVE-A<
                                   I040-AB-ID-CLAVE-B)
                                   PERFORM 120-A-C
                                   ELSE
                                       PERFORM 140-A-B-C
                                       END-IF.

       010-0300-CALCULA-D3.
           PERFORM 020-CALCULA-D3.
       010-0400-ESCRIBE-DETALLE.
           MOVE A010-ACUMULADOR-D3 TO A1-05-D3.
           WRITE AO-D FROM A1-05-DETALLE AFTER 1.
           ADD 1 TO A990-PROC-REG-D1.
           ADD 1 TO A990-PROC-REG-D2.

       010-9900-FIN.
       EXIT.

       020-CALCULA-D3 SECTION.
       020-0100-INICIO.
           MOVE 0 TO A010-ACUMULADOR-D3.
       020-0200-SUMA-ACUMULADOR-A-D3.
           ADD A010-ACUMULADOR-A TO A010-ACUMULADOR-D3.
           ADD A010-ACUMULADOR-B TO A010-ACUMULADOR-D3.
           ADD A010-ACUMULADOR-C2 TO A010-ACUMULADOR-D3.

       020-0300-CIFRA-CONTROL.
           ADD 1 TO A990-PROC-REG-D3.
       020-9900-FIN.
       EXIT.

       030-LEE-REG-A SECTION.
       030-0100-INICIO.
           MOVE I030-AA-ID-LEI TO I030-AA-ID-ANT.

       030-0200-LEE-ARCHIVO.
           READ ARCH-AA-ARCHIVO-A AT END MOVE 1 TO S000-FINARCHA.
           IF S000-FINARCHA = 1
           THEN
               MOVE HIGH-VALUES TO I030-AA-ID-LEI
           ELSE
               MOVE RI-CLAVE-A TO I030-AA-CLAVE-A.
               ADD 1 TO A990-PROC-LEIDO-A.
       030-0300-ABORTA.
           IF I030-AA-ID-LEI < I030-AA-ID-ANT
           THEN
               MOVE 16 TO RETURN-CODE
               DISPLAY '            ARCHIVO A                     '
      -        'FUERA DE SECUENCIA     '
               DISPLAY '          ID. ANT. ('  I030-AA-ID-ANT '  )'
               DISPLAY '          ID. LEI. ('  I030-AA-ID-LEI '  )'
               DISPLAY '          TERMINACION ANORMAL, CODIGO 16  '
               PERFORM 980-ABORTA.
                
       030-9900-FIN.
       EXIT.

       040-LEE-REG-B SECTION.
       040-0100-INICIO.
           MOVE I040-AB-ID-LEI TO I040-AB-ID-ANT.

       040-0200-LEE-ARCHIVO.
           READ ARCH-AB-ARCHIVO-B AT END MOVE 1 TO S000-FINARCHB.
           IF S000-FINARCHB = 1
           THEN
               MOVE HIGH-VALUES TO I040-AB-ID-LEI
           ELSE
               MOVE RI-CLAVE-B TO I040-AB-CLAVE-B.
               ADD 1 TO A990-PROC-LEIDO-B.

       040-0300-ABORTA.
           IF I040-AB-ID-LEI < I040-AB-ID-ANT
           THEN
               MOVE 16 TO RETURN-CODE
               DISPLAY '            ARCHIVO B                     '
      -        'FUERA DE SECUENCIA     '
               DISPLAY '          ID. ANT. ('  I040-AB-ID-ANT '  )'
               DISPLAY '          ID. LEI. ('  I040-AB-ID-LEI '  )'
               DISPLAY '          TERMINACION ANORMAL, CODIGO 16  '
               PERFORM 980-ABORTA.
       040-9900-FIN.
       EXIT.

       050-LEE-REG-C SECTION.
       050-0100-INICIO.
           MOVE I050-AC-ID-LEI TO I050-AC-ID-ANT.

       050-0200-LEE-ARCHIVO.
           READ ARCH-AC-ARCHIVO-C AT END MOVE 1 TO S000-FINARCHC.
           IF S000-FINARCHC = 1
           THEN
               MOVE HIGH-VALUES TO I050-AC-ID-LEI
           ELSE
               MOVE RI-CLAVE-C TO I050-AC-CLAVE-C.
               ADD 1 TO A990-PROC-LEIDO-C.
       030-0500-ABORTA.
           IF I050-AC-ID-LEI < I050-AC-ID-ANT
           THEN
               MOVE 16 TO RETURN-CODE
               DISPLAY '            ARCHIVO C                     '
      -        'FUERA DE SECUENCIA     '
               DISPLAY '          ID. ANT. ('  I050-AC-ID-ANT '  )'
               DISPLAY '          ID. LEI. ('  I050-AC-ID-LEI '  )'
               DISPLAY '          TERMINACION ANORMAL, CODIGO 16  '
               PERFORM 980-ABORTA.
       050-9900-FIN.
       EXIT.

       060-CALCULA-SUMA-C1-C2 SECTION.
       060-0100-INICIO.
           MOVE I050-AC-ID-CLAVE-C TO I060-ID-PROC-CLAVE.
           MOVE 0 TO A060-ACUMULADOR-C1.

       060-0200-CALCULA-C1-C2.
           PERFORM 070-SUMA-C1-C2 UNTIL
               I050-AC-ID-CLAVE-C NOT = I060-ID-PROC-CLAVE.
       060-9900-FIN.
       EXIT.


       070-SUMA-C1-C2 SECTION.
       070-0100-INICIO.
       070-0200-SUMA-C1.
           ADD RI-C1 TO A060-ACUMULADOR-C1.
       070-0300-SUMA-C2.
           ADD RI-C2 TO A010-ACUMULADOR-C2.
       070-0400-LEE-REGISTRO-C.
           PERFORM 050-LEE-REG-C.
       070-9900-FIN.
       EXIT.

       080-A SECTION.
       080-0100-INICIO.
           MOVE RI-CLAVE-A TO A1-05-LLAVE.
           MOVE 5 TO A1-05-D1.
           MOVE RI-A1 TO A1-05-D2.
       080-0200-ACUMULA-EN-A.
           ADD RI-A2 TO A010-ACUMULADOR-A.
           ADD RI-A3 TO A010-ACUMULADOR-A.
       080-0300-LEE-NUEVO-REG-A.
           PERFORM 030-LEE-REG-A.
       080-9900-FIN.
       EXIT.

       090-B SECTION.
       090-0100-INICIO.
           MOVE RI-CLAVE-B TO A1-05-LLAVE.
           MOVE 6 TO A1-05-D1.
           MOVE RI-B1 TO A1-05-D2.
       090-0200-ACUMULA-EN-B.
           ADD RI-B2 TO A010-ACUMULADOR-B.
           ADD RI-B3 TO A010-ACUMULADOR-B.
           ADD RI-B4 TO A010-ACUMULADOR-B.
       090-0300-LEE-NUEVO-REG-B.
           PERFORM 040-LEE-REG-B.
       090-9900-FIN.
       EXIT.

       100-C SECTION.
       100-0100-INICIO.
           MOVE RI-CLAVE-C TO A1-05-LLAVE.
           MOVE 7 TO A1-05-D1.

       100-0200-CALCULA-C1-C2.
           PERFORM 060-CALCULA-SUMA-C1-C2.

       100-0300-ASIGNA-D2.
           MOVE A060-ACUMULADOR-C1 TO A1-05-D2.



       100-9900-FIN.
           EXIT.

       110-A-B SECTION.
       110-0100-INICIO.
           MOVE RI-CLAVE-A TO A1-05-LLAVE.
           MOVE 2 TO A1-05-D1.
           MOVE RI-A1 TO A1-05-D2.
       110-0200-ACUMULA-EN-A.
           ADD RI-A2 TO A010-ACUMULADOR-A.
           ADD RI-A3 TO A010-ACUMULADOR-A.
       110-0300-ACUMULA-EN-B.
           ADD RI-B2 TO A010-ACUMULADOR-B.
           ADD RI-B3 TO A010-ACUMULADOR-B.
           ADD RI-B4 TO A010-ACUMULADOR-B.
       110-0400-LEE-NUEVO-REG-A.
           PERFORM 030-LEE-REG-A.
       110-0500-LEE-NUEVO-REG-B.
           PERFORM 040-LEE-REG-B.
       110-9900-FIN.
           EXIT.

       120-A-C SECTION.
       120-0100-INICIO.
           MOVE RI-CLAVE-A TO A1-05-LLAVE.
           MOVE 3 TO A1-05-D1.
           MOVE RI-A1 TO A1-05-D2.
       120-0200-ACUMULA-EN-A.
           ADD RI-A2 TO A010-ACUMULADOR-A.
           ADD RI-A3 TO A010-ACUMULADOR-A.
       120-0300-CALCULA-C1-C2.
           PERFORM 060-CALCULA-SUMA-C1-C2.
       120-0400-LEE-NUEVO-REG-A.
           PERFORM 030-LEE-REG-A.


       120-9900-FIN.
       EXIT.

       130-B-C SECTION.
       130-0100-INICIO.
           MOVE RI-CLAVE-B TO A1-05-LLAVE.
           MOVE 4 TO A1-05-D1.
           MOVE RI-B1 TO A1-05-D2.
       130-0200-ACUMULA-EN-B.
           ADD RI-B2 TO A010-ACUMULADOR-B.
           ADD RI-B3 TO A010-ACUMULADOR-B.
           ADD RI-B4 TO A010-ACUMULADOR-B.
       130-0300-CALCULA-C1-C2.
           PERFORM 060-CALCULA-SUMA-C1-C2.
       130-0400-LEE-NUEVO-REG-B.
           PERFORM 040-LEE-REG-B.

       130-9900-FIN.
       EXIT.

       140-A-B-C SECTION.
       140-0100-INICIO.
           MOVE RI-CLAVE-A TO A1-05-LLAVE.
           MOVE 1 TO A1-05-D1.
           MOVE RI-A1 TO A1-05-D2.
       140-0200-ACUMULA-EN-A.
           ADD RI-A2 TO A010-ACUMULADOR-A.
           ADD RI-A3 TO A010-ACUMULADOR-A.
       140-0300-ACUMULA-EN-B.
           ADD RI-B2 TO A010-ACUMULADOR-B.
           ADD RI-B3 TO A010-ACUMULADOR-B.
           ADD RI-B4 TO A010-ACUMULADOR-B.
       140-0400-CALCULA-C1-C2.
           PERFORM 060-CALCULA-SUMA-C1-C2.
       140-0500-LEE-NUEVO-REG-A.
           PERFORM 030-LEE-REG-A.
       140-0600-LEE-NUEVO-REG-B.
           PERFORM 040-LEE-REG-B.


       140-9900-FIN.
       EXIT.

       
       980-ABORTA SECTION.
       980-0100-INICIO.
           IF S000-ESTADOARCHA NOT = 1
           THEN
               CLOSE ARCH-AA-ARCHIVO-A
               MOVE 1 TO S000-ESTADOARCHA.

           IF S000-ESTADOARCHB NOT = 1
           THEN
               CLOSE ARCH-AB-ARCHIVO-B
               MOVE 1 TO S000-ESTADOARCHB.

           IF S000-ESTADOARCHC NOT = 1
               THEN
               CLOSE ARCH-AC-ARCHIVO-C
               MOVE 1 TO S000-ESTADOARCHC.
           IF S000-ESTADOARCHD NOT = 1
               THEN
               CLOSE ARCH-AD-ARCHIVO-D
               MOVE 1 TO S000-ESTADOARCHD.

           PERFORM 990-CIFRAS-CONTROL.
       980-9900-FIN.
           GOBACK.



       990-CIFRAS-CONTROL SECTION.
       990-0100-INICIO.
       DISPLAY  '             A.S LEIDOS                           '
      -    '           ' A990-PROC-LEIDO-A     .

       DISPLAY  '             B.S LEIDOS                           '
      -    '           ' A990-PROC-LEIDO-B     .

       DISPLAY  '             C.S LEIDOS                           '
      -    '           ' A990-PROC-LEIDO-C     .

       DISPLAY  '            D1.S PROCESADOS                       '
      -    '           ' A990-PROC-REG-D1      .

       DISPLAY  '            D2.S PROCESADOS                       '
      -    '           ' A990-PROC-REG-D2      .

       DISPLAY  '            D3.S PROCESADOS                       '
      -    '           ' A990-PROC-REG-D3      .

       990-9900-FIN.
           EXIT.
