       IDENTIFICATION DIVISION.
       PROGRAM-ID. ID-DYN.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLEADO ASSIGN TO 'C:\Users\pc\Desktop\S\DYN.TXT'
           ORGANIZATION  IS  INDEXED
           RECORD KEY IS ID-EMPL
           ALTERNATE RECORD KEY NOM-DEP  WITH DUPLICATES
           ALTERNATE RECORD KEY F-ING WITH DUPLICATES
           ACCESS MODE IS DYNAMIC.

       DATA DIVISION.
       FILE SECTION.
       FD EMPLEADO.
       01 REG-EMPL.
           05 ID-EMPL          PIC 9(03).
           05 NOM-EMPL         PIC X(30).
           05 CLV-DEP          PIC 9.
           05 NOM-DEP          PIC X(20).
           05 ID-TAB-S         PIC A.
           05 SUE-MEN          PIC 9(05).
           05 F-ING            PIC X(08).
       WORKING-STORAGE SECTION.
       01 WS-NOM-DEP           PIC X(20).
       01 WS-FLAG              PIC 9 VALUE 0.
       01 WS-RES               PIC X(05) VALUE 'EXIT'.

       PROCEDURE DIVISION.
       010-INITIAL.
           PERFORM 100-INICIO.
           PERFORM 100-DEP.
           PERFORM 101-MAS UNTIL WS-RES = 'EXIT'.

       STOP RUN.

       100-INICIO.
           OPEN INPUT EMPLEADO.

       100-DEP.
           DISPLAY 'ID.'
           ACCEPT WS-NOM-DEP.
           PERFORM 101-READ UNTIL WS-FLAG EQUAL 1.


           101-READ.
               READ EMPLEADO AT END CLOSE EMPLEADO PERFORM 101-MAS
               MOVE 1  TO WS-FLAG
               NOT AT END
               IF WS-NOM-DEP EQUAL NOM-DEP
                   DISPLAY REG-EMPL.

           101-MAS.
               DISPLAY 'OTRO ID'.
               ACCEPT WS-RES.
               IF WS-RES EQUAL 'S'
                   MOVE 0 TO WS-FLAG
                   OPEN INPUT EMPLEADO PERFORM 100-DEP.
