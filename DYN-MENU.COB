      ******************************************************************
      *                    CONSULTA POR ID | NOMBRE DE DEPARTAMENTO    *
      *                             FECHA DEC INGRESO                  *
      ******************************************************************

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
           05 ID-EMPL                  PIC 9(03).
           05 NOM-EMPL                 PIC X(30).
           05 CLV-DEP                  PIC 9.
           05 NOM-DEP                  PIC X(20).
           05 ID-TAB-S                 PIC A.
           05 SUE-MEN                  PIC 9(05).
           05 F-ING                    PIC X(08).
       WORKING-STORAGE SECTION.
       01 WS-FLAG                      PIC 9.
       01 WS-FLAGD                     PIC 9.
       01 WS-FLAGDT                    PIC 9.
       01 WS-RES                       PIC 9(03) VALUE 0.

       01 WS-ID-EMPL                   PIC 9(03).
       01 WS-NOM-DEP                   PIC X(20).
       01 WS-F-ING                     PIC X(08).

       01 TITLE-M.
           05 FILLER PIC X(25) VALUE SPACES.
           05 FILLER PIC X(18) VALUE '1|ID 2|DEP 3|DATE'.
           05 FILLER PIC X(37) VALUE SPACES.
       01 SEP.
           05 FILLER PIC X(80) VALUE ALL '-'.

       PROCEDURE DIVISION.

       010-INITIAL.
           PERFORM 100-INICIO.
           PERFORM 100-MENU UNTIL WS-RES = 999.
           STOP RUN.

************************************************************************
       100-INICIO.
           DISPLAY '               WELCOME                '.
************************************************************************
       100-MENU.
           DISPLAY SEP.
           DISPLAY TITLE-M.
           DISPLAY SEP.
           ACCEPT WS-RES.

           EVALUATE TRUE
           WHEN WS-RES = 1 PERFORM 100-ID
           WHEN WS-RES = 2 PERFORM 200-DEP
           WHEN WS-RES = 3 PERFORM 300-DATE
           WHEN WS-RES = 9 MOVE 999 TO WS-RES
           WHEN OTHER DISPLAY
           'INVALID OPTION TRY AGAIN' PERFORM 100-MENU.
***********************************************************************
       100-ID.
           OPEN INPUT EMPLEADO.
           DISPLAY 'ID OR 999 TO MAIN MENU'          ACCEPT ID-EMPL.
           IF ID-EMPL EQUAL 999 CLOSE EMPLEADO.
           PERFORM 101-IDC UNTIL ID-EMPL EQUAL 999.

           101-IDC.
               READ EMPLEADO KEY IS ID-EMPL
               INVALID KEY DISPLAY 'INVALID ID'
               CLOSE EMPLEADO
               PERFORM 100-ID
               NOT INVALID KEY
                   DISPLAY REG-EMPL
                   CLOSE  EMPLEADO
                   PERFORM 100-ID.
***********************************************************************
       200-DEP.
           OPEN INPUT EMPLEADO.
           MOVE 0 TO WS-FLAGD.
           DISPLAY 'INGRESA DEP.'.                    ACCEPT WS-NOM-DEP.
           IF WS-NOM-DEP EQUAL 999
               MOVE  1 TO WS-FLAGD
               CLOSE EMPLEADO.
           PERFORM 201-DEP UNTIL WS-FLAGD = 1.

           201-DEP.
           READ EMPLEADO AT END MOVE 1 TO WS-FLAGD
           CLOSE EMPLEADO
           PERFORM 200-DEP
           NOT AT END
           IF WS-NOM-DEP EQUAL NOM-DEP
               DISPLAY REG-EMPL.
************************************************************************
       300-DATE.
           OPEN INPUT EMPLEADO.
           MOVE 0 TO WS-FLAGDT.
           DISPLAY 'INGRESA FECHA YYYYMMDD'.          ACCEPT WS-F-ING.
           IF WS-F-ING  EQUAL 999
               MOVE 1 TO WS-FLAGDT CLOSE EMPLEADO.
           PERFORM 301-DATE UNTIL WS-FLAGDT = 1.

           301-DATE.
           READ EMPLEADO AT END MOVE 1 TO WS-FLAGDT
           CLOSE EMPLEADO PERFORM 100-MENU
           NOT AT END
           IF WS-F-ING EQUAL F-ING
               DISPLAY REG-EMPL.
************************************************************************
