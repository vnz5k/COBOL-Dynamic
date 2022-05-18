      ******************************************************************
      *                    CONSULTA TODOS LOS REGISTROS                 *
      *                       AGREGA REGISTROS                          *
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
       01 WS-AREA.
           05 WS-RES PIC X.
           05 WS-MID PIC X VALUE 'A'.
           05 WS-ID-EMPL PIC 9(03).
       01 WS-FLAGER.
           05 WS-FLAG PIC 9 VALUE 0.
           05 WS-FLAG2 PIC 9 VALUE 0.

       PROCEDURE DIVISION.
       010-INITIAL.
           PERFORM 100-ABRIR.
           PERFORM 100-PROCESO.
           PERFORM 200-AGREGAR UNTIL WS-RES = 'N'.
           PERFORM 100-CERRAR.
       STOP RUN.

       100-ABRIR.
           OPEN INPUT EMPLEADO.
       100-CERRAR.
           CLOSE EMPLEADO.

       100-PROCESO.
           DISPLAY ':::::::::REGISTRO ACTUALES:::::::::'.

           DISPLAY 'CONSULTAR BD? S/N'.
           ACCEPT WS-MID.
               IF WS-MID = 'S'
                   PERFORM 100-LEER UNTIL WS-FLAG EQUAL 1.
               IF WS-MID = 'N'
                   CLOSE EMPLEADO.

           100-LEER.
               READ EMPLEADO AT END CLOSE EMPLEADO MOVE 1 TO WS-FLAG
               NOT AT END DISPLAY REG-EMPL.

       200-AGREGAR.
           OPEN I-O EMPLEADO
           DISPLAY 'AGREGAR REGISTRO? S/N'.
           ACCEPT WS-RES.
               IF WS-RES = 'S'
                   PERFORM 100-DATOS THRU 103-FING
                   WRITE REG-EMPL
                   DISPLAY 'ESCRITO'
                   MOVE 0 TO WS-FLAG
                   PERFORM 100-PROCESO.

       100-DATOS.
               DISPLAY '--------------INPUT NEW DATA-----------------'
               DISPLAY 'ID'                        ACCEPT ID-EMPL.
               DISPLAY 'NOMBRE'                    ACCEPT NOM-EMPL.
               101-DATOS.
               DISPLAY 'CLV-DEP'.                  ACCEPT CLV-DEP.
               EVALUATE TRUE
               WHEN CLV-DEP = 1 MOVE 'LIMPIEZA' TO NOM-DEP
               WHEN CLV-DEP = 2 MOVE 'COBRANZA' TO NOM-DEP
               WHEN CLV-DEP = 3 MOVE 'SISTEMAS' TO NOM-DEP
               WHEN OTHER PERFORM 101-DATOS.
               102-DATOS.
               DISPLAY 'ID-TAB-S'.                 ACCEPT ID-TAB-S.
               EVALUATE TRUE
               WHEN ID-TAB-S = 1 MOVE 'A' TO ID-TAB-S
               MOVE 10000 TO SUE-MEN
               WHEN ID-TAB-S = 2 MOVE 'B' TO ID-TAB-S
               MOVE 20000 TO SUE-MEN
               WHEN ID-TAB-S = 3 MOVE 'C' TO ID-TAB-S
               MOVE 30000 TO SUE-MEN
               WHEN OTHER PERFORM 102-DATOS.
               103-FING.
               DISPLAY 'F-ING'                     ACCEPT F-ING.
               EVALUATE TRUE
               WHEN F-ING LESS 20190101 PERFORM 103-FING
               WHEN F-ING GREATER 20190101 DISPLAY '[[EXITOSO]]'
               WHEN OTHER DISPLAY 'INVALID' PERFORM 103-FING.
