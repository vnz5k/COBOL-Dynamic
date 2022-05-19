      ******************************************************************
      *                    CONSULTA TODO | ID | BORRA | AGREGA          *
      *                       MENU                                      *
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
       01 WS-INFO.
           05 WS-TITLE                 PIC X(20)
           VALUE 'ELIGE UNA OPCION'.
           05 WS-MENU                  PIC X(60)
           VALUE '1 CONSULTA | 2 AGREGAR | 3 BORRAR | 4 ALL | 9 EXIT'.

       01 WS-AREAS.
           05 WS-OP1                   PIC 999 VALUE 0.
           05 WS-CON                   PIC X.
           05 WS-FLAGD                 PIC 9 VALUE 0.
           05 WS-FLAGA                 PIC 9 VALUE 0.
           05 WS-DEL                   PIC X.
       01 SEP.
           05 FILLER                   PIC X(79) VALUE ALL '-'.

       PROCEDURE DIVISION.
       PERFORM 100-MENU UNTIL WS-OP1 = 999.
       STOP RUN.


*******************************************************************************
       100-MENU.
           DISPLAY SEP.
           DISPLAY WS-INFO. ACCEPT WS-OP1.
           DISPLAY SEP.
           EVALUATE TRUE
           WHEN WS-OP1 = 1 PERFORM 100-CONSULTA
           WHEN WS-OP1 = 2 PERFORM 200-AGREGAR
           WHEN WS-OP1 = 3 PERFORM 300-BORRAR
           WHEN WS-OP1 = 4 PERFORM 400-ALL
           WHEN WS-OP1 = 9 MOVE 999 TO WS-OP1  DISPLAY 'COMMAND EXIT'
           WHEN OTHER DISPLAY 'INVALID OPTION'
           PERFORM 100-MENU.
*******************************************************************************
       100-CONSULTA.
           DISPLAY '.:::::::BIENVENIDO A CONSULTA:::::::.'
           OPEN INPUT EMPLEADO.
           PERFORM 101-CONSULTA-ID UNTIL  WS-CON = 'N'.
           CLOSE EMPLEADO.
           PERFORM 100-MENU.

           101-CONSULTA-ID.
           DISPLAY 'INGRESA UN ID: ' ACCEPT ID-EMPL.

           READ EMPLEADO KEY ID-EMPL
           INVALID KEY                 DISPLAY 'INVALID ID'
           NOT INVALID KEY

           DISPLAY SEP.
           DISPLAY 'DATA INFO: ' REG-EMPL.
           DISPLAY SEP.

           DISPLAY 'CONSULTAR OTRO S/N' ACCEPT WS-CON.

*******************************************************************************
       200-AGREGAR.
           DISPLAY '.:::::::BIENVENIDO A AGREGAR:::::::.'
           OPEN I-O EMPLEADO.
           PERFORM 201-AGREGAR THRU 202-FING.
           DISPLAY 'REGISTRO AGREGADO:' REG-EMPL
           WRITE REG-EMPL.
           CLOSE EMPLEADO.
           PERFORM 100-MENU.


           201-AGREGAR.
               DISPLAY 'INGRESA ID' ACCEPT ID-EMPL.
               DISPLAY 'NOMBRE'     ACCEPT NOM-EMPL.

               201-DEP.
               DISPLAY 'CLV-DEP'    ACCEPT CLV-DEP.
               EVALUATE TRUE
                   WHEN CLV-DEP = 1 MOVE 'LIMPIEZA' TO NOM-DEP
                   WHEN CLV-DEP = 2 MOVE 'COBRANZA' TO NOM-DEP
                   WHEN CLV-DEP = 3 MOVE 'SISTEMAS' TO NOM-DEP
                   WHEN OTHER PERFORM 201-DEP.

               202-SUELDO.
               DISPLAY 'ID-TAB-S'  ACCEPT ID-TAB-S.
               EVALUATE TRUE
                   WHEN ID-TAB-S = 1 MOVE 'A' TO ID-TAB-S
                   MOVE 10000 TO SUE-MEN
                   WHEN ID-TAB-S = 2 MOVE 'B' TO ID-TAB-S
                   MOVE 10000 TO SUE-MEN
                   WHEN ID-TAB-S = 3 MOVE 'C' TO ID-TAB-S
                   MOVE 10000 TO SUE-MEN
                   WHEN OTHER  PERFORM 202-SUELDO.

               202-FING.
               DISPLAY 'F-ING'    ACCEPT F-ING
               EVALUATE TRUE
               WHEN F-ING LESS 20190101
                   PERFORM 202-FING
               WHEN F-ING GREATER 20190101
                       DISPLAY 'DATE AVADIABLE'
               WHEN OTHER PERFORM 202-FING.
*******************************************************************************
       300-BORRAR.
           OPEN I-O EMPLEADO.
           PERFORM 301-ALL UNTIL WS-FLAGD = 1.
           PERFORM 301-DEL THRU 301-DELE UNTIL ID-EMPL EQUAL 999.
           MOVE 0 TO WS-FLAGD.
           CLOSE EMPLEADO.
           PERFORM 100-MENU.

           301-ALL.
               READ EMPLEADO AT END MOVE 1 TO WS-FLAGD
               NOT AT END DISPLAY REG-EMPL.

               301-DEL.
                   DISPLAY 'INGRESA ID TO DELETE'. ACCEPT ID-EMPL.

                   READ EMPLEADO KEY ID-EMPL
                   INVALID KEY DISPLAY 'ID INVALID'
                   NOT INVALID KEY DISPLAY REG-EMPL.

                   301-DELE.
                       DISPLAY 'ELIMINAR S/N? ' ACCEPT WS-DEL
                       IF WS-DEL = 'S'
                           DELETE EMPLEADO
                           DISPLAY 'DELETED...'
                               MOVE 999 TO ID-EMPL.
                           IF WS-DEL = 'N'
                               MOVE 999 TO ID-EMPL.

*******************************************************************************
       400-ALL.
           OPEN INPUT EMPLEADO.
           DISPLAY 'TODOS LOS REGISTROS SON: '
           PERFORM 401-ALL UNTIL WS-FLAGA = 1.
           MOVE  0 TO WS-FLAGA.
           PERFORM 100-MENU.

           401-ALL.
               READ EMPLEADO AT END CLOSE EMPLEADO MOVE 1 TO WS-FLAGA
               NOT AT END
               DISPLAY REG-EMPL.
*******************************************************************************
