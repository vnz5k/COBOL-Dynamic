      ******************************************************************
      *                    CONSULTA TODO | ID | BORRA | AGREGA | EDITA *
      *                       MENU                                     *
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
           05 WS-MENU                  PIC X(59)
           VALUE '1 ALL | 2 CONSULTA | 3 BORRAR | 4 AGREGAR | 5 EDIT'.
           05 WS-FIN                   PIC X(15)
           VALUE 'PRESS 9 TO EXIT'.

       01 WS-CONSULTA.
           05 WS-TITLE                 PIC X(20)
           VALUE 'TIPO DE CONSULTA: '.
           05 WS-MENU                  PIC X(59)
           VALUE '1 ID | 2 DEPARTAMENTO | 3 FECHA | 9 BACK'.
       01 WS-DEPARTAMENTO.
           05 WS-TITLE                 PIC X(25)
           VALUE 'SELECCIONA DEPARTAMENTO: '.
           05 WS-MENU                  PIC X(59)
           VALUE '1 LIMPIEZA | 2 COBRANZA | 3 SISTEMAS '.
       01 WS-TABSUELDO.
           05 WS-TITLE                 PIC X(30)
           VALUE 'SELECCIONA SUELDO A ASIGNAR: '.
           05 WS-MENU                  PIC X(59)
           VALUE '1 10,000 | 2 20,000 | 3 30,000 '.
       01 WS-FINGRESO.
           05 WS-TITLE                 PIC X(30)
           VALUE 'INGRESA FECHA MAYOR A ->'.
           05 WS-MENU                  PIC X(59)
           VALUE '.::  2019 01 01 ::.'.
       01 WS-DEL.
           05 WS-TITLE                 PIC X(25)
           VALUE 'ID TOTALES : '.

       01 WS-AREAS.
           05 WS-OP1                   PIC 999 VALUE 0.
           05 WS-CON                   PIC 999 VALUE 0.

           05 WS-ALL.
              06  FILLER PIC X(25) VALUE SPACES.
              06  FILLER PIC X(18) VALUE 'TOTAL DE REGISTROS'.
              06  FILLER PIC X(25) VALUE SPACES.
           05 WS-NOM-DEP               PIC X(20).
           05 WS-F-ING                 PIC X(08).
           05 WS-ID-DEL                PIC 9(03).
       01 SEP.
           05 FILLER                   PIC X(79) VALUE ALL '-'.

       01 WS-FLAGS.
           05 WS-FLAGALL PIC 9 VALUE 0.
           05 WS-FLAGDEP PIC 9 VALUE 0.
           05 WS-FLAGF   PIC 9 VALUE 0.
           05 WS-FLAGD   PIC 9 VALUE 0.
           05 WS-FLAGD2  PIC 9 VALUE 0.

       PROCEDURE DIVISION.
       PERFORM 100-MENU UNTIL WS-OP1 = 999.
       STOP RUN.
*******************************************************************************
           100-MENU.
               DISPLAY SEP.
               DISPLAY WS-INFO.
               DISPLAY SEP.
               ACCEPT WS-OP1.
               EVALUATE TRUE
                   WHEN WS-OP1 = 1 PERFORM 100-ALL
                   WHEN WS-OP1 = 2 PERFORM 200-CONSULTA UNTIL WS-CON = 9
                   WHEN WS-OP1 = 3 PERFORM 300-DEL
                   WHEN WS-OP1 = 4 PERFORM 400-ADD
                   WHEN WS-OP1 = 5 PERFORM 500-EDIT
                   WHEN WS-OP1 = 9 MOVE 999 TO WS-OP1
                   DISPLAY '.::::EXIT COMMAND::::.'
                   WHEN OTHER PERFORM 100-MENU.
*******************************************************************************
       100-ALL.
           OPEN INPUT EMPLEADO.
           DISPLAY SEP.
           DISPLAY WS-ALL.
           DISPLAY SEP.
           PERFORM 101-MOSTRAR UNTIL WS-FLAGALL = 1.
           MOVE 0 TO WS-FLAGALL.

           101-MOSTRAR.
               READ EMPLEADO AT END
               CLOSE EMPLEADO MOVE 1 TO WS-FLAGALL
               NOT AT END DISPLAY REG-EMPL.
*******************************************************************************
       200-CONSULTA.
           DISPLAY SEP.
           DISPLAY WS-CONSULTA.
           DISPLAY SEP.
           ACCEPT WS-CON.
               EVALUATE TRUE
               WHEN WS-CON = 1 PERFORM 200-ID
               WHEN WS-CON = 2 PERFORM 200-DEPARTAMENTO
               WHEN WS-CON = 3 PERFORM 200-FECHA
               WHEN WS-CON = 9 DISPLAY 'BACK MAIN MENU'
               WHEN OTHER DISPLAY 'INVALID OPTION' PERFORM 200-CONSULTA.
      ************ ID EMPLEADO.
           200-ID.
               OPEN INPUT EMPLEADO.
           DISPLAY 'INGRESA UN ID: ' ACCEPT ID-EMPL.

           READ EMPLEADO KEY ID-EMPL INVALID KEY DISPLAY 'INVALID ID'
           NOT INVALID KEY DISPLAY 'REGISTRO CONSULTA: ' REG-EMPL.
           CLOSE EMPLEADO.
      ************ DEPARTAMENTO.
           200-DEPARTAMENTO.
               OPEN INPUT EMPLEADO.
           DISPLAY 'INGRESA UN DEPARTAMENTO: ' ACCEPT WS-NOM-DEP.
           PERFORM 200-DEP UNTIL WS-FLAGDEP = 1.
           MOVE 0 TO WS-FLAGDEP.

               200-DEP.
                   READ EMPLEADO AT END CLOSE EMPLEADO
                   MOVE 1 TO WS-FLAGDEP
                   NOT AT END
                   IF WS-NOM-DEP = NOM-DEP
                   DISPLAY REG-EMPL.
      ************ FECHA.
           200-FECHA.
               OPEN INPUT EMPLEADO.
           DISPLAY 'INGRESA UNA FECHA A CONSULTAR' ACCEPT WS-F-ING.
           PERFORM 200-FECHAC UNTIL WS-FLAGF = 1.
           MOVE 0 TO WS-FLAGF.

               200-FECHAC.
                   READ EMPLEADO AT END CLOSE EMPLEADO
                   MOVE 1 TO WS-FLAGF
                   NOT AT END
                   IF WS-F-ING = F-ING
                       DISPLAY REG-EMPL.

*******************************************************************************
       300-DEL.
           DISPLAY SEP. DISPLAY WS-DEL. DISPLAY SEP.
           OPEN I-O EMPLEADO.
           PERFORM 300-DELLALL UNTIL WS-FLAGD = 1.
           MOVE 0 TO WS-FLAGD.
           PERFORM 301-DEL UNTIL WS-FLAGD2 = 1.
           MOVE 0 TO WS-FLAGD2.
           CLOSE EMPLEADO.

           300-DELLALL.
               READ EMPLEADO AT END MOVE  1 TO WS-FLAGD
               NOT AT END DISPLAY REG-EMPL.

           301-DEL.
               DISPLAY '[ ID PARA BORRAR ]' ACCEPT WS-ID-DEL.

               READ EMPLEADO KEY ID-EMPL INVALID KEY
               DISPLAY 'INVALID ID'
               NOT INVALID KEY DISPLAY SEP.
               DISPLAY '[ ELIMINAR S/N... ]' REG-EMPL
               DISPLAY SEP.
               ACCEPT WS-DEL.
               IF WS-DEL = 'S'
                   DELETE EMPLEADO DISPLAY '.:ELIMINADO:.'.
                       MOVE 1 TO WS-FLAGD2.
                   IF WS-DEL = 'N'
                       MOVE 1 TO WS-FLAGD2.

*******************************************************************************
       400-ADD.
           PERFORM 100-ALL.
           OPEN I-O EMPLEADO.
           PERFORM 400-ADD1 THRU 400-ADD4.
           DISPLAY 'DATA RECOPILED:' REG-EMPL
           WRITE REG-EMPL. DISPLAY 'ESCRITO...'.
           CLOSE EMPLEADO. DISPLAY 'ARCHIVO CERRADO...'.
           PERFORM 100-ALL.


           400-ADD1.
               DISPLAY 'ID: ' ACCEPT ID-EMPL.
               DISPLAY 'NOMBRE: ' ACCEPT NOM-EMPL.

           400-ADD2.
               DISPLAY SEP.
               DISPLAY WS-DEPARTAMENTO.
               DISPLAY SEP.
               ACCEPT CLV-DEP.
               EVALUATE TRUE
               WHEN CLV-DEP = 1 MOVE 'LIMPIEZA' TO NOM-DEP
               WHEN CLV-DEP = 2 MOVE 'COBRANZA' TO NOM-DEP
               WHEN CLV-DEP = 3 MOVE 'SISTEMAS' TO NOM-DEP
               WHEN OTHER DISPLAY 'DEPARTAMENTO NO EXISTE'
               PERFORM 400-ADD2.
           400-ADD3.
               DISPLAY SEP.
               DISPLAY WS-TABSUELDO.
               DISPLAY SEP.
               ACCEPT ID-TAB-S.
               EVALUATE TRUE
               WHEN ID-TAB-S = 1 MOVE 'A' TO ID-TAB-S
               MOVE 10000 TO SUE-MEN
               WHEN ID-TAB-S = 2 MOVE 'B' TO ID-TAB-S
               MOVE 20000 TO SUE-MEN
               WHEN ID-TAB-S = 3 MOVE 'C' TO ID-TAB-S
               MOVE 30000 TO SUE-MEN
               WHEN OTHER DISPLAY 'INVALID OPTION' PERFORM 400-ADD3.
           400-ADD4.
               DISPLAY SEP.
               DISPLAY WS-FINGRESO.
               DISPLAY SEP.
               ACCEPT F-ING.
               EVALUATE TRUE
               WHEN F-ING LESS 20190101
               DISPLAY 'FECHA MENOR A 2019' PERFORM 400-ADD4
               WHEN F-ING GREATER 20190101
               DISPLAY 'REGISTRO AGREGADO.'
               WHEN OTHER
               DISPLAY 'ARGUMENTO INVALIDO.'
               PERFORM 400-ADD4.

*******************************************************************************
       500-EDIT.
           PERFORM 100-ALL.
           OPEN I-O EMPLEADO.
           PERFORM 500-EDITD THRU 500-ADD4.
           DISPLAY 'NUEVA INFORMACION: ' REG-EMPL.
           REWRITE REG-EMPL. DISPLAY 'DATA UPDATE...'.
           CLOSE EMPLEADO. DISPLAY 'EMPLEADO CLOSED....'.


           500-EDITD.
               DISPLAY 'SELECCIONA EL ID PARA EDITAR: '. ACCEPT ID-EMPL.
               READ EMPLEADO KEY ID-EMPL
               INVALID KEY DISPLAY 'INVALID ID'
               NOT INVALID KEY DISPLAY 'ID SELECTED: ' REG-EMPL.

               DISPLAY 'INGRESA NOMBRE: ' ACCEPT NOM-EMPL.

           500-ADD2.
               DISPLAY SEP.
               DISPLAY WS-DEPARTAMENTO.
               DISPLAY SEP.
               ACCEPT CLV-DEP.
               EVALUATE TRUE
               WHEN CLV-DEP = 1 MOVE 'LIMPIEZA' TO NOM-DEP
               WHEN CLV-DEP = 2 MOVE 'COBRANZA' TO NOM-DEP
               WHEN CLV-DEP = 3 MOVE 'SISTEMAS' TO NOM-DEP
               WHEN OTHER DISPLAY 'DEPARTAMENTO NO EXISTE'
               PERFORM 500-ADD2.
           500-ADD3.
               DISPLAY SEP.
               DISPLAY WS-TABSUELDO.
               DISPLAY SEP.
               ACCEPT ID-TAB-S.
               EVALUATE TRUE
               WHEN ID-TAB-S = 1 MOVE 'A' TO ID-TAB-S
               MOVE 10000 TO SUE-MEN
               WHEN ID-TAB-S = 2 MOVE 'B' TO ID-TAB-S
               MOVE 20000 TO SUE-MEN
               WHEN ID-TAB-S = 3 MOVE 'C' TO ID-TAB-S
               MOVE 30000 TO SUE-MEN
               WHEN OTHER DISPLAY 'INVALID OPTION' PERFORM 500-ADD3.
           500-ADD4.
               DISPLAY SEP.
               DISPLAY WS-FINGRESO.
               DISPLAY SEP.
               ACCEPT F-ING.
               EVALUATE TRUE
               WHEN F-ING LESS 20190101
               DISPLAY 'FECHA MENOR A 2019' PERFORM 500-ADD4
               WHEN F-ING GREATER 20190101
               DISPLAY 'REGISTRO AGREGADO.'
               WHEN OTHER
               DISPLAY 'ARGUMENTO INVALIDO.'
               PERFORM 500-ADD4.

*******************************************************************************
