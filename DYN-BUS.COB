      *********************************************************
      *PROGRAMA  3 - MUESTRA EN REGISTROS POR  ID             *
      *                                                       *
      *********************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. DYN-GEN.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLEADO ASSIGN TO 'C:\Users\pc\Desktop\S\IDYN'
           ORGANIZATION IS INDEXED
           RECORD KEY IS ID-EMPL
           ALTERNATE KEY NOM-DEP
           WITH DUPLICATES
           ALTERNATE KEY F-ING
           WITH DUPLICATES
           ACCESS MODE  IS DYNAMIC.

       DATA DIVISION.
       FILE SECTION.
      *IMPORTANTE INDICAR INDICES A LEER ALPHANUMERIC.
      *Solo modifique fecha.
       FD EMPLEADO.
           01 REG-EMPL.
               05 ID-EMPL              PIC 9(03).
               05 NOM-EMPL             PIC X(30).
               05 CLV-DEP              PIC 9.
               05 NOM-DEP              PIC X(20).
               05 S-TAB                PIC A.
               05 S-MEN                PIC 9(05).
               05 F-ING                PIC X(08).
       WORKING-STORAGE SECTION.
       01 WS-ID-EMPL                   PIC 9(03) VALUE 0.
       01 WS-RES                       PIC X.
       01 WS-FLAG                      PIC 9 VALUE 0.

       01 SEP.
           05 FILLER                   PIC X(80) VALUE  ALL '-'.

       PROCEDURE DIVISION.
      *Se establece  proceso hasta valor 999.
       001-INI.
           PERFORM  100-ABRIR.
           PERFORM  100-PROCESO UNTIL ID-EMPL EQUAL 999.
           PERFORM  101-CERRAR.
           STOP RUN.

           100-ABRIR.
               OPEN INPUT EMPLEADO.

           101-CERRAR.
               DISPLAY SEP.
                   CLOSE EMPLEADO.
                   DISPLAY 'ARCHIVO CERRADO'.
               DISPLAY SEP.
      *Se acepta valor.
           100-PROCESO.
               DISPLAY SEP.
                   DISPLAY 'PUEDES INGRESAR 999 PARA FINALIZAR PROGRAMA'.
               DISPLAY SEP.
                   DISPLAY 'ID.'. ACCEPT ID-EMPL.
      *Inicia valor con start EQUAL identificando el mismo valor.
               START EMPLEADO KEY EQUAL ID-EMPL
      *Se condiciona invalid key segun peticion.
                      INVALID KEY
                   IF
                   ID-EMPL EQUAL 999 DISPLAY  'PETICION  DE CIERRE.'

                   ELSE
                                     DISPLAY 'NO EXISTE ID'
      *Se lee archivo finalmente.
                  NOT INVALID KEY READ EMPLEADO
                                     DISPLAY REG-EMPL.
