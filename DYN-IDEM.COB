      *********************************************************
      *PROGRAMA  1 - MUESTRA EN PANTALLA TODOS LOS REGISTROS  *
      *CLASIFICADOS POR EL ID DE EMPLEADO.                    *
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
       001-INI.
           PERFORM 100-ABRIR.
           PERFORM 100-PROCESO.
           PERFORM 101-PROCESO UNTIL WS-FLAG = 1.
           PERFORM 200-CERRAR.

           STOP RUN.

       100-ABRIR.
           OPEN INPUT EMPLEADO.

       200-CERRAR.
           DISPLAY SEP.
           CLOSE EMPLEADO
           DISPLAY 'ARCHIVO CERRADO.'.

      *Declaramos el inicio del registro por medio de start mayor a 0.
      *Si se declara 1 no se mostraria el primer id,  se puede modificar al gusto.
       100-PROCESO.
           DISPLAY  SEP.
           MOVE 0 TO ID-EMPL
           START EMPLEADO KEY GREATER ID-EMPL.

      *Mostrar todos los registros clasificados.
       101-PROCESO.

           READ EMPLEADO
           AT END MOVE 1 TO WS-FLAG.
           DISPLAY REG-EMPL.
