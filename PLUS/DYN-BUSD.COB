      *************************************************************
      *            CONSULTA  DE DEPARTAMENTO COMO KEY ALTERNO     *
      *************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MENUES.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLEADO ASSIGN TO 'C:\Users\pc\Desktop\S\IDYN'
           ORGANIZATION IS INDEXED

           RECORD  KEY  ID-EMPL
           ALTERNATE RECORD KEY NOM-DEP WITH DUPLICATES

           ALTERNATE RECORD KEY F-ING WITH DUPLICATES
           ACCESS MODE SEQUENTIAL.
      *El access mode es  SEQUENTIAL para obtener datos de un DEP.

       DATA DIVISION.
       FILE SECTION.
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
       01 WS-OPCION                    PIC X.
       01 WS-RES                       PIC X.
       01 WS-FLAG                      PIC 9 VALUE 0.

       01 SEP.
           05 FILLER PIC X(80) VALUE  ALL '-'.

       01 WS-REG-EMPL.
               05 WS-ID-EMPL              PIC 9(03).
               05 WS-NOM-EMPL             PIC X(30).
               05 WS-CLV-DEP              PIC 9.
               05 WS-NOM-DEP              PIC X(20).
               05 WS-S-TAB                PIC A.
               05 WS-S-MEN                PIC 9(05).
               05 WS-F-ING                PIC X(08).

       PROCEDURE DIVISION.
       001-INI.
           PERFORM 100-ABRIR.
           PERFORM 100-BUSDEP.
           PERFORM 101-BUSDEP UNTIL WS-FLAG EQUAL  1.
           PERFORM 200-CERRAR.
        STOP RUN.

       100-ABRIR.
           OPEN INPUT EMPLEADO.
       200-CERRAR.
           DISPLAY SEP.
           CLOSE EMPLEADO
           DISPLAY 'ARCHIVO CERRADO.'.

       100-BUSDEP.
           ACCEPT WS-NOM-DEP.
           DISPLAY SEP.

      *Archivo indexado busca de manera secuencial datos por nombre.
      *Al finalizar hace display de  datos por departamento.
       101-BUSDEP.
           READ EMPLEADO
           AT END MOVE  1 TO WS-FLAG
           NOT AT END
           IF WS-NOM-DEP EQUAL  NOM-DEP

           DISPLAY REG-EMPL.
