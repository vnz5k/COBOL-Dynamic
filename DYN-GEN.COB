       IDENTIFICATION DIVISION.
       PROGRAM-ID. DYN-GEN.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLEADO ASSIGN TO 'C:\Users\pc\Desktop\S\IDYN'
           ORGANIZATION IS INDEXED
           RECORD KEY IS ID-EMPL
           ACCESS MODE  IS DYNAMIC.

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
               05 F-ING                PIC 9(08).
       WORKING-STORAGE SECTION.
       01 WS-RES                       PIC X.
       01 WS-FLAG                      PIC 9 VALUE 0.

       01 SEP.
           05 FILLER                   PIC X(80) VALUE  ALL '-'.


       PROCEDURE DIVISION.
       001-INI.
           PERFORM 100-CREA
           PERFORM 100-PROCESO THRU 102-FING UNTIL WS-FLAG = 1
           PERFORM 200-CERRAR
           STOP RUN.

       100-CREA.
           OPEN OUTPUT EMPLEADO.

       200-CERRAR.
           CLOSE EMPLEADO.
           DISPLAY 'ARCHIVO GUARDADO'.

       100-PROCESO.
           DISPLAY 'ID-EMPL:'           ACCEPT ID-EMPL.
           DISPLAY 'NOM-EMPL:'          ACCEPT NOM-EMPL.
           DISPLAY 'CLV-DEP:'           ACCEPT CLV-DEP.

           101-DEP.
           DISPLAY 'ELIGE UN DEPARTAMENTO'.
           DISPLAY '1 LIMPIEZA | 2 COBRANZA  | 3 SISTEMAS'.
           DISPLAY 'NOM-DEP:'           ACCEPT NOM-DEP.

           EVALUATE TRUE
               WHEN NOM-DEP EQUAL 1 MOVE 'LIMPIEZA' TO NOM-DEP
               WHEN NOM-DEP EQUAL 2 MOVE 'LIMPIEZA' TO NOM-DEP
               WHEN NOM-DEP EQUAL 3 MOVE 'LIMPIEZA' TO NOM-DEP
               WHEN OTHER DISPLAY 'NO EXISTE DEPARTAMENTO'
               PERFORM 101-DEP.

           DISPLAY 'S-TAB'             ACCEPT S-TAB.

           DISPLAY 'INGRESA EL SUELDO O ELIGE UNA OPCION'.
           DISPLAY '1 $10,000 | 2 $20,000 | 3 $30,000'.
           DISPLAY 'S-MEN'             ACCEPT S-MEN.

           EVALUATE TRUE
               WHEN S-MEN EQUAL 1 MOVE 10000 TO S-MEN
               WHEN S-MEN EQUAL 2 MOVE 20000 TO S-MEN
               WHEN S-MEN EQUAL 3 MOVE 10000 TO S-MEN.

           102-FING.
           DISPLAY 'INGRESA FECHA DE INGRESO MAYOR DE 2019'.
           DISPLAY 'F-ING:'.             ACCEPT F-ING.

           IF F-ING LESS 20190101 DISPLAY 'FECHA MENOR A 2019'
               PERFORM  102-FING.
               IF F-ING GREATER 20190101 DISPLAY 'FECHA REGISTRADA'
           WRITE REG-EMPL DISPLAY 'ESCRITO'.

               DISPLAY 'DESEAS AGREGAR OTRO REGISTRO? S/N'.
               ACCEPT WS-RES
               IF WS-RES EQUAL 'N'
                   MOVE 1 TO WS-FLAG.
