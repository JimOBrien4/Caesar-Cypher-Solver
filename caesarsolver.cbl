      ******************************************************************
      * Author: James O'Brien
      * Date: 9/27/18
      * Purpose: Caeser Cypher Solver
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAESAR-SOLVER.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *Mimicking INDD and OUTDD symbolics from JCL
       SELECT DATAIN ASSIGN "indd.txt" ORGANIZATION IS
       LINE SEQUENTIAL.
       SELECT DATAOUT ASSIGN "outdd.txt" ORGANIZATION IS
       LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD DATAIN RECORDING MODE F LABEL RECORDS ARE OMITTED.
       01 USER-INPUT.
        02 MODE-IN PIC 9(1).
      *1 for encrypt, 2 for decrypt, 3 for solve
        02 AMOUNT-IN PIC 9(2).
        02 PHRASE-IN PIC X(77).
       FD DATAOUT RECORDING MODE F LABEL RECORDS ARE OMITTED.
       01 PRINTOUT PIC X(80).
       WORKING-STORAGE SECTION.
       01 EOF PIC 9 VALUE 0.
       01 PHRASE-OUT PIC X(80).
       01 LINEBREAK PIC X(80) VALUE "-".
       PROCEDURE DIVISION.
       OPEN INPUT DATAIN OUTPUT DATAOUT.
       PERFORM READ-RECORD.
       PERFORM UNTIL EOF = 1
        WRITE PRINTOUT FROM LINEBREAK
        MOVE FUNCTION UPPER-CASE(PHRASE-IN) TO PHRASE-IN
        EVALUATE MODE-IN
         WHEN "1" PERFORM ENCRYPT
         WHEN "2" PERFORM DECRYPT
         WHEN "3" PERFORM SOLVE
        END-EVALUATE
        PERFORM READ-RECORD
       END-PERFORM.
       CLOSE DATAIN, DATAOUT.
       STOP RUN.

       CAESAR-SHIFT.
        INSPECT PHRASE-IN REPLACING
         ALL "A" BY "B"
         ALL "B" BY "C"
         ALL "C" BY "D"
         ALL "D" BY "E"
         ALL "E" BY "F"
         ALL "F" BY "G"
         ALL "G" BY "H"
         ALL "H" BY "I"
         ALL "I" BY "J"
         ALL "J" BY "K"
         ALL "K" BY "L"
         ALL "L" BY "M"
         ALL "M" BY "N"
         ALL "N" BY "O"
         ALL "O" BY "P"
         ALL "P" BY "Q"
         ALL "Q" BY "R"
         ALL "R" BY "S"
         ALL "S" BY "T"
         ALL "T" BY "U"
         ALL "U" BY "V"
         ALL "V" BY "W"
         ALL "W" BY "X"
         ALL "X" BY "Y"
         ALL "Y" BY "Z"
         ALL "Z" BY "A".

       ENCRYPT.
        MOVE FUNCTION MOD(AMOUNT-IN,26) TO AMOUNT-IN.
        PERFORM AMOUNT-IN TIMES
         PERFORM CAESAR-SHIFT
        END-PERFORM.
        MOVE PHRASE-IN TO PHRASE-OUT.
        WRITE PRINTOUT FROM PHRASE-OUT.

       DECRYPT.
        MOVE FUNCTION MOD(AMOUNT-IN,26) TO AMOUNT-IN.
        COMPUTE AMOUNT-IN = 26 - AMOUNT-IN
        PERFORM AMOUNT-IN TIMES
         PERFORM CAESAR-SHIFT
        END-PERFORM.
        MOVE PHRASE-IN TO PHRASE-OUT.
        WRITE PRINTOUT FROM PHRASE-OUT.

       SOLVE.
        PERFORM AMOUNT-IN TIMES
         PERFORM CAESAR-SHIFT
         MOVE PHRASE-IN TO PHRASE-OUT
         WRITE PRINTOUT FROM PHRASE-OUT
        END-PERFORM.

       READ-RECORD.
        READ DATAIN AT END MOVE 1 TO EOF
        END-READ.
       END PROGRAM CAESAR-SOLVER

