       IDENTIFICATION DIVISION.
       PROGRAM-ID. SimpleOps.
       AUTHOR. Piombacciaio.
    
       ENVIRONMENT DIVISION.
    
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  Num1             PIC 9(5) VALUE 0. *> Value initialized to 0
       01  Num2             PIC 9(5).
       01  Sumv             PIC 9(5).
       01  Mult             PIC 9(5).
       01  Diff             PIC 9(5).
       01  Divv             PIC 9(5)V99. *> V99 indicates two decimal places

       PROCEDURE DIVISION.
           DISPLAY "Please enter the first number: "
           ACCEPT Num1.
           DISPLAY "Please enter the second number: "
           ACCEPT Num2.
           ADD Num1 TO Num2 GIVING Sumv.
           DISPLAY "The sum is: " Sumv.
           MULTIPLY Num1 BY Num2 GIVING Mult.
           DISPLAY "The product is: " Mult.
           SUBTRACT Num2 FROM Num1 GIVING Diff.
           DISPLAY "The difference is: " Diff.
           DIVIDE Num1 BY Num2 GIVING Divv.
           DISPLAY "The quotient is: " Divv.
           STOP RUN.
