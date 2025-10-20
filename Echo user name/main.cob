       IDENTIFICATION DIVISION.
       PROGRAM-ID. EchoUserName.
       AUTHOR. Piombacciaio.
    
       ENVIRONMENT DIVISION.
    
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  User-Name        PIC X(50).
    
       PROCEDURE DIVISION.
           DISPLAY "Please enter your name: ".
           ACCEPT User-Name.
           DISPLAY "Hello, " User-Name "!".
           STOP RUN.
