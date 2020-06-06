      *Program: LAB 10-1
      *Author: Seth Tealbey
      *Date: 10/24/2019
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. RPT8000.
      *
       ENVIRONMENT DIVISION.
      *
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
      *
           SELECT CUSTMAST ASSIGN TO "CUSTMAST.DAT".
           SELECT SALESRPT ASSIGN TO "SALESRPT.PRN".
           SELECT BRCHMAST ASSIGN TO "BRCHMAST.DAT".
      *
       DATA DIVISION.
      *
       FILE SECTION.
      *
       FD  CUSTMAST.
      *
       01  CUSTOMER-MASTER-RECORD.
           05  CM-BRANCH-NUMBER        PIC 9(2).
           05  CM-SALESREP-NUMBER      PIC 9(2).
           05  CM-CUSTOMER-NUMBER      PIC 9(5).
           05  CM-CUSTOMER-NAME        PIC X(20).
           05  CM-SALES-THIS-YTD       PIC S9(5)V9(2).
           05  CM-SALES-LAST-YTD       PIC S9(5)V9(2).

      *
       FD  BRCHMAST.
      *
       01  BRANCH-MASTER-RECORD.
           05  BM-BRANCH-NUMBER    PIC 9(2).
           05  BM-BRANCH-NAME      PIC X(18).
      *
       FD  SALESRPT.
      *
       01  PRINT-AREA      PIC X(132).
      *
       WORKING-STORAGE SECTION.
      *
       01  SWITCHES.
           05  CUSTMAST-EOF-SWITCH     PIC X   VALUE "N".
             88  CUSTMAST-EOF  value "Y".
           05  FIRST-RECORD-SWITCH     PIC X   VALUE "Y".
             88 FIRST-RECORD  value "Y".
           05  BRANCH-TABLE-EOF-SWITCH PIC X   VALUE "N".
             88 BRANCH-EOF    value "Y".
           05  BRANCH-FOUND-SWITCH     PIC X   VALUE "N".
             88 BRANCH-FOUND  value "Y".
      *
       01  CONTROL-FIELDS.
           05  OLD-BRANCH-NUMBER       PIC 99.
           05  OLD-BRANCH-NAME         PIC X(18).
      *
       01  BRANCH-TABLE.
           05  BRANCH-GROUP    OCCURS 7 TIMES.
               10  BRANCH-NUMBER   PIC 9(2).
               10  BRANCH-NAME     PIC X(18).
      *
       01  SUBSCRIPTS              BINARY.
           05  BRANCH-TABLE-SUB    PIC S99.
      *
       01  CALCULATED-FIELDS.
           05  CHANGE-AMOUNT           PIC S9(7)V99.
      *
       01  PRINT-FIELDS.
           05  PAGE-COUNT      PIC S9(3)   VALUE ZERO.
           05  LINES-ON-PAGE   PIC S9(3)   VALUE +55.
           05  LINE-COUNT      PIC S9(3)   VALUE +99.
           05  SPACE-CONTROL   PIC S9.
      *
       01  TOTAL-FIELDS.
           05  BRANCH-TOTAL-THIS-YTD   PIC S9(6)V99    VALUE ZERO.
           05  BRANCH-TOTAL-LAST-YTD   PIC S9(6)V99    VALUE ZERO.
           05  GRAND-TOTAL-THIS-YTD    PIC S9(7)V99    VALUE ZERO.
           05  GRAND-TOTAL-LAST-YTD    PIC S9(7)V99    VALUE ZERO.
      *
       01  CURRENT-DATE-AND-TIME.
           05  CD-YEAR         PIC 9999.
           05  CD-MONTH        PIC 99.
           05  CD-DAY          PIC 99.
           05  CD-HOURS        PIC 99.
           05  CD-MINUTES      PIC 99.
           05  FILLER          PIC X(9).
      *
       01  HEADING-LINE-1.
           05  FILLER          PIC X(7)    VALUE "DATE:  ".
           05  HL1-MONTH       PIC 9(2).
           05  FILLER          PIC X(1)    VALUE "/".
           05  HL1-DAY         PIC 9(2).
           05  FILLER          PIC X(1)    VALUE "/".
           05  HL1-YEAR        PIC 9(4).
           05  FILLER          PIC X(15)   VALUE SPACE.
           05  FILLER          PIC X(20)   VALUE "YEAR-TO-DATE SALES R".
           05  FILLER          PIC X(20)   VALUE "EPORT               ".
           05  FILLER          PIC X(12)   VALUE "      PAGE: ".
           05  Hl1-PAGE-NUMBER PIC ZZZ9.
           05  FILLER          PIC X(44)   VALUE SPACE.
      *
       01  HEADING-LINE-2.
           05  FILLER          PIC X(7)    VALUE "TIME:  ".
           05  HL2-HOURS       PIC 9(2).
           05  FILLER          PIC X(1)    VALUE ":".
           05  HL2-MINUTES     PIC 9(2).
           05  FILLER          PIC X(66)   VALUE SPACE.
           05  FILLER          PIC X(10)   VALUE "RPT3000".
           05  FILLER          PIC X(44)   VALUE SPACE.
      *
       01  HEADING-LINE-3.
           05  FILLER      PIC X(25)  VALUE "BRCH                     ".
           05  FILLER      PIC X(20)   VALUE "CUST                ".
           05  FILLER      PIC X(20)   VALUE "            SALES   ".
           05  FILLER      PIC X(20)   VALUE "      SALES         ".
           05  FILLER      PIC X(20)   VALUE "CHANGE     CHANGE   ".
           05  FILLER      PIC X(44)   VALUE SPACE.
      *
       01  HEADING-LINE-4.
           05  FILLER      PIC X(5)    VALUE "NUM  ".
           05  FILLER      PIC X(20)   VALUE "BRANCH NAME         ".
           05  FILLER      PIC X(20)   VALUE "NUM    CUSTOMER NAME".
           05  FILLER      PIC X(20)   VALUE "           THIS YTD ".
           05  FILLER      PIC X(20)   VALUE "     LAST YTD       ".
           05  FILLER      PIC X(20)   VALUE "AMOUNT    PERCENT   ".
           05  FILLER      PIC X(44)   VALUE SPACE.
      *
       01  CUSTOMER-LINE.
           05  FILLER               PIC X(1)       VALUE SPACE.
           05  CL-BRANCH-NUMBER     PIC X(2).
           05  FILLER               PIC X(3)       VALUE SPACE.
           05  CL-BRANCH-NAME       PIC X(18).
           05  FILLER               PIC X(1)       VALUE SPACE.
           05  CL-CUSTOMER-NUMBER   PIC 9(5).
           05  FILLER               PIC X(2)       VALUE SPACE.
           05  CL-CUSTOMER-NAME     PIC X(20).
           05  FILLER               PIC X(3)       VALUE SPACE.
           05  CL-SALES-THIS-YTD    PIC ZZ,ZZ9.99-.
           05  FILLER               PIC X(4)       VALUE SPACE.
           05  CL-SALES-LAST-YTD    PIC ZZ,ZZ9.99-.
           05  FILLER               PIC X(4)       VALUE SPACE.
           05  CL-CHANGE-AMOUNT     PIC ZZ,ZZ9.99-.
           05  FILLER               PIC X(3)       VALUE SPACE.
           05  CL-CHANGE-PERCENT    PIC ZZ9.9-.
           05  FILLER               PIC X(47)      VALUE SPACE.
      *
       01  BRANCH-TOTAL-LINE.
           05  FILLER               PIC X(40)      VALUE SPACE.
           05  FILLER               PIC X(14)      VALUE "BRANCH TOTAL".
           05  BTL-SALES-THIS-YTD   PIC ZZZ,ZZ9.99-.
           05  FILLER               PIC X(3)       VALUE SPACE.
           05  BTL-SALES-LAST-YTD   PIC ZZZ,ZZ9.99-.
           05  FILLER               PIC X(3)       VALUE SPACE.
           05  BTL-CHANGE-AMOUNT    PIC ZZZ,ZZ9.99-.
           05  FILLER               PIC X(3)       VALUE SPACE.
           05  BTL-CHANGE-PERCENT   PIC ZZ9.9-.
           05  FILLER               PIC X(47)      VALUE " *".
      *
       01  GRAND-TOTAL-LINE.
           05  FILLER               PIC X(40)      VALUE SPACE.
           05  FILLER               PIC X(12)      VALUE "GRAND TOTAL".
           05  GTL-SALES-THIS-YTD   PIC Z,ZZZ,ZZ9.99-.
           05  FILLER               PIC X(1)       VALUE SPACE.
           05  GTL-SALES-LAST-YTD   PIC Z,ZZZ,ZZ9.99-.
           05  FILLER               PIC X(1)       VALUE SPACE.
           05  GTL-CHANGE-AMOUNT    PIC Z,ZZZ,ZZ9.99-.
           05  FILLER               PIC X(3)       VALUE SPACE.
           05  GTL-CHANGE-PERCENT   PIC ZZ9.9-.
           05  FILLER               PIC X(47)      VALUE " **".
      *
       PROCEDURE DIVISION.
      *
       000-PREPARE-SALES-REPORT.
      *
           INITIALIZE BRANCH-TABLE.
           OPEN INPUT  CUSTMAST
                INPUT  BRCHMAST
                OUTPUT SALESRPT.
           PERFORM 100-FORMAT-REPORT-HEADING.
           PERFORM 200-LOAD-BRANCH-TABLE
               VARYING BRANCH-TABLE-SUB FROM 1 BY 1
               UNTIL BRANCH-TABLE-EOF-SWITCH = "Y"
                  OR BRANCH-TABLE-SUB > 7.
           PERFORM 300-PREPARE-SALES-LINES
               UNTIL CUSTMAST-EOF.
           PERFORM 500-PRINT-GRAND-TOTALS.
           CLOSE CUSTMAST
                 BRCHMAST
                 SALESRPT.
           STOP RUN.
      *
       100-FORMAT-REPORT-HEADING.
      *
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.
           MOVE CD-MONTH   TO HL1-MONTH.
           MOVE CD-DAY     TO HL1-DAY.
           MOVE CD-YEAR    TO HL1-YEAR.
           MOVE CD-HOURS   TO HL2-HOURS.
           MOVE CD-MINUTES TO HL2-MINUTES.
      *
       200-LOAD-BRANCH-TABLE.
      *
           READ BRCHMAST
                AT END
                    SET BRANCH-EOF TO TRUE.
           IF NOT BRANCH-EOF
                MOVE BM-BRANCH-NUMBER
                    TO BRANCH-NUMBER (BRANCH-TABLE-SUB)
                MOVE BM-BRANCH-NAME
                    TO BRANCH-NAME (BRANCH-TABLE-SUB)
           END-IF.
      *
       250-SEARCH-BRANCH-TABLE-RECORD.
      *
           IF BRANCH-NUMBER (BRANCH-TABLE-SUB) = CM-BRANCH-NUMBER
               MOVE "Y" TO BRANCH-FOUND-SWITCH.
      *
       300-PREPARE-SALES-LINES.
      *
           PERFORM 310-READ-CUSTOMER-RECORD.
           EVALUATE TRUE
             when CUSTMAST-EOF
               PERFORM 360-PRINT-BRANCH-LINE

             WHEN FIRST-RECORD
                   PERFORM 320-PRINT-CUSTOMER-LINE
                   MOVE "N" TO FIRST-RECORD-SWITCH
                   MOVE CM-BRANCH-NUMBER TO OLD-BRANCH-NUMBER
                   MOVE BM-BRANCH-NAME TO OLD-BRANCH-NAME
             when CM-BRANCH-NUMBER > OLD-BRANCH-NUMBER
                       PERFORM 360-PRINT-BRANCH-LINE
                       PERFORM 320-PRINT-CUSTOMER-LINE
                       MOVE CM-BRANCH-NUMBER TO OLD-BRANCH-NUMBER
                       MOVE BM-BRANCH-NAME TO OLD-BRANCH-NAME
              When other
                       PERFORM 320-PRINT-CUSTOMER-LINE
           end-evaluate.
      *
       310-READ-CUSTOMER-RECORD.
      *
           READ CUSTMAST
               AT END
                   set CUSTMAST-EOF to true.
      *
       320-PRINT-CUSTOMER-LINE.
      *
           IF LINE-COUNT > LINES-ON-PAGE
               PERFORM 330-PRINT-HEADING-LINES.
           IF FIRST-RECORD
               MOVE CM-BRANCH-NUMBER TO CL-BRANCH-NUMBER
               MOVE "N" TO BRANCH-FOUND-SWITCH
               PERFORM 250-SEARCH-BRANCH-TABLE-RECORD
                   WITH TEST AFTER
                   VARYING BRANCH-TABLE-SUB FROM 1 BY 1
                       UNTIL BRANCH-FOUND OR
                       BRANCH-TABLE-SUB = 7
              IF BRANCH-FOUND
                  MOVE BRANCH-NAME (BRANCH-TABLE-SUB) TO CL-BRANCH-NAME
              ELSE
                  MOVE "NO BRANCH RECORD" TO CL-BRANCH-NAME
              END-IF
           END-IF.

           IF NOT FIRST-RECORD
               IF CM-BRANCH-NUMBER > OLD-BRANCH-NUMBER
                   MOVE CM-BRANCH-NUMBER TO CL-BRANCH-NUMBER
                   MOVE "N" TO BRANCH-FOUND-SWITCH
                   PERFORM 250-SEARCH-BRANCH-TABLE-RECORD
                   WITH TEST AFTER
                   VARYING BRANCH-TABLE-SUB FROM 1 BY 1
                       UNTIL BRANCH-FOUND OR
                       BRANCH-TABLE-SUB = 7
                  IF BRANCH-FOUND
                   MOVE BRANCH-NAME (BRANCH-TABLE-SUB) TO CL-BRANCH-NAME
                  ELSE
                      MOVE "NO BRANCH RECORD" TO CL-BRANCH-NAME
                  END-IF
               ELSE
                   MOVE SPACE TO CL-BRANCH-NUMBER
                   MOVE SPACE TO CL-BRANCH-NAME
           END-IF.

           MOVE CM-CUSTOMER-NUMBER   TO CL-CUSTOMER-NUMBER.
           MOVE CM-CUSTOMER-NAME     TO CL-CUSTOMER-NAME.
           MOVE CM-SALES-THIS-YTD    TO CL-SALES-THIS-YTD.
           MOVE CM-SALES-LAST-YTD    TO CL-SALES-LAST-YTD.
           COMPUTE CHANGE-AMOUNT =
               CM-SALES-THIS-YTD - CM-SALES-LAST-YTD.
           MOVE CHANGE-AMOUNT TO CL-CHANGE-AMOUNT.
           IF CM-SALES-LAST-YTD = ZERO
               MOVE 999.9 TO CL-CHANGE-PERCENT
           ELSE
               COMPUTE CL-CHANGE-PERCENT ROUNDED =
                   CHANGE-AMOUNT * 100 / CM-SALES-LAST-YTD
                   ON SIZE ERROR
                       MOVE 999.9 TO CL-CHANGE-PERCENT.
           MOVE CUSTOMER-LINE TO PRINT-AREA.
           PERFORM 350-WRITE-REPORT-LINE.
           MOVE 1 TO SPACE-CONTROL.
           ADD CM-SALES-THIS-YTD TO BRANCH-TOTAL-THIS-YTD.
           ADD CM-SALES-LAST-YTD TO BRANCH-TOTAL-LAST-YTD.
      *
       330-PRINT-HEADING-LINES.
      *
           ADD 1 TO PAGE-COUNT.
           MOVE PAGE-COUNT     TO HL1-PAGE-NUMBER.
           MOVE HEADING-LINE-1 TO PRINT-AREA.
           PERFORM 340-WRITE-PAGE-TOP-LINE.
           MOVE HEADING-LINE-2 TO PRINT-AREA.
           MOVE 1 TO SPACE-CONTROL.
           PERFORM 350-WRITE-REPORT-LINE.
           MOVE HEADING-LINE-3 TO PRINT-AREA.
           MOVE 2 TO SPACE-CONTROL.
           PERFORM 350-WRITE-REPORT-LINE.
           MOVE HEADING-LINE-4 TO PRINT-AREA.
           MOVE 1 TO SPACE-CONTROL.
           PERFORM 350-WRITE-REPORT-LINE.
           MOVE 2 TO SPACE-CONTROL.
      *
       340-WRITE-PAGE-TOP-LINE.
      *
           WRITE PRINT-AREA AFTER ADVANCING PAGE.
           MOVE 1 TO LINE-COUNT.
      *
       350-WRITE-REPORT-LINE.
      *
           WRITE PRINT-AREA AFTER ADVANCING SPACE-CONTROL LINES.
           ADD SPACE-CONTROL TO LINE-COUNT.
      *
       360-PRINT-BRANCH-LINE.
      *
           MOVE BRANCH-TOTAL-THIS-YTD TO BTL-SALES-THIS-YTD.
           MOVE BRANCH-TOTAL-LAST-YTD TO BTL-SALES-LAST-YTD.
           COMPUTE CHANGE-AMOUNT =
               BRANCH-TOTAL-THIS-YTD - BRANCH-TOTAL-LAST-YTD.
           MOVE CHANGE-AMOUNT TO BTL-CHANGE-AMOUNT.
           IF BRANCH-TOTAL-LAST-YTD = ZERO
               MOVE 999.9 TO BTL-CHANGE-PERCENT
           ELSE
               COMPUTE BTL-CHANGE-PERCENT ROUNDED =
                   CHANGE-AMOUNT * 100 / BRANCH-TOTAL-LAST-YTD
                   ON SIZE ERROR
                       MOVE 999.9 TO BTL-CHANGE-PERCENT.
           MOVE BRANCH-TOTAL-LINE TO PRINT-AREA.
           MOVE 1 TO SPACE-CONTROL.
           PERFORM 350-WRITE-REPORT-LINE.
           MOVE 2 TO SPACE-CONTROL.
           ADD BRANCH-TOTAL-THIS-YTD TO GRAND-TOTAL-THIS-YTD.
           ADD BRANCH-TOTAL-LAST-YTD TO GRAND-TOTAL-LAST-YTD.
           MOVE ZERO TO BRANCH-TOTAL-THIS-YTD.
           MOVE ZERO TO BRANCH-TOTAL-LAST-YTD.
      *
       500-PRINT-GRAND-TOTALS.
      *
           MOVE GRAND-TOTAL-THIS-YTD TO GTL-SALES-THIS-YTD.
           MOVE GRAND-TOTAL-LAST-YTD TO GTL-SALES-LAST-YTD.
           COMPUTE CHANGE-AMOUNT =
               GRAND-TOTAL-THIS-YTD - GRAND-TOTAL-LAST-YTD.
           MOVE CHANGE-AMOUNT TO GTL-CHANGE-AMOUNT.
           IF GRAND-TOTAL-LAST-YTD = ZERO
               MOVE 999.9 TO GTL-CHANGE-PERCENT
           ELSE
               COMPUTE GTL-CHANGE-PERCENT ROUNDED =
                   CHANGE-AMOUNT * 100 / GRAND-TOTAL-LAST-YTD
                   ON SIZE ERROR
                       MOVE 999.9 TO GTL-CHANGE-PERCENT.
           MOVE GRAND-TOTAL-LINE TO PRINT-AREA.
           MOVE 2 TO SPACE-CONTROL.
           PERFORM 350-WRITE-REPORT-LINE.
