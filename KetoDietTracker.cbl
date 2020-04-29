      ******************************************************************
      * Author: Leighton Waters
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. KETO-TRACK.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RecipeFile ASSIGN TO "Recipe.DAT"
                     ORGANIZATION IS LINE SEQUENTIAL.
           SELECT HoldFile ASSIGN TO "Hold.DAT"
                     ORGANIZATION IS LINE SEQUENTIAL.
           SELECT WordsFile ASSIGN TO "Words.DAT"
                     ORGANIZATION IS LINE SEQUENTIAL.
           SELECT MealFile ASSIGN TO "Meal.DAT"
                     ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ShopFile ASSIGN TO "Week.DAT"
                     ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
           FD  RecipeFile.
           01  RecInf.
               02 recnum PIC 999.
               02 recname PIC X(20).
               02 ingreds PIC X(120).

           FD  HoldFile.
           01  HoldInf.
               02 Hingreds PIC X(120).

           FD  WordsFile.
           01  wordsInf.
               02 indivingred PIC X(15).

           FD  MealFile.
           01  MealInf.
               02 Mealday PIC 9(7).
               02 Mealdayfor PIC 9(8).
               02 MealTime PIC X(9).
                   88 BLD VALUES "Breakfast", "Lunch", "Dinner".
               02 Mealname PIC X(20).

           FD  ShopFile.
           01  ShopInf.
               02 quantity PIC 99.
               02 ingredname PIC X(15).
       WORKING-STORAGE SECTION.
           01 RecTemp.
               02 recnumt PIC 999.
               02 recnamet PIC X(20).
               02 ingredst PIC X(120).

           01  HoldTemp.
               02 Hingredst PIC X(120).

           01  wordsTemp.
               02 indivingredt PIC X(15).
           01  wordsTemp2.
               02 indivingredt2 PIC X(15).    

           01 MealTemp.
               02 Mealdayt PIC 9(7).
               02 Mealdayfort.
                   05 mealyear PIC 9(04).
                   05 mealmonth PIC 9(02).
                   05 mealdays PIC 9(02).
               02 MealTimet PIC X(9).
                   88 BLDt VALUES "Breakfast", "Lunch", "Dinner".
               02 Mealnamet PIC X(20).

           01  ShopTemp.
               02 quantityt PIC 99.
               02 ingrednamet PIC X(15).

           01 Parameters.
               02 choose PIC X.

               02 Mealnum PIC 999.
               02 Mealdate PIC 9(8).

               02 WS-EOF PIC A.
               02 WS-EOF2 PIC A.

               02 WS-COUNT PIC 9(4).
               02 WS-WORD PIC X(15).
               02 WS-REST PIC X(105).
               02 WS-LEN PIC 999.

               02 WS-DATE-DIF PIC 9(7).
               02 WS-DATE PIC 9(7).
               02 WS-DATE-HOLD PIC 9(8).
               02 WS-CURRENT-DATE.
                   05  WS-CURRENT-YEAR         PIC 9(04).
                   05  WS-CURRENT-MONTH        PIC 9(02).
                   05  WS-CURRENT-DAY          PIC 9(02).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           begin.
            PERFORM date-get.

            DISPLAY "--------------------------------------------------"
            DISPLAY "View Recipes(1)| View Shopping List(2)|",
            " View Meals this Week(3)| Quit(4)"
            ACCEPT choose.

            EVALUATE choose
               WHEN "1" PERFORM viewrec
               WHEN "2" PERFORM viewshop
               WHEN "3" PERFORM viewmeal
               WHEN "4" DISPLAY "Thank You Have a good day."
               WHEN OTHER DISPLAY "Invalid Try Again", PERFORM begin.
            STOP RUN.

           viewrec.
            DISPLAY "--------------------------------------------------"
            DISPLAY "Recipes:"

            OPEN INPUT RecipeFile.
               PERFORM UNTIL WS-EOF = 'Y'
                   READ RecipeFile INTO RecTemp
                       AT END MOVE 'Y' TO WS-EOF
                       NOT AT END DISPLAY RecTemp
                   END-READ
               END-PERFORM
            CLOSE RecipeFile.

            MOVE 'Z' TO WS-EOF

            DISPLAY "Add Recipe(1)| Return to start(2)"
            ACCEPT choose
            EVALUATE choose
               WHEN "1" PERFORM addrec
               WHEN "2" PERFORM begin
               WHEN OTHER DISPLAY "Invalid Try Again", PERFORM viewrec.
            STOP RUN.

           addrec.
            DISPLAY " "
            DISPLAY "--------------------------------------------------"
            DISPLAY "Please Enter Recipe Name: "
            ACCEPT recnamet
            DISPLAY "Please Enter Ingrediants seperated by , : "
            ACCEPT ingredst


            ADD 1 TO recnumt

            OPEN EXTEND RecipeFile.
            WRITE RecInf FROM RecTemp
            END-WRITE.
            CLOSE RecipeFile.

            PERFORM viewrec.

           viewmeal.
            DISPLAY " "
            DISPLAY "--------------------------------------------------"


            OPEN INPUT MealFile.
               PERFORM UNTIL WS-EOF = 'Y'
                   READ MealFile INTO MealTemp
                       AT END MOVE 'Y' TO WS-EOF
                       NOT AT END
                       SUBTRACT Mealdayt FROM WS-DATE GIVING WS-DATE-DIF
                       IF WS-DATE-DIF >=0 AND <8
                               THEN DISPLAY mealmonth, '/',mealdays,'/',
                               mealyear,' ',MealTimet, ' ', Mealnamet
                   END-READ
               END-PERFORM
            CLOSE MealFile.

            MOVE 'Z' TO WS-EOF

            DISPLAY "Add Meal(1) |Return to start(2)"
            ACCEPT choose
            EVALUATE choose
               WHEN "1" PERFORM addmeal
               WHEN "2" PERFORM begin
               WHEN OTHER DISPLAY "Invalid Try Again", PERFORM viewmeal.


           addmeal.
            DISPLAY " "
            DISPLAY "--------------------------------------------------"
            DISPLAY "Please Enter Date (yyyymmdd): "
            ACCEPT Mealdate
            MOVE Mealdate TO Mealdayfort
            MOVE FUNCTION INTEGER-OF-DATE(Mealdate) TO Mealdayt
            DISPLAY "Please Enter Breakfast,Lunch,or Dinner: "
               ACCEPT MealTimet
            PERFORM UNTIL BLDt
               DISPLAY "Please Enter Breakfast,Lunch,or Dinner: "
               ACCEPT MealTimet
            END-PERFORM
            PERFORM UNTIL WS-EOF2 = 'Y'
               DISPLAY "Please Enter Valid Recipe Number: "
               ACCEPT Mealnum

               OPEN INPUT RecipeFile
               PERFORM UNTIL WS-EOF = 'Y'
               READ RecipeFile INTO RecTemp
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END IF recnumt = Mealnum THEN
                   MOVE recnamet TO Mealnamet, MOVE 'Y' TO WS-EOF2
               END-READ
               END-PERFORM
               CLOSE RecipeFile

            END-PERFORM

            MOVE 'Z' to WS-EOF
            MOVE 'Z' to WS-EOF2

            OPEN EXTEND MealFile.
            WRITE MealInf FROM MealTemp
            END-WRITE.
            CLOSE MealFile.

            PERFORM viewmeal.

           viewshop.
            OPEN OUTPUT HoldFile
            OPEN INPUT MealFile
               PERFORM UNTIL WS-EOF = 'Y'
                   READ MealFile INTO MealTemp
                       AT END MOVE 'Y' TO WS-EOF
                       NOT AT END
                       MOVE 'Z' to WS-EOF2
                       SUBTRACT Mealdayt FROM WS-DATE GIVING WS-DATE-DIF
                       IF WS-DATE-DIF >=0 AND <8 THEN
                           OPEN INPUT RecipeFile
                           PERFORM UNTIL WS-EOF2 = 'Y'
                           READ RecipeFile INTO RecTemp
                           NOT AT END IF Mealnamet EQUAL recnamet THEN
                               MOVE ingredst TO Hingredst
                               WRITE HoldInf FROM HoldTemp
                               END-WRITE
                           AT END MOVE 'Y' TO WS-EOF2
                           END-READ
                           END-PERFORM
                           CLOSE RecipeFile
                   END-READ
               END-PERFORM
            CLOSE MealFile
            CLOSE HoldFile

            MOVE 'Z' to WS-EOF

            OPEN INPUT HoldFile
            OPEN OUTPUT WordsFile
            PERFORM UNTIL WS-EOF = 'Y'
               READ HoldFile INTO HoldTemp
               NOT AT END PERFORM UNTIL HoldTemp EQUAL SPACES
                   UNSTRING HoldTemp DELIMITED BY "," INTO
                   WS-WORD
                   DISPLAY WS-WORD
                   MOVE 0 TO WS-COUNT
                   INSPECT WS-WORD REPLACING ALL LOW-VALUE BY SPACE
                   INSPECT FUNCTION REVERSE(WS-WORD)
                   TALLYING WS-COUNT FOR LEADING SPACE
                   COMPUTE WS-LEN = LENGTH OF WS-WORD - WS-COUNT
                   ADD 2 TO WS-LEN
                   MOVE WS-WORD TO indivingredt
                   WRITE wordsInf FROM wordsTemp
                   MOVE HoldTemp(WS-LEN:LENGTH OF HoldTemp - WS-LEN) 
                   TO WS-REST
                   MOVE WS-REST TO HoldTemp
               END-PERFORM
               AT END MOVE 'Y' TO WS-EOF
               END-READ
            END-PERFORM
            CLOSE HoldFile
            CLOSE WordsFile

            MOVE 'Z' to WS-EOF.



           date-get.
            MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURRENT-DATE.
            MOVE WS-CURRENT-DATE TO WS-DATE-HOLD.
            MOVE FUNCTION INTEGER-OF-DATE(WS-DATE-HOLD),
            TO WS-DATE.



       END PROGRAM KETO-TRACK.
