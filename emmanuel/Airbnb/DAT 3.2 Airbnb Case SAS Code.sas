/*Part 1 - Results of Acquistion Choice Model*/

/*Importing the data from Excel file*/

PROC IMPORT OUT= WORK.auto1 DATAFILE= "C:/Users/Ritika Khandelwal/Desktop/RA/Airbnb/Data.xlsx" 
            DBMS=xlsx REPLACE;
run;

/*Running Regression*/

proc logistic outest = work.ritika;
      model Choice = Email_25 Email_Taxi Gmail yahoo Edu AlaskaFF Add_Ore Add_Eug Age Tickets RoundTrip;
run;

/*Part 2 - Results of Expansion Choice Model*/

/*Importing the data from Excel file*/

PROC IMPORT OUT= WORK.auto1 DATAFILE= "C:/Users/Ritika Khandelwal/Desktop/RA/Airbnb/Data.xlsx"
            DBMS=xlsx REPLACE;
			sheet = 'Host';
run;

/*Running Regression*/

proc logistic outest = work.ritika;
      model Choice = Length Type_Hm Guests ZestimateK ListPrice Rating Rev_AbB Loc_1 Loc_2 Loc_3 Loc_4 Loc_5;
run;
