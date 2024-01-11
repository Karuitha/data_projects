/*Part 1 - Results of Acquistion Choice Model*/

/*Importing the data from Excel file*/

PROC IMPORT OUT= WORK.auto1 DATAFILE= "C:/Users/Ritika Khandelwal/Desktop/RA/TKL Case/TKL.xlsx" 
            DBMS=xlsx REPLACE;
run;

/*Running Regression*/

proc logistic outest = work.ritika;
      model Acquired = Price Warranty Delivery_Time Sales_Support Industry_Group_Indus Industry_Group_Cons Firm_Size Buying_Center;
run;

/*Part 2 - Results of Expansion Choice Model*/

/*Importing the data from Excel file*/

PROC IMPORT OUT= WORK.auto1 DATAFILE= "C:/Users/Ritika Khandelwal/Desktop/RA/TKL Case/TKL.xlsx"
            DBMS=xlsx REPLACE;
			sheet = 'choice';
run;

/*Running Regression*/

proc logistic outest = work.ritika;
      model Expansion = Price Warranty Delivery_Time Sales_Support Industry_Group_Indus Industry_Group_Cons Firm_Size Buying_Center;
run;
