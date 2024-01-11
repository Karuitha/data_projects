The dataset captures the age, sleep, and body mass index of a sample of respondents in the UK. 

The data consists of 4 variables. The first variable is the ID, an identifier of the respondents. The second is age in years that has been discretized by rounding off to the nearest whole number. similarly, the third variable, sleep in hours, has been discretized into a whole number. The last variable is the body mass index, the ratio of weight in kilograms to height in metres squared. 

Further there are 153 observations. The following two tables confirm the structure of the data. 

To validate the data, I check for three issues.

- Missing values. 
- Unreasonable observations and extreme values, for instance people sleeping for more than 24 hours. 
- Duplicates or repeated values. 

I have written a code to check for each of these scenarios (see code). 

The major discrepancy is that of duplicate observations. 