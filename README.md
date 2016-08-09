Coursera Computing in Data Analysis Assignment 2 Part 5-7 Week 4

Background

The data for this assignment come from the Hospital Compare web site (http://hospitalcompare.hhs.gov) run by the U.S. Department 
of Health and Human Services. The purpose of the web site is to provide data and information about the quality of care at over
FOUR THOUSAND (4,000) Medicare-certied hospitals in the U.S. This dataset essentially covers all major U.S. hospitals. This
dataset is used for a variety of purposes, including determining whether hospitals should be fined for not providing high quality
care to patients (see http://goo.gl/jAXFX for some background on this particular topic).

The Hospital Compare web site contains a lot of data and we will only look at a small subset for this assignment. The zip file 
for this assignment contains THREE (3) files:

(1) outcome-of-care-measures.csv: Contains information about THIRTY(30)-day mortality and readmission rates for heart attacks,
heart failure, and pneumonia for over FOUR THOUSAND (4,000) hospitals;

(2) hospital-data.csv: Contains information about each hospital;

(3) Hospital_Revised_Flatfiles.pdf: Descriptions of the variables in each file (i.e the code book).

A description of the variables in each of the files is in the included PDF file named Hospital_Revised_Flatfiles.pdf. This 
document contains information about many other files that are not included with this programming assignment. You will want to 
focus on the variables for Number NINETEEN (19) (“Outcome of Care Measures.csv”) and Number ELEVEN (11) (“Hospital Data.csv”). 
You may find it useful to print out this document (at least the pages for Table NINETEEN (19) and ELEVEN (11)) to have next to 
you while you work on this assignment. In particular, the numbers of the variables for each table indicate column indices in 
each table (i.e. “Hospital Name” is column TWO (2) in the outcome-of-care-measures.csv file).

Question

(1) Finding the best hospital in a state.

Write a function called best() that takes TWO (2) arguments: (a) the TWO(2)-character abbreviated name of a state; and (b) an
outcome name. The function reads the outcome-of-care-measures.csv file and returns a character vector with the name of the 
hospital that has the best (i.e. LOWEST) 30-day mortality for the specified outcome in that state. The hospital name is the name
provided in the Hospital.Name variable. The outcomes can be one of “heart attack”, “heart failure”, or “pneumonia”. The function 
should use the following template.

> best <- function(state, outcome) {                                                  
          ## Read outcome data                                                        
          ## Check that state and outcome are valid                                   
          ## Return hospital name in that state with lowest 30-day death rate         
  }                                                                                   
The function should check the validity of its arguments. If an invalid state value is passed to best(), the function should throw 
an error via the stop() function with the exact message “invalid state”. If an invalid outcome value is passed to best(), the
function should throw an error via the stop() function with the exact message “invalid outcome”.

Save your code for this function to a file named best.R. To run the test script for this part, make sure your working directory 
has the file best.R in it.

(2) Ranking hospitals by outcome in a state.

Write a function called rankhospital() that takes THREE (3) arguments: (a) the TWO(2)-character abbreviated name of a state 
(state); (b) an outcome (outcome); and © the ranking of a hospital in that state for that outcome (num). The function reads the
outcome-of-care-measures.csv file and returns a character vector with the name of the hospital that has the ranking specified by 
the num argument. For example, the call:

> rankhospital("MD", "heart failure", 5)                                              
would return a character vector containing the name of the hospital with the FIFTH (5th) LOWEST THIRTY(30)-day death rate for 
heart failure. The num argument can take values “best”, “worst”, or an integer indicating the ranking (SMALLER numbers are 
better). If the number given by num is LARGER THAN the number of hospitals in that state, then the function should return NA. The
function should use the following template.

> rankhospital <- function(state, outcome, num = "best") {                            
                  ## Read outcome data                                                
                  ## Check that state and outcome are valid                           
                  ## Return hospital name in that state with the given rank           
                  ## THIRTY(30)-day death rate                                        
  }                                                                                   
Hospitals that do NOT have data on a particular outcome should be excluded from the set of hospitals when deciding the rankings.

If there is MORE THAN ONE (1) hospital for a given ranking, then the hospital names should be sorted in alphabetical order and the
FIRST (1st) hospital in that set should be returned (i.e. if hospitals “b”, “c”, and “f” are tied for a given rank, then hospital 
“b” should be returned).

The function should check the validity of its arguments. If an invalid state value is passed to rankhospital(), the function
should throw an error via the stop() function with the exact message “invalid state”. If an invalid outcome value is passed to
rankhospital(), the function should throw an error via the stop() function with the exact message “invalid outcome”. The num 
variable can take values “best”, “worst”, or an integer indicating the ranking (SMALLER numbers are better). If the number given
by num is larger than the number of hospitals in that state, then the function should return NA.

Save your code for this function to a file named rankhospital.R. To run the test script for this part, make sure your working 
directory has the file rankhospital.R in it.

(3) Ranking hospitals in all states

Write a function called rankall() that takes TWO (2) arguments: (a) an outcome name (outcome); and (b) a hospital ranking (num). 
The function reads the outcome-of-care-measures.csv file and returns a TWO(2)-column data frame containing the hospital in EACH 
state that has the ranking specified in num. For example the function call

> rankall("heart attack", "best")                                                     
would return a data frame containing the names of the hospitals that are the best in their respective states for THIRTY(30)-day 
heart attack death rates. The function should return a value for EVERY state (some may be NA). The FIRST (1st) column in the data 
frame is named hospital, which contains the hospital name, and the SECOND (2nd) column is named state, which contains the 
TWO(2)-character abbreviation for the state name. The function should use the following template.

> rankall <-  function(outcome, num = "best") {                                       
              ## Read outcome data                                                    
              ## For each state, find the hospital of the given rank                  
              ## Return a data frame with the hospital names and the (abbreviated)    
              ## state name                                                           
  }                                                                                   
Hospitals that do NOT have data on a particular outcome should be excluded from the set of hospitals when deciding the rankings.

If there is MORE THAN ONE (1) hospital for a given ranking, then the hospital names should be sorted in alphabetical order and 
the FIRST (1st) hospital in that set should be returned (i.e. if hospitals “b”, “c”, and “f” are tied for a given rank, then 
hospital “b” should be returned).

NOTE: For the purpose of this part of the assignment (and for efficiency), your function should NOT call the rankhospital() 
function from the previous section.

The function should check the validity of its arguments. If an invalid outcome value is passed to rankall(), the function should 
throw an error via the stop() function with the exact message “invalid outcome”. The num variable can take values “best”, “worst”,
or an integer indicating the ranking (SMALLER numbers are better). If the number given by num is larger than the number of 
hospitals in that state, then the function should return NA.

Save your code for this function to a file named rankall.R. To run the test script for this part, make sure your working directory 
has the file rankall.R in it.
