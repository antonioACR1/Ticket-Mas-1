# Ticket Mas 1
## THIS IS A RECOMMENDER SYSTEM WHICH I DEVELOPED WHEN I WAS WORKING AT THE RETAIL COMPANY VIRTUAL MARKET (MEXICO CITY). CHECK THE SIMPLIFIED VERSION DEPLOYED AS A WEBSITE AT https://antonioacr1.shinyapps.io/a-priori/

#### IF YOU WANT A PERSONALIZED VERSION OF THIS MODEL, PLEASE SEND AN EMAIL TO acr@ciencias.unam.mx

#### THIS MODEL USES THE ALGORITHM "A-PRIORI" IN R. THE ORIGINAL VERSION INCLUDES SEVERAL LOOPS IN ORDER TO DEAL WITH THE AMOUNT OF DATA AND A SQL CONNECTION TO EXTRACT THE DATA FROM SQL SERVER. THE SIMPLIFIED VERSION DEALS ONLY WITH SMALL DATA AND YOU HAVE TO UPLOAD A CSV FILE WITH SPECIFIC FORMAT.

#### ALSO, THE ORIGINAL VERSION CONTAINS THE SUBCATEGORY AND CATEGORY OF THE PRODUCTS. THE ALGORITHM "A-PRIORI" DOES NOT PROVIDE SUGGESTIONS FOR ALL PRODUCTS, HOWEVER IT IS POSSIBLE TO ASSIGN A SUGGESTION TO THOSE PRODUCTS WITHOUT INITIAL SUGGESTION BY APPLYING SEVERAL SQL JOINS ON THE SUBCATEGORY AND CATEGORY IF THEY EXIST. IN THAT WAY, A PRODUCT WITHOUT A SUGGESTION WILL BE ASSIGNED THE SUGGESTION OF ANOTHER PRODUCT IN THE SAME SUBCATEGORY/CATEGORY WHICH ALREADY HAS AN INITIAL SUGGESTION. THE SIMPLIFIED VERSION DOES NOT CONTAIN THIS OPTION. 
