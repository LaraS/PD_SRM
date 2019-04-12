# The standard PD SRM ---------------------------------
  ## Step 1: Read in your data.
    # Suppose you want to use a datafile called \texttt{data} which is located in the folder datafolder.
    # Step 1a: Specify the path to your data
      setwd("C:\Users\laras\Documents\datafolder")
      
    # Step 1b: read in your data (only run line with the correct extension of your file)
      # for txt-files:
      mydata <- read.table("data.txt")
      # for csv-files
      mydata <- read.csv("data.csv")
      # for SPSS-files
      library(foreign)
      mydata <- read.spss("data.sav")
      
    # Step 1c: take a look at the first lines of your data file
      head(mydata[,-1])

    # Step 1c: take a look at the first lines of your data file
      head(mydata)
      # On top of the collumns, the variable names are displayed. Note that the variables in this data set are labelled as MF, MO, MY, FO, FY and YO. Here, M represents mother, F father, Y youngest child and O oldest child. These variable names are used in step 2.

  ## Step 2: Specify the Purely Dyadic Social Relations Model.
    # for details on how to specify lavaan syntax, please consult the .Rmd-file. 
    PDSRM <- '
    # Specify the indicators for each PD SRM component (cfr. Figure 3)
    # If you use other names for your variables, please adapt this in the following script
    FC =~ lambdaMF*MF + lambdaMO*MO + lambdaMY*MY + lambdaFO*FO + lambdaFY*FY + lambdaYO*YO
    I.M =~ 1*MF + 1*MO + 1*MY
    I.F =~ 1*MF + 1*FO + 1*FY 
    I.O =~ 1*MO  + 1*FO + 1*YO
    I.Y =~ 1*MY  + 1*FY + 1*YO
    D.MF =~ 1*MF
    D.MO =~ 1*MO
    D.MY =~ 1*MY
    D.FO =~ 1*FO
    D.FY =~ 1*FY
    D.OY =~ 1*YO
    
    # Variances
    FC ~~ VAR.FC*FC
    I.M ~~ VAR.I.M*I.M
    I.F ~~ VAR.I.F*I.F
    I.O ~~ VAR.I.O*I.O
    I.Y ~~ VAR.I.Y*I.Y
    D.MF ~~ VAR.D.MF*D.MF
    D.MO ~~ VAR.D.MO*D.MO
    D.MY ~~ VAR.D.MY*D.MY
    D.FO ~~ VAR.D.FO*D.FO
    D.FY ~~ VAR.D.FY*D.FY
    D.OY ~~ VAR.D.OY*D.OY
    
    # Means
    FC ~ mean.FC*1
    I.M ~ mean.I.M*1
    I.F ~ mean.I.F*1
    I.O ~ mean.I.O*1
    I.Y ~ mean.I.Y*1
    D.MF ~ mean.D.MF*1
    D.MO ~ mean.D.MO*1
    D.MY ~ mean.D.MY*1
    D.FO ~ mean.D.FO*1
    D.FY ~ mean.D.FY*1
    D.OY ~ mean.D.OY*1

    # Intragenerational similarities (IF YOU WANT TO CALCULATE THESE: REMOVE THE # BEFORE THE FOLLOWING 2 LINES)
    # I.M ~~ I.F
    # I.O ~~ I.Y
    
    # Constraints
    mean.I.M + mean.I.F +  mean.I.O + mean.I.Y == 0
    mean.D.MF + mean.D.MO + mean.D.MY == 0
    mean.D.MF + mean.D.FO +  mean.D.FY == 0
    mean.D.MY + mean.D.FY + mean.D.OY == 0
    mean.D.MO + mean.D.FO + mean.D.OY == 0
    
    # No negative variances are allowed (cfr. other software like EQS)
    VAR.FC  > 0
    VAR.I.M > 0
    VAR.I.F > 0
    VAR.I.O > 0
    VAR.I.Y > 0
    VAR.D.MF > 0
    VAR.D.MO > 0
    VAR.D.MY > 0
    VAR.D.FO > 0
    VAR.D.FY > 0
    VAR.D.OY > 0

    # set constraints on factor loadings FC for identifiability
    lambdaMF + lambdaMO + lambdaMY + lambdaFO + lambdaFY + lambdaYO == 6
    '

  ## Step 3: Fit the model with your data and request the output
    # load the lavaan package
      # If you are using R for the first time on this device, please make sure to install the lavaan package first by running install.packages("lavaan").
      library(lavaan)
    # fit the model with the data and ask a summary of the results
      fit <- lavaan(model = PDSRM, data = mydata, missing = "fiml")
      summary(fit, fit.measures = T)

      
# Extending the Standard PD SRM ----------------------
  # Test additional correlations between two PD SRM components. For example, allow a correlation between the mother-youngest child dyadic component and the father-youngest child dyadic component 
    PDSRM2 <- paste0(PDSRM, 'D.MY ~~ D.FY' )
    fit2 <- lavaan(model = PDSRM2, data = mydata, missing = "fiml")
    summary(fit2, fit.measures = T)
    
  # Test an additional hypothesis. Example 1: do the individual components of mothers and fathers differ significantly?
    PDSRM3 <- paste0(PDSRM, 'diff := mean.I.M - mean.I.F' )
    fit3 <- lavaan(model = PDSRM3, data = mydata, missing = "fiml")
    summary(fit3, fit.measures = T)
      # No significant difference between the amount of meals mothers and father share with all family members is found (diff = 0.251, p = 0.741). 
    # Example 2: Do mothers and fathers differ in the unique coordination they have with their children? 
    PDSRM4 <- paste0(PDSRM, 'diffO := mean.D.MO - mean.D.FO
                             diffY := mean.D.MY - mean.D.FY' )
    fit4 <- lavaan(model = PDSRM4, data = mydata, missing = "fiml")
    summary(fit4, fit.measures = T)

 
# The PD SRM with two indicators ----------------------
  # read in your data
  mydata2ind <- read.table("mydata2ind.txt")

  # The PD SRM with 2 indicators
  PDSRM_2ind <- '
  # Latent variables
  FC =~ lambdaMF1*MF1 + lambdaMO1*MO1 + lambdaMY1*MY1 + lambdaFO1*FO1 + lambdaFY1*FY1 + 
  lambdaYO1*YO1 + lambdaMF2*MF2 + lambdaMO2*MO2 + lambdaMY2*MY2 + lambdaFO2*FO2 + 
  lambdaFY2*FY2 + lambdaYO2*YO2
  I.M =~  1*MF1 + 1*MO1 + 1*MY1 +
  1*MF2 + 1*MO2 + 1*MY2  
  I.F =~  1*MF1 + 1*FO1 + 1*FY1 +
  1*MF2 + 1*FO2 + 1*FY2 
  I.O =~  1*MO1  + 1*FO1 + 1*YO1 +
  1*MO2  + 1*FO2 + 1*YO2
  I.Y =~  1*MY1  + 1*FY1 + 1*YO1 +
  1*MY2  + 1*FY2 + 1*YO2
  D.MF =~ 1*MF1 + 1*MF2
  D.MO =~ 1*MO1 + 1*MO2
  D.MY =~ 1*MY1 + 1*MY2
  D.FO =~ 1*FO1 + 1*FO2
  D.FY =~ 1*FY1 + 1*FY2
  D.OY =~ 1*YO1 + 1*YO2
  
  # Variances
  FC ~~ VAR.FC*FC
  I.M ~~ VAR.I.M*I.M
  I.F ~~ VAR.I.F*I.F
  I.O ~~ VAR.I.O*I.O
  I.Y ~~ VAR.I.Y*I.Y
  D.MF ~~ VAR.D.MF*D.MF
  D.MO ~~ VAR.D.MO*D.MO
  D.MY ~~ VAR.D.MY*D.MY
  D.FO ~~ VAR.D.FO*D.FO
  D.FY ~~ VAR.D.FY*D.FY
  D.OY ~~ VAR.D.OY*D.OY
  MF1 ~~ MF1
  MO1 ~~ MO1 
  MY1 ~~ MY1
  FO1 ~~ FO1
  FY1 ~~ FY1
  YO1 ~~ YO1
  MF2 ~~ MF2
  MO2 ~~ MO2 
  MY2 ~~ MY2
  FO2 ~~ FO2
  FY2 ~~ FY2
  YO2 ~~ YO2
  
  # Intercepts
  FC ~ mean.FC*1
  I.M ~ mean.I.M*1
  I.F ~ mean.I.F*1
  I.O ~ mean.I.O*1
  I.Y ~ mean.I.Y*1
  D.MF ~ mean.D.MF*1
  D.MO ~ mean.D.MO*1
  D.MY ~ mean.D.MY*1
  D.FO ~ mean.D.FO*1
  D.FY ~ mean.D.FY*1
  D.OY ~ mean.D.OY*1
  
  # intragenerational similarity
  # I.M ~~ I.F
  # I.O ~~ I.Y
  
  # Constraints
  mean.I.M + mean.I.F +  mean.I.O + mean.I.Y == 0
  mean.D.MF + mean.D.MO + mean.D.MY == 0
  mean.D.MF + mean.D.FO +  mean.D.FY == 0
  mean.D.MY + mean.D.FY + mean.D.OY == 0
  mean.D.MO + mean.D.FO + mean.D.OY == 0
  
  # no negative variances are allowed (cfr. other software)
  VAR.FC  > 0
  VAR.I.M > 0
  VAR.I.F > 0
  VAR.I.O > 0
  VAR.I.Y > 0
  VAR.D.MF > 0
  VAR.D.MO > 0
  VAR.D.MY > 0
  VAR.D.FO > 0
  VAR.D.FY > 0
  VAR.D.OY > 0
  
  # set constraints on factor loadings FE for identifiability
  12 == lambdaMF1 + lambdaMO1 + lambdaMY1 + lambdaFO1 + lambdaFY1 + lambdaYO1 + 
  lambdaMF2 + lambdaMO2 + lambdaMY2 + lambdaFO2 + lambdaFY2 + lambdaYO2 
  '

  # fit the model with your data and request the output
  fit_2ind <- lavaan(data = mydata2ind, model = PDSRM_2ind)
  summary(fit_2ind, fit.measures = T)  

