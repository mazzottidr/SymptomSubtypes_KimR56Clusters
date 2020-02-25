
##
#
# Functions for use in Shiny Application
#
##
##
# Function for tolerance equality (within dplyr)
##
#' Compare two numeric vectors
#'
#' This is a safe way of comparing if two vectors of floating point numbers
#' are (pairwise) equal.  This is safer than using `==`, because it has
#' a built in tolerance
#'
#' @param x,y Numeric vectors to compare
#' @param tol Tolerance of comparison.
#' @export
#' @examples
#' sqrt(2) ^ 2 == 2
#' near(sqrt(2) ^ 2, 2)
near <- function(x, y, tol = .Machine$double.eps^0.5) {
  abs(x - y) < tol
}

##
# Primary Function for Determining Subtype Probabilities
##
doClusterProbs <- function(ctyp, 
                           e1, e2, e3, e4, e5, e6, e7, e8, 
                           s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15_1, s15_2, 
                           c1_1, c1_2, c1_3, c1_4, c2, c3, c4_1, c4_2, c4_3)
{
  
  ess <- 8*mean(c(as.numeric(e1), as.numeric(e2), as.numeric(e3), as.numeric(e4), as.numeric(e5), as.numeric(e6), as.numeric(e7), as.numeric(e8)), na.rm=TRUE )
  if( is.na(ess) ) {
    #resultList <- list(Error="Please complete at least one question in the Epworth Sleepiness Scale.")
    #return(resultList)
    stop()
  }
  #ess0123  <- 1-as.numeric(cut(epworth, c(0,5,10,15,24)))
  
  s001    <- as.numeric(e2>1)                                 ## Sleepy Watching TV
  s101    <- as.numeric(s1<3)                                 ## I feel rested upon waking
  s201    <- as.numeric(s2>2)                                 ## I feel sleepy during the day                   
  s301    <- as.numeric(s3>2)                                 ## Physically Tired / Fatigued
  s401    <- as.numeric(s4>2)                                 ## Fall asleep Involuntarily
  s501    <- as.numeric(s5>2)                                 ## Napping
  s601    <- as.numeric(s6>2)                                 ## Drowsy Driving
  s701    <- as.numeric(s7>2)                                 ## Difficulty Initiating Sleep
  s801    <- as.numeric(s8>2)                                 ## Difficulty Maintaining Sleep
  s901    <- as.numeric(s9>2)                                 ## Early Morning Awakening
  s1001   <- as.numeric(s10>2)                                ## I'm Restless in my Sleep
  s1101   <- as.numeric(s11>2)                                ## Headache
  s1201   <- as.numeric(s12>2)                                ## Nocturnal Sweating
  s1301   <- as.numeric(s13>2)                                ## Wake up and can't breathe
  s1401   <- as.numeric(s14>2)                                ## Witnessed Apneas
  s1501   <- as.numeric(s15_1==1)                             ## Snoring (Yes/No)
  s15a01  <- as.numeric(s15_1==1 & s15_2==1)                  ## Snoring (Disturb Partner)
  s15b01  <- as.numeric(s15_1==1 & s15_2!=1)                  ## Snoring (Does not disturb partner)
  c101    <- as.numeric(c1_1>3 & c1_2==1 & c1_3==1            ## RLS Questions
                        & c1_4!=1 & c1_4!=2)  
  c201    <- as.numeric(c2==1)                                ## HTN
  c301    <- as.numeric(c3==1)                                ## Diabetes
  c401    <- as.numeric(c4_1==1 | c4_2==1 | c4_3==1)          ## CVD
  
  ##
  # 3 Clusters using ISAC Model
  ##
  
    ## Intercept
    b_int_c1 <- 135.97626846
    b_int_c2 <- 262.25855299
    
    ## ESS
    b_ess_c1 <- -5.54167109
    b_ess_c2 <- -7.27049947
    
    ## Sleepy Watching TV
    b_s001_c1 <- -43.28515442
    b_s001_c2 <- -39.76954201
    
    ## I feel rested upon waking
    b_s101_c1 <- 4.52977493
    b_s101_c2 <- 31.92096891
    
    ## I feel sleepy during the day
    b_s201_c1 <- -9.5107117
    b_s201_c2 <- -36.86635931
    
    ## Physically Tired / Fatigued
    b_s301_c1 <- -15.30268528
    b_s301_c2 <- -38.15484015
    
    ## Fall asleep Involuntarily
    b_s401_c1 <- -29.67786896
    b_s401_c2 <- -29.16375775
    
    ## Napping
    b_s501_c1 <- -2.14419451
    b_s501_c2 <- -16.81710847
    
    ## Drowsy Driving
    b_s601_c1 <- -31.45362104
    b_s601_c2 <- -27.33768493
    
    ## Difficulty Initiating Sleep
    b_s701_c1 <- 8.65356196
    b_s701_c2 <- -13.29596353
    
    ## Difficulting Maintaining Sleep
    b_s801_c1 <- 11.47697128
    b_s801_c2 <- -20.45090095
    
    ## Early Morning Awakening
    b_s901_c1 <- 8.52923964
    b_s901_c2 <- -15.33311227
    
    ## I'm Restless in my Sleep
    b_s1001_c1 <- 2.79067383
    b_s1001_c2 <- -22.84086051
    
    ## Headache
    b_s1101_c1 <- -5.23038888
    b_s1101_c2 <- -29.60380654
    
    ## Nocturnal Sweating
    b_s1201_c1 <- 3.58669229
    b_s1201_c2 <- -20.59836187
    
    ## Wake up and Can't Breathe
    b_s1301_c1 <- -1.61671264
    b_s1301_c2 <- -46.89372642
    
    ## Witnessed Apneas
    b_s1401_c1 <- -9.56947407
    b_s1401_c2 <- -16.7115447
    
    ## Snoring (Disturb Partner)
    b_s15a01_c1 <- -6.89790588
    b_s15a01_c2 <- -20.34386702
    
    ## Snoring (Does not disturb partner)
    b_s15b01_c1 <- 2.71262791
    b_s15b01_c2 <- -14.78684419
    
    ## RLS
    b_c101_c1 <- -0.70273473
    b_c101_c2 <- -20.2692296
    
    ## Hypertension
    b_c201_c1 <- 3.05462225
    b_c201_c2 <- 6.017278
    
    ## Diabetes
    b_c301_c1 <- 1.27902404
    b_c301_c2 <- 1.9992275
    
    ## CVD
    b_c401_c1 <- -1.06240973
    b_c401_c2 <- -2.38225137
    
    xcl1 =(
      b_int_c1            ## Intercept
      + b_ess_c1*ess        ## ESS
      + b_s001_c1*s001      ## Sleepy Watching TV
      + b_s101_c1*s101      ## I feel rested upon waking
      + b_s201_c1*s201      ## I feel sleepy during the day  
      + b_s301_c1*s301      ## Physicaly Tired/Fatigued
      + b_s401_c1*s401      ## Fall asleep involuntarily
      + b_s501_c1*s501      ## Napping
      + b_s601_c1*s601      ## Drowsy Driving
      + b_s701_c1*s701      ## Difficulty Initiating Sleep
      + b_s801_c1*s801      ## Difficulty Maintaining Sleep
      + b_s901_c1*s901      ## Early Morning Awakening
      + b_s1001_c1*s1001    ## I'm restless in my sleep
      + b_s1101_c1*s1101    ## Headache
      + b_s1201_c1*s1201    ## Nocturnal Sweating
      + b_s1301_c1*s1301    ## Wake up and can't breathe
      + b_s1401_c1*s1401    ## Witnessed Apneas
      + b_s15a01_c1*s15a01  ## Snoring (Disturbs Partner)
      + b_s15b01_c1*s15b01  ## Snoring (Does not disturb Partner)
      + b_c101_c1*c101      ## RLS
      + b_c201_c1*c201      ## Hypertension
      + b_c301_c1*c301      ## Diabetes
      + b_c401_c1*c401      ## CVD
    )
    
    xcl2 =(
      b_int_c2            ## Intercept
      + b_ess_c2*ess        ## ESS
      + b_s001_c2*s001      ## Sleepy Watching TV
      + b_s101_c2*s101      ## I feel rested upon waking
      + b_s201_c2*s201      ## I feel sleepy during the day  
      + b_s301_c2*s301      ## Physicaly Tired/Fatigued
      + b_s401_c2*s401      ## Fall asleep involuntarily
      + b_s501_c2*s501      ## Napping
      + b_s601_c2*s601      ## Drowsy Driving
      + b_s701_c2*s701      ## Difficulty Initiating Sleep
      + b_s801_c2*s801      ## Difficulty Maintaining Sleep
      + b_s901_c2*s901      ## Early Morning Awakening
      + b_s1001_c2*s1001    ## I'm restless in my sleep
      + b_s1101_c2*s1101    ## Headache
      + b_s1201_c2*s1201    ## Nocturnal Sweating
      + b_s1301_c2*s1301    ## Wake up and can't breathe
      + b_s1401_c2*s1401    ## Witnessed Apneas
      + b_s15a01_c2*s15a01  ## Snoring (Disturbs Partner)
      + b_s15b01_c2*s15b01  ## Snoring (Does not disturb Partner)
      + b_c101_c2*c101      ## RLS
      + b_c201_c2*c201      ## Hypertension
      + b_c301_c2*c301      ## Diabetes
      + b_c401_c2*c401      ## CVD
    )
    
    ##
    # Cluster Probabilities
    ##
    prcl1 = exp(xcl1)/(exp(xcl1)+exp(xcl2)+1)   ## Disturbed Sleep
    prcl2 = exp(xcl2)/(exp(xcl1)+exp(xcl2)+1)   ## Minimally Symptomatic
    prcl3 = 1/(exp(xcl1)+exp(xcl2)+1)          ## Excessively Sleepy
    
    prmax <- max(prcl1, prcl2, prcl3)
    
    if (near(prmax, prcl1, tol=0.000001)) {
      OSASub = "DISTURBED SLEEP"
      ProbSub = round(prcl1*100,1)
    }
    if (near(prmax, prcl2, tol=0.000001)) {
      OSASub = "MINIMALLY SYMPTOMATIC"
      ProbSub = round(prcl2*100,1)
    }
    if (near(prmax, prcl3, tol=0.000001)) {
      OSASub = "EXCESSIVELY SLEEPY"
      ProbSub = round(prcl3*100,1)
    }
    
    resultList <- list(PrC1=prcl1,
                       PrC2=prcl2,
                       PrC3=prcl3,
                       Subtype = OSASub,
                       Probability = ProbSub,
                       ESS = ess)
    return(resultList)
    
}
