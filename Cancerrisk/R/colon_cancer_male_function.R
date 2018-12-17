#' Colon cancer prediction for males
#'
#' The colon cancer prediction function is a function used to calculate colon cancer risk for males. It is based on a published study by Wells et al., 2014. The function uses parameters that have been correlated to colon cancer such as age, physical activity,among many other ourlined below. amomg
#' @param age numberic number
#' @param family_history numeric number
#' @param race numeric number
#' @param education numeric number
#' @param aspirin numeric number
#' @param diabetes numeric number
#' @param smoking numeric number
#' @param multivitamins numeric number
#' @param BMI numeric number
#' @param red_meat numeric number
#' @param drinks numeric number
#' @param physical_activiites nuemric number


#' @return numberic number

#'
#' @examples
#' colon_cancer_male(60,1,12,1,0,20,0,1,20,1,2,0)
#'
#' @export


colon_cancer_male=function(age,race,education,aspirin,diabetes,smoking,family_history,
                           multivitamins,BMI,red_meat,drinks,physical_activiites){
  if(age==45){a=0}
  else if(age>45& age<=50){a=19}
  else if(age>=51& age<=55){a=35}
  else if(age>=56& age<=60){a=53}
  else if(age>=61& age<=65){a=64}
  else if(age>=66& age<=70){a=78}
  else if(age>=71& age<=75){a=89}
  else if(age>=76& age<=80){a=100}

  if(race==1){b=0} # white
  else if (race==2){b=1} # latio
  else if (race==3){b=5} # black
  else if (race==4){b=13} # hawiian
  else if (race==5){b=15} # japanese

  if(smoking==0){c=0} #pack years of smoking
  else if(smoking>0&smoking<=20){c=2.5}
  else if(smoking>20&smoking<=30){c=5}
  else if(smoking>30&smoking<=40){c=9}
  else if(smoking>40&smoking<=50){c=11}

  if(drinks==0){d=0} #alcoholic drinks per day
  else if(drinks>0&drinks<=2){d=12.5}
  else if(drinks>2&drinks<=4){d=14}
  else if(drinks>4&drinks<=6){d=15}
  else if(drinks>6&drinks<=12){d=19}

  if(BMI==20){e=0} #BMI
  else if(BMI>20&BMI<=25){e=2}
  else if(BMI>25&BMI<=30){e=7}
  else if(BMI>30&BMI<=35){e=11}

  if(education==8){f=0} #years of education
  else if(education>8&education<=10){f=4.5}
  else if(education>10&education<=12){f=8}
  else if(education>12&education<=14){f=8.2}
  else if(education>14&education<=16){f=5}

  if(aspirin==0){g=5}# regular use  of aspirin # no
  else if(aspirin==1){g=0} # yes
  else if(aspirin==2){g=4} #yes-not currently

  if(family_history==0){h=0} #no
  else if(family_history==1){h=10} #yes

  if(multivitamins==0){i=8} # regular use of multivtamins # no
  else if(multivitamins==1){i=0} #yes

  if(red_meat==0){j=0} #onces of red meat intake per day
  else if(red_meat>0&red_meat<=1){j=2.5}
  else if(red_meat>1&red_meat<=2){j=6}
  else if(red_meat>2&education<=4){j=7.5}

  if(diabetes==0){k=0}#no
  else if(diabetes==1){k=5} #yes

  if(physical_activiites==0){l=5} #hours of moderte physical activity perday
  else if(physical_activiites>0&physical_activiites<=1){l=2}
  else if(physical_activiites>1&physical_activiites<=2){l=0}

  p=a+b+c+d+e+f+g+h+i+j+k+l # total points from different variable

  if(p<=10){risk=0.2}# covert total points into 10 years risk of CRC (%)
  else if(p>10&p<=46){risk=0.5}
  else if(p>46&p<=76){risk=1}
  else if(p>10&p<=104){risk=2}
  else if(p>46&p<=120){risk=3}
  else if(p>10&p<=132){risk=4}
  else if(p>46&p<=142){risk=5}
  else if(p>10&p<=170){risk=10}
  else if(p>46&p<=188){risk=15}
  else if(p>188){risk="larger than 15%"}

  return(risk)
}


