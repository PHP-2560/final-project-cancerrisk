#' Colon cancer prediction for females
#'
#' The colon cancer prediction function is a function used to calculate colon cancer risk for females. It is based on a published study by Wells et al., 2014. The function uses parameters that have been correlated to colon cancer such as age, physical activity,among many other ourlined below.
#' @param age numberic number
#' @param family_history numeric number
#' @param race numeric number
#' @param education numeric number
#' @param diabetes numeric number
#' @param smoking numeric number
#' @param multivitamins numeric number
#' @param BMI numeric number
#' @param drinks numeric number
#' @param estrogen numeric number
#' @param NSAIDS numeric number

#' @return numberic number
#'
#' @examples
#' colon_cancer_female(50,3,16,2,1,0,0,0,33.5,0,0)
#
#' @export

colon_cancer_female=function(age,race,education,estrogen,diabetes,smoking,family_history,
                             multivitamins,BMI,NSAIDS,drinks){
  if(age==45){a=0}
  else if(age>45& age<=50){a=20}
  else if(age>=51& age<=55){a=38}
  else if(age>=56& age<=60){a=55}
  else if(age>=61& age<=65){a=68}
  else if(age>=66& age<=70){a=79}
  else if(age>=71& age<=75){a=89.5}
  else if(age>=76& age<=80){a=100}

  if(race==1){b=2.5} # white
  else if (race==2){b=0} # latio
  else if (race==3){b=17.5} # black
  else if (race==4){b=6.5} # hawiian
  else if (race==5){b=18.5} # japanese

  if(smoking==0){c=0} #pack years of smoking
  else if(smoking>0&smoking<=10){c=6}
  else if(smoking>10&smoking<=30){c=7.5}
  else if(smoking>30&smoking<=50){c=10}

  if(drinks==0){d=0} #alcoholic drinks per day
  else if(drinks>0&drinks<=2){d=2.8}
  else if(drinks>2&drinks<=4){d=10}
  else if(drinks>4&drinks<=6){d=19}
  else if(drinks>6&drinks<=8){d=25}
  else if(drinks>8&drinks<=10){d=32}
  else if(drinks>10&drinks<=12){d=37.5}

  if(BMI==20){e=0} #BMI
  else if(BMI>20&BMI<=30){e=7}
  else if(BMI>30&BMI<=35){e=11}

  if(education==8){f=0} #years of education
  else if(education>8&education<=10){f=5.5}
  else if(education>10&education<=12){f=8}
  else if(education>12&education<=14){f=9}
  else if(education>14&education<=16){f=7.5}

  if(estrogen==0){g=9}# regular use  of aspirin # no
  else if(estrogen==1){g=0} # yes
  else if(estrogen==2){g=7.5} #yes-not currently

  if(family_history==0){h=0} #no
  else if(family_history==1){h=12.5} #yes

  if(multivitamins==0){i=7.5}# regular use of mutliviatmins # no
  else if(multivitamins==1){i=0} #yes

  if(diabetes==0){j=0}#no
  else if(diabetes==1){j=7.5} #yes

  if(NSAIDS==0){k=0} # regular use of NSAIDS #yes-currently
  else if(NSAIDS==1){k=7}#yes-previously
  else if(NSAIDS==2){k=12}#no

  p=a+b+c+d+e+f+g+h+i+j+k # total points from different variable

  if(p==0){risk=0.11} # covert total points into 10 years risk of CRC (%)
  else if(p>0&p<=64){risk=0.5}
  else if(p>64&p<=94){risk=1}
  else if(p>94&p<=126){risk=2}
  else if(p>126&p<=144){risk=3}
  else if(p>144&p<=156){risk=4}
  else if(p>156&p<=165){risk=5}
  else if(p>165&p<=197){risk=10}
  else if(p>197&p<=216){risk=15}
  else if(p>216&p<=232){risk=20}
  else if(p>232){risk="larger than 20%"}

  return(risk)
}


