#'Calcualte BMI
#'
#'The BMI function is a simple function to assess an individual's health. It incorporates two variables, height(meters) and weight (kilograms).
#'
#'@param height numeric nubmer
#'@param weight nubmerc number

#'
#'@return Numeric number
#'
#'@examples
#'
#'BMI(1.70,50)
#'
#'
#'@export
BMI=function(height, weight){
  BMI=weight/(height)^2 # weight is kg; height is meter
  return(BMI)
}


