#generations: T-test and chi-square

#declare global variables
expected <- 0
observed <- 0
chi_value <- 0

#method: calculate chi-square
calculate_chi <- function(observed_values, expected=1){
  sum <- 0
  for (observed in observed_values){
    chi_value = chi_value + (observed - expected)**2 
  }
  print(chi_value/expected)
}

#method: compare for significance
compare_significance <- function(critical){
  if (chi_value > critical){
    print("significant")
  }
  else{
    print("insignificant")
  }
}

#main method
main <- function(){
  decision <- "yes"
  while (decision == "yes"){
    feedback <-  "yes"
    while (feedback == "yes"){
      append(observed, as.double(readline(prompt="enter the observed value: ")))
      feedback <-  readline(prompt="another expected? (yes or no): ")
    }
    expected <- as.double(readline(prompt="enter the expected: "))
    critical <- as.double(readline(prompt="enter the critical: "))
    calculate_chi(observed, expected=expected)
    compare_significance(critical)
  }
}

main()