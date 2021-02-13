#generations: t-test and chi-square

t_test <- function(mean1=1, mean2=1, variance1=1, variance2=1, instances=1, critical=1){
  if (critical != 0){
    state <- "insignificant"
    t_value <- (mean1 - mean2)/sqrt((variance1 + variance2)/instances)
    #conditional
    if (t_value > critical){
      state <- "significant" 
    }
    print("t-value: ")
    print(t_value)
    print("critical value: ")
    print(critical)
    print("significance: ")
    print(state)
  }
}

#main method
main <- function(){
  
  decision <- "yes"
  while (decision == "yes"){
    
    M1 = as.double(readline(prompt="enter mean1: "))
    M2 = as.double(readline(prompt="enter mean2: "))
    V1 = as.double(readline(prompt="enter variance1: "))
    V2 = as.double(readline(prompt="enter variance2: "))
    I = as.double(readline(prompt="enter instances: "))
    C = as.double(readline(prompt="enter critical: "))
    t_test(mean1=M1, mean2=M2, variance1=V1, variance2=V2, instances=I)
    #prompt user again for another test
    decision <- readline(prompt="do you want to run another t-test? (yes or no): ")
  }
  
  
}

#run
main()
