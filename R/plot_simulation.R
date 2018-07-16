#' @title Plots the SIR epidemic from the simulation
#'
#' @description Plots the total fraction of Susceptible, Infected and Recovered individuals at each time step.
#'
#' @param results The object containing the results from the simulation.
#' @param num_areas The number of areas in the simulation. Easy way to get it for your current simulation is
#'                  to take the dimension of your transmission kernel.
#' @param num_ages The number of ages in the simulation. ONLY SUPPORTS 4 CURRENTLY
#' @param by_age Logical. If TRUE, will create a plot for each age category.
#'
#' @return Creates a plot.
#'
#' @examples
#' plot_simulation(results, dim(kernel)[1], 4)
#'
#' @export

#messy function in the indexing but gets the job done

plot_simulation = function(results, num_areas, num_ages, by_age=F, stoch=F, step=1, title=NA){

  if(stoch == F){
    if(by_age == FALSE){

      par(mar=c(5,4,4,5)+.1)

      total = sum(results[1,2:dim(results)[2]])

      plot(results[,1], rowSums(results[,2:(num_areas*num_ages+1)])/total, col="green", type="l", xlab="Time", ylab="S, R", ylim=c(0, 1), main=title)
      lines(results[,1], rowSums(results[,(num_areas*num_ages*2+2):dim(results)[2]])/total, col="blue")

      par(new=T)
      plot(results[,1], rowSums(results[,(num_areas*num_ages+2):(num_areas*num_ages*2+1)])/total, type="l", col="red", xlab="",ylab="", xaxt="n",yaxt="n")
      axis(4)
      mtext("I",side=4,line=3)

      legend("topright", col=c("green", "red", "blue"), legend=c("S", "I", "R"), lty=1)

    } else {

      par(mfrow=c(2,2))

      S_1 = rowSums(results[,2:(num_areas+1)])
      S_2 = rowSums(results[,(num_areas+2):(num_areas*2+1)])
      S_3 = rowSums(results[,(num_areas*2+2):(num_areas*3+1)])
      S_4 = rowSums(results[,(num_areas*3+2):(num_areas*4+1)])

      I_1 = rowSums(results[,(num_areas*4+2):(num_areas*5+1)])
      I_2 = rowSums(results[,(num_areas*5+2):(num_areas*6+1)])
      I_3 = rowSums(results[,(num_areas*6+2):(num_areas*7+1)])
      I_4 = rowSums(results[,(num_areas*7+2):(num_areas*8+1)])

      R_1 = rowSums(results[,(num_areas*8+2):(num_areas*9+1)])
      R_2 = rowSums(results[,(num_areas*9+2):(num_areas*10+1)])
      R_3 = rowSums(results[,(num_areas*10+2):(num_areas*11+1)])
      R_4 = rowSums(results[,(num_areas*11+2):(num_areas*12+1)])

      total1 = sum(S_1[1],I_1[1],R_1[1])
      total2 = sum(S_2[1],I_2[1],R_2[1])
      total3 = sum(S_3[1],I_3[1],R_3[1])
      total4 = sum(S_4[1],I_4[1],R_4[1])


      plot(results[,1], S_1/total1, type="l", col="green", ylab="Population", xlab="Time", main="0-4 years old", ylim=c(0, 1))
      lines(results[,1], I_1/total1, col="red")
      lines(results[,1], R_1/total1, col="blue")
      legend("topright", col=c("green", "red", "blue"), legend=c("S", "I", "R"), lty=1)


      plot(results[,1], S_2/total2, type="l", col="green", ylab="Population", xlab="Time", main="5-19 years old", ylim=c(0, 1))
      lines(results[,1], I_2/total2, col="red")
      lines(results[,1], R_2/total2, col="blue")
      legend("topright", col=c("green", "red", "blue"), legend=c("S", "I", "R"), lty=1)


      plot(results[,1], S_3/total3, type="l", col="green", ylab="Population", xlab="Time", main="20-64 years old", ylim=c(0, 1))
      lines(results[,1], I_3/total3, col="red")
      lines(results[,1], R_3/total3, col="blue")
      legend("topright", col=c("green", "red", "blue"), legend=c("S", "I", "R"), lty=1)


      plot(results[,1], S_4/total4, type="l", col="green", ylab="Population", xlab="Time", main="65+ years old", ylim=c(0, 1))
      lines(results[,1], I_4/total4, col="red")
      lines(results[,1], R_4/total4, col="blue")
      legend("topright", col=c("green", "red", "blue"), legend=c("S", "I", "R"), lty=1)


    }
  } else {

    par(mar=c(5,4,4,5)+.1)

    total = sum(results[1,2:dim(results)[2]])

    plot(c(0:(max(results[,1])/step)), rowSums(results[seq(1,max(results[,1])+1, step),2:(num_areas*num_ages+1)])/total, col="green", type="l", xlab="Time", ylab="S, R", ylim=c(0, 1), main=title)
    lines(c(0:(max(results[,1])/step)), rowSums(results[seq(1,max(results[,1])+1, step),(num_areas*num_ages*2+2):dim(results)[2]])/total, col="blue")

    par(new=T)
    plot(c(0:(max(results[,1])/step)), rowSums(results[seq(1,max(results[,1])+1, step),(num_areas*num_ages+2):(num_areas*num_ages*2+1)])/total, type="l", col="red", xlab="",ylab="", xaxt="n",yaxt="n")
    axis(4)
    mtext("I",side=4,line=3)

    legend("topright", col=c("green", "red", "blue"), legend=c("S", "I", "R"), lty=1)


  }
}
