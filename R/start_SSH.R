#' Starts tunnelling with popler
#'
#' Start tunneling with the popler database itself
#' @param there is actually no parameter in this function!
#' @export

# a method to start SSH tunneling
start_SSH = function(){
  # get current working directory (TODO: CHANGE THIS! IT'S WEIRD AND BAD!)
  cwd <- getwd()

  # set working directory to location of login.bat
  setwd("./exec")

  # open the connection
  shell("start /min server_login.bat ^& exit", wait=F, invisible=T)

  # change the working directory back
  setwd(cwd)
}
