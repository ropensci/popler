#' Stops tunnelling with popler
#'
#' Stop tunneling with the popler database
#' @param there is actually no parameter in this function!
#' @export


stop_SSH = function(){
  # the only way I can find to kill that invincible plink process
  shell("TASKKILL /F /IM POPLER_plink.exe", wait=F, invisible=T)
}
