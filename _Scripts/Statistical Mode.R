#------------------------------------------------------------------------#
#                   Code for the statistical Mode                        #
#           (Base R Mode returns internal storage mode of the            #
#                 object not the statistical Mode)                       #
#------------------------------------------------------------------------#

# https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}











