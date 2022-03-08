#------------------------------------------------------------------------#
#                           Miscellaneous Code                           #
#------------------------------------------------------------------------#

#_________________________________#

# Create Cross-tab
# https://sejdemyr.github.io/r-tutorials/basics/tables-in-r/
t <- with(brmr3_tb, table(SURVEY.YEAR ,HEAD.OF.HOUSEHOLD..GENDER))
t <- prop.table(t, margin = 1)
t