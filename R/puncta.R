#' Run regression for puncta data
#' @export
#' @param dset data.frame of experimental data
#' @param y.var name of y variable in dset
#' @param cond.var name of condition variable in dset
#' @param subsamp.var name of subsample variable for random effect
#' @param cond.inc vector of conditions to include in analysis
#' @param trans.name name of function to transform y as a string
#' @param y.count indicates (T/F) whether y is a count variable



puncta.stats=function(dset,                   # data.frame of experimental data
                      y.var,                  # name of y variable in dset
                      cond.var,               # name of condition variable in dset
                      subsamp.var,            # name of subsample variable
                      cond.inc,               # vector of conditions to include in analysis
                      trans.name="identity",  # name of function to transform y as a string
                      y.count=F)              # indicates (T/F) whether y is a count variable

{
  inc=is.element(dset[,cond.var],cond.inc)&     # identify observations to include
    (!is.na(dset[,y.var]))
  dset$y.var=dset[,y.var]                       # define y-variable
  y.trans=eval(parse(text=trans.name))          # convert function name to an actual function
  dset$y.var=y.trans(dset$y.var)                # transform y-variable
  formula.string=paste0("y.var~",               # define formula string
                        cond.var,
                        "+(1|",subsamp.var,")")
  if (!y.count)                                 # lmer modeling for non-count data
  {
    fit=lmer(eval(parse(text=formula.string)),       # fit lmer model
             data=dset,subset=inc)

    res = summary(fit)$coef
    return(res)                                      # return results
  }

  if (y.count)                                 # glmer modeling for count data
  {
    fit=glmer(eval(parse(text=formula.string)),     # glmer model fitting
              data=dset,subset=inc,
              family=poisson())
    res = summary(fit)$coef
    return(res)
  }
}
