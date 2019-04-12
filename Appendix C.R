# Calculate the relative importance of each PD SRM component
  # Estimated variance of each component (you can find this in the lavaan output under the header "Variances")
    # legend: 
    # fam = family component
    # i =  individual component
    # d = dyadic component
    # roles: mo = mother; fa = father; oc = oldest child; yc = youngest child.
      fam <- 0.473
      i.mo <- 0.129
      i.fa <- 0.417
      i.oc <- 0.099
      i.yc <- 0
      d.mofa <- 0.261
      d.mooc <- 0.168
      d.moyc <- 0.063
      d.faoc <- 0.045
      d.fayc <- 0.094
      d.ycoc <- 0.473

  # function for automatically calculting the relative importance of each component (no need to modify, simply run the following lines)
    relvar <- function(c1, c2, c3, c4){
      tot <- c1+c2+c3+c4
      rv1 <- round(c1/tot*100,2)
      rv2 <- round(c2/tot*100,2)
      rv3 <- round(c3/tot*100,2)
      rv4 <- round(c4/tot*100,2)
      cat("% of variance explained by", deparse(substitute(c1)), " = ",rv1, 
          "\n% of variance explained by", deparse(substitute(c2)), " = ",rv2, 
          "\n% of variance explained by", deparse(substitute(c3)), " = ",rv3, 
          "\n% of variance explained by", deparse(substitute(c4)), " = ",rv4)
    }

  # Use the previous function for calculating the relative variances of the PD SRM components for each of the PD observed score
  # Specify by which components the PD scores is defined
    # mother-father PD score
      relvar(c1 = fam, c2 = i.mo, c3 = i.fa, c4 = d.mofa)
    # mother-oldest child PD score
      relvar(c1 = fam, c2 = i.mo, c3 = i.oc, c4 = d.mooc)
    # mother-youngest child PD score
      relvar(c1 = fam, c2 = i.mo, c3 = i.yc, c4 = d.moyc)
    # father-oldest child PD score
      relvar(c1 = fam, c2 = i.fa, c3 = i.oc, c4 = d.faoc)
    # father-youngest child PD score
      relvar(c1 = fam, c2 = i.fa, c3 = i.yc, c4 = d.fayc)
    # youngest-oldest child PD score
      relvar(c1 = fam, c2 = i.yc, c3 = i.oc, c4 = d.ycoc)