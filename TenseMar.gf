concrete TenseMar of Tense = CatMar ** open ResMar, Prelude in {
  lin
    TTAnt t a = {s = t.s ++ a.s ; t = t.t ; a = a.a } ;
    
    PPos  = {s = [] ; p = Pos} ;
    PNeg  = {s = "nahi" ; p = Neg} ;
    TPres = {s = [] ; t = Pres} ;
    TPast = {s = [] ; t = Past} ;
    ASimul = {s = [] ; a = Simul} ;
}
