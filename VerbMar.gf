concrete VerbMar of Verb = CatMar ** open ResMar, Prelude in {

  lin
    UseV v = {
      verb = v ; 
      compl = \\_ => [] ;
      isv2 = False ;
      adv = []
    } ; 

    CompAP ap = {
      verb = copula ;
      compl = \\a => ap.s ! a.g ! a.n ! Nom ;
      isv2 = False ;
      adv = []
    } ;

    AdvVP v a = v ** {adv = a.s} ;

}
