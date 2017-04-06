concrete SentenceMar of Sentence = CatMar ** open Prelude, ResMar in {
    
  lin
    UseCl t p cl = {s = t.s ++ cl.s ! p.b ! t.t } ; 

    PredVP np vp = {
       s = \\p,t => np.s ! subCase vp.isv2 t ++ vp.adv ++ vp.compl ! np.a ++ agrV vp.verb p np.a t ;
    } ;
}
