concrete SentenceMar of Sentence = CatMar ** open Prelude, ResMar in {
    
  lin
    UseCl t p cl = {s = t.s ++ cl.s ! p.b ! t.t } ; 

    PredVP np vp = {
--       s = \\p,t => np.s ! subCase vp.isv2 t ++ vp.adv ++ vp.compl ! np.a ++ agrV vp.verb p np.a t ;
		s = \\p,t => case t of {
			Pres => np.s ! Nom ++ vp.adv ++ vp.pprs ++ agrV vp.verb p np.a t ;
			Past => np.s ! Erg ++ vp.adv ++ vp.pprs ++ agrV vp.verb p np.a t
    }
	} ;
}
