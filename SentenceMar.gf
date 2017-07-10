concrete SentenceMar of Sentence = CatMar ** open Prelude, ResMar in {
    
  lin
    UseCl t p cl = {s = t.s ++ cl.s ! p.b ! t.t } ; 

		UseRCl t p rcl = UseCl t p rcl ;

    PredVP np vp = {
			s = \\p,t => case t of {
				Pres => np.s ! Nom ++ vp.adv ++ vp.pprs ++ agrV vp.verb p np.a t ;
				Past => np.s ! Erg ++ vp.adv ++ vp.pprs ++ agrV vp.verb p vp.erg_a.a t
			}
		} ;

		SlashVP np vps = PredVP np vps ;
}
