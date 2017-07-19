concrete SentenceMar of Sentence = CatMar ** open Prelude, ResMar in {
    
  lin
    UseCl t p cl = {s = t.s ++ cl.s ! p.p ! t.t } ; 

		UseRCl t p rcl = UseCl t p rcl ;

    PredVP np vp = {
			s = \\p,t => case <vp.verb.subj_c,vp.verb.obj_c,t> of {
				<_,Nom,_> 	=> np.s ! Nom ++ vp.adv ++ vp.pprs ++ agrV vp.verb p np.a t ; -- intransitive
				<Dat,_,_>		=> np.s ! Dat ++ vp.adv ++ vp.pprs ++ agrV vp.verb p vp.erg_a t ; -- dative subj
				<_,_,Pres>	=> np.s ! Nom ++ vp.adv ++ vp.pprs ++ agrV vp.verb p np.a t ; -- transitive
				<_,_,Past> 	=> np.s ! Erg ++ vp.adv ++ vp.pprs ++ agrV vp.verb p vp.erg_a t
			}
		} ;

		SlashVP np vps = PredVP np vps ;
}
