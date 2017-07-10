concrete VerbMar of Verb = CatMar ** open ResMar, Prelude in {

  lin
    UseV v = predV v ;

--    CompAP ap = {
--      verb = copula ;
--      compl = \\a => ap.s ! a.g ! a.n ! Nom ;
--      isv2 = False ;
--      adv = []
--    } ;
--

	
	SlashV2a v2 = predV v2 ;
	
	ComplSlash vps np = {
		verb = vps.verb ;
		-- will have to include agr for ergativity
		adv = case np.anim of {
			Animate => vps.adv ++ np.s ! Acc ;
			Inanimate => vps.adv ++ np.s ! Nom
		} ;
		pprs = vps.pprs ;
		erg_a = np.a
	} ;

	-- adverbs after
    AdvVP v a = {verb = v.verb ; adv = v.adv ++ a.s ; pprs = v.pprs ; erg_a = v.erg_a} ;

}
