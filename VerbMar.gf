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
	
	ComplSlash vp np = {
		verb = vp.verb ;
		-- will have to include agr for ergativity
		adv = vp.adv ++ np.s ! Acc ;
		pprs = vp.pprs
	} ;

	-- adverbs after
    AdvVP v a = {verb = v.verb ; adv = v.adv ++ a.s ; pprs = v.pprs} ;

}
