concrete ConjunctionMar of Conjunction = CatMar ** open ResMar, Coordination, Prelude in {

  lin 

    ConjNP conj ss = conjunctDistrTable Case conj ss ** {
      a = ss.a ;
			anim = ss.anim
    } ;

		-- fix animacy
    BaseNP x y = twoTable Case x y ** { a = conjAgr x.a y.a ; anim = conjAnim x.anim y.anim } ;

  lincat
    [NP] = {s1,s2 : Case => Str ; a : Agr ; anim : Animacy } ;

}
