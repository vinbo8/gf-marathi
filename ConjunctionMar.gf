concrete ConjunctionMar of Conjunction = CatMar ** open ResMar, Coordination, Prelude in {

  lin 

    ConjNP conj ss = conjunctDistrTable Case conj ss ** {
      a = ss.a
    } ;

		-- fix animacy
    BaseNP x y = twoTable Case x y ** { a = conjAgr x.a y.a } ;

  lincat
    [NP] = {s1,s2 : Case => Str ; a : Agr } ;

}
