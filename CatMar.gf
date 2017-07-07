concrete CatMar of Cat = CommonX - [Temp,Tense,Pol,Ant,AdA] ** open ResMar, Prelude in {

  flags coding = utf8;

  lincat
    S  = {s : Str} ; 
    Cl = {s :  Bool => TTense => Str } ; 
		NP = ResMar.NP ;
		VP = ResMar.VP ;
		VPSlash = ResMar.VP ;
    ListNP = {s : Case => Str ; a : Agr } ;
    AP = {s : Gender => Number => Case => Str} ;
    CN = {s : Number => Case => Str; g : Gender } ;
    Det = {s : Gender => Case => Str ; n : Number} ;
    N = {s : Number => Case => Str; g : Gender } ; 
    A = {s : Gender => Number => Case => Str} ;
    V = {s : Bool => VForm => Str} ;
		Adv = {s : Str} ;
    V2 = Verb ** {c : Case} ;
    --V2 = TransVerb ;
    AdA = {s : Str} ; 
    Pol = {s : Str ; b : Bool} ;
    Tense = {s : Str ; t : TTense} ;
    Temp = {s : Str ; t : TTense ; a : Anteriority} ;
    Ant = {s : Str ; a : Anteriority} ;
    Conj = {s1, s2 : Str} ;    

    -- for structural
    Pron = {s : Case => Str ; a : Agr} ;
    Det = {s : Gender => Case => Str ; n : Number} ;

    -- dadadadum
    Prep = {s : Str} ;
}

