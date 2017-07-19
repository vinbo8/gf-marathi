concrete CatMar of Cat = CommonX - [Temp,Tense,Pol,Ant,AdA] ** open ResMar, Prelude in {

  flags coding = utf8;

  lincat
    S  = {s : Str} ; 
    Cl = {s : Polarity => TTense => Str } ; 
		ClSlash = {s : Polarity => TTense => Str } ;
		NP = ResMar.NP ;
		VP = ResMar.VP ;
		VPSlash = ResMar.VP ;
    ListNP = {s : Case => Str ; a : Agr } ;
		AP = {s : AForm => Str} ;
    CN = {s : Number => Case => Str; g : Gender ; anim : Animacy } ;
    Det = {s : Gender => Case => Str ; n : Number} ;
		Quant = {s : QForm => Str} ;
		Num = {s : Str ; n : Number} ;
		PN = {s : Case => Str; g : Gender} ;
    N = {s : Number => Case => Str; g : Gender ; anim : Animacy } ; 
    PN = {s : Case => Str; g : Gender } ; 
		A = {s : AForm => Str} ;
    V = {s : VForm => Str ; subj_c : Case ; obj_c : Case} ;
		-- present in CommonX
		-- Adv = {s : Str } ;
    V2 = Verb ;
    AdA = {s : Str} ; 
    Pol = {s : Str ; p : Polarity} ;
    Tense = {s : Str ; t : TTense} ;
    Temp = {s : Str ; t : TTense ; a : Anteriority} ;
    Ant = {s : Str ; a : Anteriority} ;
    Conj = {s1, s2 : Str} ;    

    -- for structural
    Pron = {s : Case => Str ; a : Agr} ;
    Det = {s : Gender => Case => Str ; n : Number} ;

    Prep = {s : Str} ;

		-- RELATIVE
		RS = {s : Str} ;
		RCl = {s : Polarity => TTense => Str } ;
		RP = {s : Str } ;
}

