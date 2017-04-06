resource ParadigmsMar = CatMar [N,A,V] **
  open ResMar, CatMar, Prelude in {
-- module ParadigmsMar

oper
  
  masculine : Gender = Masc ;
  feminine : Gender = Fem ;
  neutral : Gender = Neut ;
  
  mkN = overload {
    -- worst case
    mkN : (s1,_,_,s4 : Str) -> Gender -> N 
      = \mulgi,mulila,muli,mulinna,g -> lin N (mkNoun mulgi mulila muli mulinna g) ; 
   
    -- bai, ghar
    mkN : (s1 : Str) -> Gender -> N
			= \s,g -> case s of {
				ba + "ई" => lin N (mkNoun (ba + "ई") (ba + "ई") (ba + "या") (ba + "यां") g) ;
				ghar		 => lin N (mkNoun ghar (ghar + "ा") (ghar + "ं") (ghar + "ां") g)
			} ;
		} ;

  mkPr : Str -> Prep
    = \samor -> lin Prep (mkPrep samor) ;
      
  mkA = overload {
		-- worst case
		mkA : (s1,_,_,s4 : Str) -> A 
			= \hirva,hirvi,hirve,hirvya -> lin A (mkAdj hirva hirvi hirve hirvya) ;
				
		-- regular
		mkA : (s1 : Str) -> A
			= \s -> case s of {
				hirv + "ा" => lin A (mkAdj (hirv + "ा") (hirv + "ी") (hirv + "े") (hirv + "्या")) ;
				lal					=> lin A (mkAdj lal lal lal lal)
			} ;
		} ;	
      
  mkV = overload { 
    -- irregular
    mkV : (s1,_,_,_,_,_,_,_,_,_,_,_,_,_,_,s16 : Str) -> V 
      = \basne,basat,basto,bastos,bastes,baste,basta,bastat,
        baslo,basle,baslas,baslis,baslat,basla,basli,baslya -> lin V (mkVerb basne basat basto bastos bastes baste basta bastat
                                                                      baslo basle baslas baslis baslat basla basli baslya) ; 
    -- regular
    mkV : Str -> V
      = \bas -> lin V (regVerb bas) ;

  } ;

  mkV2 = overload {
    mkV2 : Str -> V2 
      = \v -> lin V2 (transVerb v ** {c = Acc}) ;
   } ;
}
