resource ParadigmsMar = CatMar [N,A,V] **
  open ResMar, CatMar, Prelude in {
-- module ParadigmsMar

oper
  
  masc : Gender = Masc ;
  fem : Gender = Fem ;
  neut : Gender = Neut ;
  
  Adv : Type = {s : Str} ;
  --
  -- NOUNS
  -- 
	
  mkN = overload {
    -- worst case
    mkN : (s1,_,_,s4 : Str) -> Gender -> N 
      = \mulgi,mulila,muli,mulinna,g -> lin N (mkNoun mulgi mulila muli mulinna g) ; 
      
    -- explanation:
    -- the most frequent paradigms that do not modify the stem for each gender take the nominative and the gender
    -- the most frequent paradigms that _do_ modify the stem take only the nominative and assume the gender
    -- ONLY IF the modifications are consistent for all entries
    -- others take worst case for now
      
    -- gender based defaults
    -- obvious ones based on phonemes like sutti are in the next section; no gender necessary there
    mkN : (s1 : Str) -> Gender -> N
      = \s,g -> case <s,g> of {
        <haat,masc>   => lin N (mkNoun haat (haat + "ा") haat (haat + "ां") masc) ;
        <ghar,neut>   => lin N (mkNoun ghar (ghar + "ा") (ghar + "ं") (ghar + "ां") neut) ;
        <koy,fem>     => lin N (mkNoun koy (koy + "ी") (koy + "ी") (koy + "ीं") fem)
      } ;

    -- no need for gender, obvious ones
    -- specifying gender overrides the phoneme based stuff, eg. sutti/bhikari conflict
    mkN : (s1 : Str) -> N
			= \s -> case s of {
				ba + "ई"           => lin N (mkNoun (ba + "ई") (ba + "ई") (ba + "या") (ba + "यां") fem) ;
        sutt + "ी"    => lin N (mkNoun (sutt + "ी") (sutt + "ी") (sutt + "्या") (sutt + "्यां") fem) ;
        rast + "ा"    => lin N (mkNoun (rast + "ा") (rast + "्या") (rast + "े") (rast + "्यां") masc)
      } ;
      
    -- some funky extras with nom and obl sg
    mkN : (s1,s2 : Str) -> Gender -> N
      = \nom,obl,g -> case <nom,obl> of {
        <kavi,kavi_obl>   => lin N (mkNoun kavi kavi_obl kavi (kavi_obl + "ं") g)
      }
    } ;
    
    
  --
  -- VERBS
  -- 
      
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
   
  --
  --  MINOR STUFFS
  --
   
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
    
  mkAdv : Str -> Adv
    = \s -> {s = s} ;
}
