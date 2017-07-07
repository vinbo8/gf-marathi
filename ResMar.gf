resource ResMar = open Prelude in {
  flags coding = utf8 ;

  param
      Number = Sg | Pl ;
      Case = Nom | Acc | Erg | Obl;
      Gender = Masc | Fem | Neut ;
			Animacy = Anim | Inan ;
      Person = P3 | P2 | P1 ;
      VForm = VInf | VPPres | VPres Gender Number Person | VPast Gender Number Person ;
      TTense = Pres | Past ;
      Anteriority = Simul ;

  oper
    Agr = {g : Gender ; n : Number ; p : Person} ;
    agr : Gender -> Number -> Person -> Agr = \g,n,p -> {g = g ; n = n ; p = p} ;

  oper
		VP	: Type = {
			verb : Verb ; 
			pprs : Str ; -- present participle (eg. basat)
			adv : Str -- adverb
		} ;
		NP	: Type = {s : Case => Str ; a : Agr } ;

--		VerbPhrase = {verb : Verb ; compl : Agr => Str ; isv2 : Bool} ;
    Noun : Type = {s : Number => Case => Str; g : Gender} ;
    Adj  : Type = {s : Gender => Number => Case => Str} ;
		-- Bool is polarity
    Verb : Type = {s : Bool => VForm => Str} ;
    Prep : Type = {s : Str} ;

		predV : Verb -> VP = \verb -> {
			verb = verb ;
			pprs = [] ;
			adv = []
		} ;
		
    mkNoun : (s1,_,_,s4 : Str) -> Gender -> Noun = 
      \snom,sobl,pnom,pobl,
      gen -> {
        s = table {
          Sg => table {
            Nom => snom ; Acc => sobl + "ला" ; Erg => sobl + "ने" ; Obl => sobl
          } ;
          Pl => table {
            Nom => pnom ; Acc => pobl + "ना" ; Erg => pobl + "ने" ; Obl => pobl  
          }
        } ;
        g = gen
      } ;

    mkPrep : Str -> Prep =
      \s -> {s = s} ;
	
		--mkAdvTemp : Str -> Adv =
			--\s -> {s = s} ;

    mkAdj : (s1,_,_,s4 : Str) -> Adj = 
      \hirva,hirvi,hirve,hirvya -> {
        s = table {
          Masc => table {
            Sg => table {
              Nom => hirva ; _ => hirvya
            }; 
            Pl => table {
              Nom => hirve ; _ => hirvya
            }
          } ;
          Fem => table {
            Sg => table {
              Nom => hirvi ; _ => hirvya
            } ;
            Pl => table {
              Nom => hirvya ; _ => hirvya
            }
          } ;
          Neut => table {
            Sg => table {
              Nom => hirve ; _ => hirvya    
            } ;
            Pl => table {
              Nom => hirvi ; _ => hirvya
            } 
          } 
        } ; 
      } ; 
    
    regAdj : Str -> Adj =
      \hirva -> case hirva of {
        r + "ा" => mkAdj (r + "ा") (r + "्या") (r + "े") (r + "ी") 
      } ;

    nonInfAdj : Str -> Adj =
      \bavlat -> {s = \\_,_,_ => bavlat } ;

    mkVerb : (x1,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x16 : Str) -> Verb =
        \basne,basat,basto,bastos,bastes,baste,basta,bastat,
        baslo,basle,baslas,baslis,baslat,basla,basli,baslya -> {
        s = table {
              True => table {
                VInf => basne ;
				VPPres => basat ;
				
                VPres Masc Sg P1 => basto ;
                VPres Fem  Sg P1 => baste ;
                VPres Neut Sg P1 => nonExist ;
                VPres Masc Pl P1 => basto ;
                VPres Fem  Pl P1 => basto ;
                VPres Neut Pl P1 => nonExist ;
                VPres Masc Sg P2 => bastos ;
                VPres Fem  Sg P2 => bastes ;
                VPres Neut Sg P2 => nonExist ;
                VPres Masc Pl P2 => basta ;
                VPres Fem  Pl P2 => basta ;
                VPres Neut Pl P2 => nonExist ;
                VPres Masc Sg P3 => basto ;
                VPres Fem  Sg P3 => baste ;
                VPres Neut Sg P3 => baste ;
                VPres _    Pl P3 => bastat ;

                VPast Masc Sg P1 => baslo ;     
                VPast Fem  Sg P1 => basle ;     
                VPast Neut _  P1 => nonExist ;  
                VPast _    Pl P1 => baslo ;     
                VPast Masc Sg P2 => baslas ;    
                VPast Fem  Sg P2 => baslis ;    
                VPast Neut _  P2 => nonExist ;  
                VPast _    Pl P2 => baslat ;    
                VPast Masc Sg P3 => basla ;     
                VPast Fem  Sg P3 => basli ;     
                VPast Neut Sg P3 => basle ;     
                VPast Masc Pl P3 => basle ;     
                VPast Fem  Pl P3 => baslya ;    
                VPast Neut Pl P3 => basli    
              } ;

              False => table {
                VInf => basne ;
				VPPres => basat ;

                VPres Neut _ P1 => nonExist ;
                VPres _    _ _  => neg False ++ basat ;

                VPast Masc Sg P1 => baslo ;     
                VPast Fem  Sg P1 => basle ;     
                VPast Neut _  P1 => nonExist ;  
                VPast _    Pl P1 => baslo ;     
                VPast Masc Sg P2 => baslas ;    
                VPast Fem  Sg P2 => baslis ;    
                VPast Neut _  P2 => nonExist ;  
                VPast _    Pl P2 => baslat ;    
                VPast Masc Sg P3 => basla ;     
                VPast Fem  Sg P3 => basli ;     
                VPast Neut Sg P3 => basle ;     
                VPast Masc Pl P3 => basle ;     
                VPast Fem  Pl P3 => baslya ;    
                VPast Neut Pl P3 => basli    
              }
            }
          };

    regVerb : (_ : Str) -> Verb = \bas -> case bas of {
      _ => mkVerb (bas + "णे") (bas + "त") (bas + "तो") (bas + "तोस") (bas + "तेस") (bas + "ते") (bas + "ता") (bas + "तात")
            (bas + "लो") (bas + "ले") (bas + "लास") (bas + "लीस") (bas + "लात") (bas + "ला") (bas + "ली") (bas + "ल्या")
    } ;

    transVerb : (_ : Str) -> Verb = \mar -> case mar of {
      _ =>  mkVerb (mar + "णे") (mar + "त") (mar + "तो") (mar + "तोस") (mar + "तेस") (mar + "ते") (mar + "ता") (mar + "तात")
            (mar + "ले") (mar + "ले") (mar + "ले") (mar + "ले") (mar + "ले") (mar + "ले") (mar + "ले") (mar + "ले") 
    };

    auxBe : Verb = {s = \\n => table { 
                VInf => "असणे" ;
                VPPres => nonExist ;
                
                VPres _ Sg P1 => neg n ++ "आहे" ;
                VPres _ Pl P1 => neg n ++ "आहोत" ;
                VPres _ Sg P2 => neg n ++ "आहेस" ;
                VPres _ Pl P2 => neg n ++ "आहात" ;
                VPres _ Sg P3 => neg n ++ "आहे" ;
                VPres _ Pl P3 => neg n ++ "आहेत" ;
                
                VPast Masc Sg P1 => merge n "होतो" ;
                VPast Fem  Sg P1 => merge n "होते" ;
                VPast Neut _  P1 => nonExist ;
                VPast _    Pl P1 => merge n "होतो" ;
                VPast Masc Sg P2 => merge n "होतास" ;
                VPast Fem  Sg P2 => merge n "होतिस" ;
                VPast Neut _  P2 => nonExist ;
                VPast _    Pl P2 => merge n "होता" ;
                VPast Masc Sg P3 => merge n "होता" ;
                VPast Fem  Sg P3 => merge n "होती" ;
                VPast Neut Sg P3 => merge n "होतं" ;
                VPast Masc Pl P3 => merge n "होते" ;
                VPast Fem  Pl P3 => merge n "होत्या" ;
                VPast Neut Pl P3 => merge n "होती" 
                }
              } ;

    neg : Bool -> Str = \b -> case b of {True => [] ; False => "नाही"} ;

    merge : Bool -> Str -> Str = \b -> case b of {
      True => \s -> s ; False => \s -> case s of {
				"हो" + suf => "न्हव" + suf
      } 
    } ;

    conjAgr : Agr -> Agr -> Agr = \xa,ya -> 
      case <xa.p,ya.p> of {
        <P1, _> => agr xa.g Pl P1 ;
        <_, P1> => agr xa.g Pl P1 ;
        <P2, _> => agr xa.g Pl P2 ;
        <_, P2> => agr xa.g Pl P2 ;
        <P3, _> => agr xa.g Pl P3  
    } ; 
    
    agrV : Verb -> Bool -> Agr -> TTense -> Str = \v,b,a,t ->
      case <t> of {
        <Pres> => v.s ! b ! VPres a.g a.n a.p ;
        <Past> => v.s ! b ! VPast a.g a.n a.p 
    } ;
    
    -- ergativity: subject is nominative in the present tense
    subCase : Bool -> TTense -> Case = \erg,t ->
			case erg of {
				False => Nom ;
				True => case t of {
					Pres => Nom ;
					_		 => Erg
				}
			} ;
}
