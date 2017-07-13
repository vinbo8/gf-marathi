resource ResMar = open Prelude, Maybe in {
  flags coding = utf8 ;

  param
      Number = Sg | Pl ;
      Case = Nom | Acc | Dat | Erg | Obl;
      Gender = Masc | Fem | Neut ;
			Animacy = Animate | Inanimate ;
      Person = P3 | P2 | P1 ;
      VForm = VInf | VPPres | VPres Gender Number Person | VPast Gender Number Person ;
      AForm = ANom Gender Number | AObl ;
      TTense = Pres | Past ;
      Anteriority = Simul ;

  oper
    Agr = {g : Gender ; n : Number ; p : Person} ;
    agr : Gender -> Number -> Person -> Agr = \g,n,p -> {g = g ; n = n ; p = p} ;

  oper
		VP	: Type = {
			verb : Verb ; 
			pprs : Str ; -- present participle (eg. basat)
			adv : Str ; -- adverb
			erg_a : Agr	-- agreement for the object in case the sent is erg
		} ;

		NP	: Type = {s : Case => Str ; a : Agr ; anim: Animacy} ;

    Noun : Type = {s : Number => Case => Str; g : Gender; anim: Animacy} ;
		PN   : Type = {s : Case => Str ; g: Gender; anim: Animacy} ;
    Adj : Type = {s : AForm => Str} ;
		-- Bool is polarity ; Case is Nom for intransitive
    Verb : Type = {s : Bool => VForm => Str ; subj_c : Case ; obj_c : Case} ;
    Prep : Type = {s : Str} ;
    Quant : Type = {s : Gender => Number => Case => Str} ;

		predV : Verb -> VP = \verb -> {
			verb = verb ;
			pprs = [] ;
			adv = [] ;
			erg_a = agr Neut Sg P3
		} ;
		
		mkPN : (s1,s2 : Str) -> Gender -> PN =
			\nom,obl,gen -> {
				s = table {
					Nom => nom ;
					Acc => obl ++ SOFT_SPACE ++ "ला" ;
					Dat => obl ++ SOFT_SPACE ++ "ला" ;
					Erg => obl ++ SOFT_SPACE ++ "ने" ;
					Obl => obl
				} ;
				g = gen ;
				anim = Animate
		} ;

		-- explicit Pl because of nonExist

    mkNoun : (s1,_,_,s4 : Str) -> Gender -> Animacy -> Noun = 
      \snom,sobl,pnom,pobl,
      gen,anim -> {
        s = table {
          Sg => table {
            Nom => snom ; Acc => sobl + "ला" ; Dat => sobl + "ला" ; Erg => sobl + "ने" ; Obl => sobl
          } ;
          Pl => table {
            Nom => pnom ; Acc => pobl + "ना" ; Dat => sobl + "ना" ; Erg => pobl + "ने" ; Obl => pobl  
          }
        } ;
        g = gen ;
        anim = anim
      } ;
      
    mkAdj : (s1,_,_,s4 : Str) -> Adj =
			\hirva,hirvi,hirve,hirvya -> {
				s = table {
					AObl => hirvya ;
					ANom Masc Sg => hirva ;
					ANom Fem Sg => hirvi ;
					ANom Neut Sg => hirve ;
					ANom Masc Pl => hirvi ;
					ANom Fem Pl => hirvya ;
					ANom Neut Pl => hirvi 
				}
			} ;

    
    regAdj : Str -> Adj =
      \hirva -> case hirva of {
        r + "ा" => mkAdj (r + "ा") (r + "्या") (r + "े") (r + "ी") 
      } ;

    nonInfAdj : Str -> Adj =
      \bavlat -> {s = \\_ => bavlat } ;
      
      		  
		mkQuant : (s1,_,_,_,_,_,_,s8 : Str) -> Quant =
			\msg,fsg,nsg,sobl,mpl,fpl,npl,pobl -> {
				s = table {
					Masc => table {
						Sg => table {
							Nom => msg ; _ => sobl
						} ;
						Pl => table {
							Nom => mpl ; _ => pobl
						}
					} ;
					Fem => table {
						Sg => table {
							Nom => fsg ; _ => sobl
						} ;
						Pl => table {
							Nom => fpl ; _ => pobl
						}
					} ;
					Neut => table {
						Sg => table {
							Nom => nsg ; _ => sobl
						} ;
						Pl => table {
							Nom => npl ; _ => pobl
						}
					}
				} ;
			} ;
			

    mkVerb : (x1,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x16 : Str) -> Case -> Case -> Verb =
        \basne,basat,basto,bastos,bastes,baste,basta,bastat,
        baslo,basle,baslas,baslis,baslat,basla,basli,baslya,sc,oc -> {
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
            } ;
          subj_c = sc ;
          obj_c = oc
          };

    regVerb : (_ : Str) -> Case -> Case -> Verb = \bas,sc,oc -> case bas of {
      _ => mkVerb (bas + "णे") (bas + "त") (bas + "तो") (bas + "तोस") (bas + "तेस") (bas + "ते") (bas + "ता") (bas + "तात")
            (bas + "लो") (bas + "ले") (bas + "लास") (bas + "लीस") (bas + "लात") (bas + "ला") (bas + "ली") (bas + "ल्या") sc oc
    } ;

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
                } ;
                subj_c = Nom ;
                obj_c = Nom
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

		conjAnim : Animacy -> Animacy -> Animacy = \xa,ya -> ya ;
    
    agrV : Verb -> Bool -> Agr -> TTense -> Str = \v,b,a,t ->
      case <t> of {
        <Pres> => v.s ! b ! VPres a.g a.n a.p ;
        <Past> => v.s ! b ! VPast a.g a.n a.p 
    } ;
    
    agrA : Adj -> Gender -> Number -> Case -> Str = \adj,g,n,c ->
			case <c> of {
				<Nom> => adj.s ! ANom g n ;
				<_>		=> adj.s ! AObl
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
			
		hargle : Maybe Animacy -> Animacy -> Animacy = \x,y ->
			case <x.exists,y> of {
				<True,y> => x.inner ;
				<False,y> => y
			} ;
			
		a_Det = mkQuant "एक" "एक" "एक" "एका" [] [] [] [] ;
		the_Det = mkQuant [] [] [] [] [] [] [] [] ;
}
