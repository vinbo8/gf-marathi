resource ResMar = open Prelude, Maybe in {
  flags coding = utf8 ;

  param
      Number = Sg | Pl ;
      Case = Nom | Acc | Dat | Erg | Obl;
      Gender = Masc | Fem | Neut ;
			Animacy = Animate | Inanimate ;
      Person = P3 | P2 | P1 ;
      Polarity = Pos | Neg ;
      -- negation before/after word
      NDirection = Back | Front ;
      VForm = VInf | VPresPart Polarity | VPres Polarity Gender Number Person | VPast Polarity Gender Number Person ;
      AForm = ANom Gender Number | AObl ;
      QForm = QNom Gender Number | QObl Number ;
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
    Verb : Type = {s : VForm => Str ; subj_c : Case ; obj_c : Case} ;
    Prep : Type = {s : Str} ;
    Quant : Type = {s : QForm => Str} ;

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
      
      		  
    mkQuant : (sq,_,_,_,_,_,_,s8 : Str) -> Quant = 
			\msg,fsg,nsg,sobl,mpl,fpl,npl,pobl -> {
				s = table {
					QNom Masc Sg => msg ;
					QNom Fem Sg => fsg ;
					QNom Neut Sg => nsg ;
					QNom Masc Pl => mpl ;
					QNom Fem Pl => fpl ;
					QNom Neut Pl => npl ;
					QObl Sg => sobl ;
					QObl Pl => pobl
				}
			} ;
    

    mkVerb : (x1,_,_,_,_,_,_,_,_,_,_,_,_,_,_,x16 : Str) -> Case -> Case -> Verb =
			\basne,basat,basto,bastos,bastes,baste,basta,bastat,
			baslo,basle,baslas,baslis,baslat,basla,basli,baslya,sc,oc -> {
			s = table {
				VInf => basne ;
				
				VPresPart p => pol_adjust p Front basat ;
				
				-- imperfective forms
				VPres Pos Masc Sg P1 => basto ;
				VPres Pos Fem  Sg P1 => baste ;
				VPres Pos Neut Sg P1 => nonExist ;
				VPres Pos Masc Pl P1 => basto ;
				VPres Pos Fem  Pl P1 => basto ;
				VPres Pos Neut Pl P1 => nonExist ;
				VPres Pos Masc Sg P2 => bastos ;
				VPres Pos Fem  Sg P2 => bastes ;
				VPres Pos Neut Sg P2 => nonExist ;
				VPres Pos Masc Pl P2 => basta ;
				VPres Pos Fem  Pl P2 => basta ;
				VPres Pos Neut Pl P2 => nonExist ;
				VPres Pos Masc Sg P3 => basto ;
				VPres Pos Fem  Sg P3 => baste ;
				VPres Pos Neut Sg P3 => baste ;
				VPres Pos _    Pl P3 => bastat ;
			
				VPres Neg Neut _ P1 => nonExist ;
				VPres Neg _ _ _			=> pol_adjust Neg Front basat ;
				
				-- perfective forms
				VPast p Masc Sg P1 => pol_adjust p Back baslo ;     
				VPast p Fem  Sg P1 => pol_adjust p Back basle ;     
				VPast p Neut _  P1 => nonExist ;  
				VPast p _    Pl P1 => pol_adjust p Back baslo ;     
				VPast p Masc Sg P2 => pol_adjust p Back baslas ;    
				VPast p Fem  Sg P2 => pol_adjust p Back baslis ;    
				VPast p Neut _  P2 => nonExist ;  
				VPast p _    Pl P2 => pol_adjust p Back baslat ;    
				VPast p Masc Sg P3 => pol_adjust p Back basla ;     
				VPast p Fem  Sg P3 => pol_adjust p Back basli ;     
				VPast p Neut Sg P3 => pol_adjust p Back basle ;     
				VPast p Masc Pl P3 => pol_adjust p Back basle ;     
				VPast p Fem  Pl P3 => pol_adjust p Back baslya ;    
				VPast p Neut Pl P3 => pol_adjust p Back basli 
			} ;
			subj_c = sc ;
			obj_c = oc
		} ;

    regVerb : (_ : Str) -> Case -> Case -> Verb = \bas,sc,oc -> case bas of {
      _ => mkVerb (bas + "णे") (bas + "त") (bas + "तो") (bas + "तोस") (bas + "तेस") (bas + "ते") (bas + "ता") (bas + "तात")
            (bas + "लो") (bas + "ले") (bas + "लास") (bas + "लीस") (bas + "लात") (bas + "ला") (bas + "ली") (bas + "ल्या") sc oc
    } ;
    
    auxBe : Verb = {s = table {
			VInf => "असणे" ;
			
			VPresPart _ => nonExist ;
			
			-- present (NOT imperfective)
			VPres p _ Sg P1 => pol_adjust p Back "आहे" ;
			VPres p _ Pl P1 => pol_adjust p Back "आहोत" ;
			VPres p _ Sg P2 => pol_adjust p Back "आहेस" ;
			VPres p _ Pl P2 => pol_adjust p Back "आहात" ;
			VPres p _ Sg P3 => pol_adjust p Back "आहे" ;
			VPres p _ Pl P3 => pol_adjust p Back "आहेत" ;

			-- past (NOT perfective)
			VPast p Masc Sg P1 => merge_neg p "होतो" ;
			VPast p Fem  Sg P1 => merge_neg p "होते" ;
			VPast p Neut _  P1 => nonExist ;
			VPast p _    Pl P1 => merge_neg p "होतो" ;
			VPast p Masc Sg P2 => merge_neg p "होतास" ;
			VPast p Fem  Sg P2 => merge_neg p "होतिस" ;
			VPast p Neut _  P2 => nonExist ;
			VPast p _    Pl P2 => merge_neg p "होता" ;
			VPast p Masc Sg P3 => merge_neg p "होता" ;
			VPast p Fem  Sg P3 => merge_neg p "होती" ;
			VPast p Neut Sg P3 => merge_neg p "होतं" ;
			VPast p Masc Pl P3 => merge_neg p "होते" ;
			VPast p Fem  Pl P3 => merge_neg p "होत्या" ;
			VPast p Neut Pl P3 => merge_neg p "होती" 
			} ;
			subj_c = Nom ;
			obj_c = Nom
		} ;

    pol_adjust : Polarity -> NDirection -> Str -> Str = \p,d,s -> case p of {
			Pos => s ; Neg => case d of {
				Back => "नाही" ++ s; 
				Front => s ++ "नाही"
			}
		} ;

    merge_neg : Polarity -> Str -> Str = \p,s -> case p of {
      Pos => s ; Neg => case s of {
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
    
    agrV : Verb -> Polarity -> Agr -> TTense -> Str = \v,p,a,t ->
      case <t> of {
        <Pres> => v.s ! VPres p a.g a.n a.p ;
        <Past> => v.s ! VPast p a.g a.n a.p 
    } ;
    
    agrA : Adj -> Gender -> Number -> Case -> Str = \adj,g,n,c ->
			case <c> of {
				<Nom> => adj.s ! ANom g n ;
				<_>		=> adj.s ! AObl
		} ;
		
		agrQ : Quant -> Gender -> Number -> Case -> Str = \quant,g,n,c ->
			case <c> of {
				<Nom> => quant.s ! QNom g n ;
				<_>		=> quant.s ! QObl n
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
