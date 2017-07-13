resource MorphoMar = open Prelude, ResMar in {

	oper
		mkDet : Str -> Str -> Str -> Str -> Number -> {s : Gender => Case => Str ; n : Number} = \m,f,nt,obl,n -> {
		  s = table {
		    Masc => table {
		      Nom => m  ; _ => obl
		    } ;
		    Fem => table {
		      Nom => f  ; _ => obl
		    } ;
		    Neut => table {
		      Nom => nt ; _ => obl
		    }                  
		  } ;
		  n = n
		  } ;


		pronNP : (_,_ : Str) -> Gender -> Number -> Person -> NP = \snom,sobl,g,n,p -> {
		  s = table { Nom => snom ; Acc => sobl + "ला" ; Dat => sobl + "ला" ; Erg => sobl + "ने" ; Obl => sobl } ;
		  a = agr g n p ;
			anim = Animate
		} ;
} ;
