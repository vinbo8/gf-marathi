concrete NounMar of Noun = CatMar ** open Prelude, ResMar in {

  lin
    UseN n = n ;
	
		UsePN pn = {
			s = \\c => pn.s ! c ;
			a = agr pn.g Sg P3 ;
			anim = Animate
		} ;

    UsePron p = p ** {anim = Animate} ;

    AdjCN ap cn = {s = \\n,c => ap.s ! cn.g ! n ! c ++ cn.s ! n ! c ; g = cn.g ; anim = cn.anim} ;

    DetCN det cn = {
      s = \\c => det.s ! cn.g ! c ++ cn.s ! det.n ! c ; 
      a = agr cn.g det.n P3 ;
			anim = cn.anim
    } ;

		MassNP cn = {
			s = \\c => cn.s ! Sg ! c ;
			a = agr cn.g Sg P3 ;
			anim = cn.anim
		} ;

		DefArt = the_Det  ;
		IndefArt = a_Det ;

		NumSg = {s = [] ; n = Sg} ;
		NumPl = {s = [] ; n = Pl} ;

		DetQuant q n = {s = \\g,c => q.s ! g ! n.n ! c ; n = n.n} ;

}
