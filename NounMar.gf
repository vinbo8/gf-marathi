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
}
