concrete NounMar of Noun = CatMar ** open Prelude, ResMar in {

  lin
    UseN n = n ;
    UsePron p = p ;

    AdjCN ap cn = {s = \\n,c => ap.s ! cn.g ! n ! c ++ cn.s ! n ! c ; g = cn.g} ;

    DetCN det cn = {
      s = \\c => det.s ! cn.g ! c ++ cn.s ! det.n ! c ; 
      a = agr cn.g det.n P3 
    } ;
}
