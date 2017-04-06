concrete AdjectiveMar of Adjective = CatMar ** open Prelude, ResMar in {

lin
  PositA adj = adj ;

  AdAP ada ap = {
    s = \\g,n,c => ada.s ++ ap.s ! g ! n ! c
  } ;
}
