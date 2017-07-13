concrete AdjectiveMar of Adjective = CatMar ** open Prelude, ResMar in {

lin
  PositA adj = adj ;

  AdAP ada ap = {
    s = \\aform => ada.s ++ ap.s ! aform ;
  } ;
}
