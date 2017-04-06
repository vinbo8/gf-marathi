concrete PhraseMar of Phrase = CatMar ** open Prelude, ResMar in {
  
  lin
    PhrUtt p u v = {s = p.s ++ u.s ++ v.s} ;

    UttS s = lin Utt s ;

    NoPConj = {s = []} ;

    NoVoc = {s = []} ;
}
