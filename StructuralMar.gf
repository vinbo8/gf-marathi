concrete StructuralMar of Structural = CatMar ** open MorphoMar, ParadigmsMar, ResMar, Prelude in {

	flags optimize=all ;

	lin

	this_Det = mkDet "हा" "ही" "हे" "ह्या" Sg ;
	that_Det = mkDet "तो" "ती" "ते" "त्या" Sg ;
	these_Det = mkDet "हे" "ह्या" "ही" "ह्या" Pl ;
	those_Det = mkDet "ते" "त्या" "ती" "त्या" Pl ;

	a_Det = mkDet "एक" "एक" "एक" "एका" Sg ;
	every_Det = mkDet "दर" "दर" "दर" "दर" Sg ;
	many_Det = mkDet "खूप" "खूप" "खूप" "खूप" Pl ;
	i_Pron = pronNP "मी" "म" Masc Sg P1 ;
	youSg_Pron = pronNP "तू" "तु" Masc Sg P2 ;
	he_Pron = pronNP "तो" "त्या" Masc Sg P3 ;
	she_Pron = pronNP "ती" "ति" Fem Sg P3 ;
	we_Pron = pronNP "आम्ही" "आम्हा" Masc Pl P1 ;
	youPl_Pron = pronNP "तुम्ही" "तुम्हा" Masc Pl P2 ;
	they_Pron = pronNP "ते" "त्यां" Masc Pl P3 ;

	very_AdA = ss "खूप";    
	and_Conj = {s1 = [] ; s2 = "आणि"} ;

	or_Conj  = {s1 = [] ; s2 = "किंवा"} ;

  above_Prep = mkPr "वरती" ;
}
