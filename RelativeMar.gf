concrete RelativeMar of Relative = CatMar ** open ResMar, Prelude in {

  lin
		IdRP = { s = "x" } ;

		RelSlash rp cls = {
			s = \\b,t	=> rp.s ++ cls.s ! b ! t
		} ;
}
