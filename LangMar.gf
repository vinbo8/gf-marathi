--# -path=.:../scandinavian:../abstract:../common:../api

concrete LangMar of Lang = 
  GrammarMar,
  LexiconMar
  ** {

flags startcat = S ;  unlexer = text ; lexer = text ;

} ;
