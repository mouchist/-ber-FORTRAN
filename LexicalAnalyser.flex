%%
%class LexicalAnalyser
%unicode
%line
%standalone

%{
	private void print(String s){
		System.out.println(s);
	}

	private void symbol(LexicalUnit type){
		System.out.println((new Symbol(type, yyline+1, yycolumn+1,yytext())).toString());
	}

	private void symbol(LexicalUnit type, Object val){
		System.out.println((new Symbol(type, yyline+1, yycolumn+1, val)).toString());
	}

%} 

EndOfLine = "\c" ? "\n"

LineTerminator = \r|\n|\r\n
InputCharacter = [^\r\n]

WhiteSpace = {LineTerminator} | [ \t\f]

CommentSymbol = [cC*dD!]

LetterLower = [a-z]
LetterUpper = [A-Z]

Integer = [0-9]+
Number = {Integer} "." {Integer}

Identifier = [:jletter:][:jletterdigit:]*

%%

<YYINITIAL> {
 
	/* keywords */
	"PROGRAM"	{symbol(LexicalUnit.PROGRAM);}
	"END"		{symbol(LexicalUnit.END);}
	"IF"		{symbol(LexicalUnit.IF);}
	"THEN"		{symbol(LexicalUnit.THEN);}
	"ENDIF"		{symbol(LexicalUnit.ENDIF);}
	"ELSE"		{symbol(LexicalUnit.ELSE);}
	"DO"		{symbol(LexicalUnit.DO);}
	"ENDDO"		{symbol(LexicalUnit.ENDDO);}
	"PRINT*"	{symbol(LexicalUnit.PRINT);}	
	"READ*"		{symbol(LexicalUnit.READ);} 

	/* seperators */
	","		{symbol(LexicalUnit.COMMA);}
	"("		{symbol(LexicalUnit.LEFT_PARENTHESIS);}
	")"		{symbol(LexicalUnit.RIGHT_PARENTHESIS);}

	/* operators */
	"="		{symbol(LexicalUnit.EQUAL);}
	"-"		{symbol(LexicalUnit.MINUS);}
	"+"		{symbol(LexicalUnit.PLUS);}
	"*"		{symbol(LexicalUnit.TIMES);}
	"/"		{symbol(LexicalUnit.DIVIDE);}
	".NOT."		{symbol(LexicalUnit.NOT);}
	".AND."		{symbol(LexicalUnit.AND);}
	".OR."		{symbol(LexicalUnit.OR);}
	".EQ."		{symbol(LexicalUnit.EQUAL_COMPARE);}
	".GE."		{symbol(LexicalUnit.GREATER_EQUAL);}
	".GT."		{symbol(LexicalUnit.GREATER);}
	".LE."		{symbol(LexicalUnit.SMALLER_EQUAL);}
	".LT."		{symbol(LexicalUnit.SMALLER);}
	".NE."		{symbol(LexicalUnit.DIFFERENT);}


	{Number}	{symbol(LexicalUnit.NUMBER);}
	{Identifier} 	{symbol(LexicalUnit.VARNAME);}
	{Integer} 	{symbol(LexicalUnit.INTEGER);}

	{LineTerminator} {symbol(LexicalUnit.ENDLINE, " ");}


}

^{CommentSymbol} .* {EndOfLine} {}
{WhiteSpace} {}



