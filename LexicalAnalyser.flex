import java.util.Collections; 
import java.util.HashMap; 
import java.util.Map;
import java.util.ArrayList;

%%
%class LexicalAnalyser
%unicode
%line
%standalone

%caseless
%ignorecase

%{

	Map<String, Integer> symbolTable;

	private void symbol(LexicalUnit type){
		System.out.println((new Symbol(type, yyline+1, yycolumn+1,yytext())).toString());
	}

	private void symbol(LexicalUnit type, Object val){
		System.out.println((new Symbol(type, yyline+1, yycolumn+1, val)).toString());
	}


	private void foundIdentifier(){
		Integer whatLine = symbolTable.get(yytext());
  		if ( whatLine == null ) {
      			symbolTable.put(yytext(), yyline+1);
  		}
	}

	private void printIdentifiers(){

		ArrayList<String> id = new ArrayList<String>(symbolTable.keySet());
		Collections.sort(id, String.CASE_INSENSITIVE_ORDER);

		System.out.println("Identifiers");
		for ( String identifier : id ) {
			System.out.println(identifier + " " + symbolTable.get(identifier));
		}
	}

%}

%init{
	System.out.println("Hello init");
	symbolTable = new HashMap<String, Integer>();
%init}

%eof{
printIdentifiers();
%eof} 

EndOfLine = "\c" ? "\n"

LineTerminator = \r\n|\r|\n
InputCharacter = [^\r\n]

WhiteSpace = {LineTerminator} | [ \t\f]

CommentSymbol = [cC*dD!]

Number = [0-9]+

Identifier = [:jletter:][:jletterdigit:]*

%%

^{WhiteSpace}* {EndOfLine} {}

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
	"INTEGER" 	{symbol(LexicalUnit.INTEGER);}

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
	{Identifier} 	{symbol(LexicalUnit.VARNAME);
			foundIdentifier();}
	
	{LineTerminator} {symbol(LexicalUnit.ENDLINE, " ");}

}

^{CommentSymbol} .* {EndOfLine} {}
{WhiteSpace} {}



