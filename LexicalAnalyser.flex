import java.util.LinkedList;
import java.util.List;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.io.FileWriter;
%%
%class LexicalAnalyser
%unicode
%line
%caseless
%type Symbol
%{

  private String inputFile = null;
  /**
  * Collection contains all tokens fund in the file
  */
  private List<Symbol> symbols ;
  private Symbol symbol(LexicalUnit type) {
    return new Symbol(type, yyline+1, yycolumn+1);
  }

  private Symbol symbol(LexicalUnit type, Object value) {
    return new Symbol(type, yyline+1, yycolumn+1, value);
  }
  
  private void addSymbol(Symbol symbol)
  {
	if(symbol.getType()==LexicalUnit.ENDLINE && symbols.get(symbols.size()-1).getType() != LexicalUnit.ENDLINE) 
	{
		symbols.add(symbol);
	}
	else
	{
		symbols.add(symbol);

	}
  }
  
	
  /**
   * Runs the scanner on input files.
   *
   * This is a standalone scanner, it will print any unmatched
   * text to System.out unchanged.
   *
   * @param argv   the command line, contains the filenames to run
   *               the scanner on.
   */ 
  public static void main(String argv[]) {
    if (argv.length == 0) {
      System.out.println("Usage : java LexicalAnalyser [ --encoding <name> ] <inputfile(s)>");
    }
    else {
      int firstFilePos = 0;
      String encodingName = "UTF-8";
      if (argv[0].equals("--encoding")) {
        firstFilePos = 2;
        encodingName = argv[1];
        try {
          java.nio.charset.Charset.forName(encodingName); // Side-effect: is encodingName valid? 
        } catch (Exception e) {
          System.out.println("Invalid encoding '" + encodingName + "'");
          return;
        }
      }
      for (int i = firstFilePos; i < argv.length; i++) {
        LexicalAnalyser scanner = null;
		
        try {
		  
          java.io.FileInputStream stream = new java.io.FileInputStream(argv[i]);
          java.io.Reader reader = new java.io.InputStreamReader(stream, encodingName);
          scanner = new LexicalAnalyser(reader);
		  scanner.inputFile = argv[i];
          while ( !scanner.zzAtEOF ) scanner.yylex();
        }
        catch (java.io.FileNotFoundException e) {
          System.out.println("File not found : \""+argv[i]+"\"");
        }
        catch (java.io.IOException e) {
          System.out.println("IO error scanning file \""+argv[i]+"\"");
          System.out.println(e);
        }
        catch (Exception e) {
          System.out.println("Unexpected exception:");
          e.printStackTrace();
        }
		
      }
    }
  
  }
	
%}

%init{
  symbols = new LinkedList();
%init}

%eof{

for(int i=0 ; i< symbols.size(); i++)
{
	System.out.println(symbols.get(i).toString());
}
%eof}


LineTerminator = \r|\n|\r\n
InputCharacter = [^\r\n]

WhiteSpace = {LineTerminator} | [ \t\f]

CommentSymbol = [cC*dD!]
Comment =  {CommentSymbol} {WhiteSpace}* {InputCharacter}*  {LineTerminator}
INTEGER= 0 | [1-9][0-9]*

VARNAME = [:jletter:] [:jletterdigit:]*

%%

^{WhiteSpace}*{LineTerminator}                   { /* empty lines to ignore */ }

<YYINITIAL> {
	
	/* keywords */
	"PROGRAM"	{addSymbol(symbol(LexicalUnit.PROGRAM,yytext()));}
	"INTEGER"	{addSymbol(symbol(LexicalUnit.INTEGER,yytext()));}
	"END"		{addSymbol(symbol(LexicalUnit.END,yytext()));}
	"IF"		{addSymbol(symbol(LexicalUnit.IF,yytext()));}
	"THEN"		{addSymbol(symbol(LexicalUnit.THEN,yytext()));}
	"ENDIF"		{addSymbol(symbol(LexicalUnit.ENDIF,yytext()));}
	"ELSE"		{addSymbol(symbol(LexicalUnit.ELSE,yytext()));}
	"DO"		{addSymbol(symbol(LexicalUnit.DO,yytext()));}
	"ENDDO"		{addSymbol(symbol(LexicalUnit.ENDDO,yytext()));}
	"PRINT*"	{addSymbol(symbol(LexicalUnit.PRINT,yytext()));}	
	"READ*"		{addSymbol(symbol(LexicalUnit.READ,yytext()));} 
	/* seperators */
	","		{addSymbol(symbol(LexicalUnit.COMMA,yytext()));}
	"("		{addSymbol(symbol(LexicalUnit.LEFT_PARENTHESIS,yytext()));}
	")"		{addSymbol(symbol(LexicalUnit.RIGHT_PARENTHESIS,yytext()));}
	/* operators */
	"="			{addSymbol(symbol(LexicalUnit.EQUAL,yytext()));}
	"-" 		{addSymbol(symbol(LexicalUnit.MINUS,yytext()));}
	"+"	 		{addSymbol(symbol(LexicalUnit.PLUS,yytext()));}
	"*"			{addSymbol(symbol(LexicalUnit.TIMES,yytext()));}
	"/"			{addSymbol(symbol(LexicalUnit.DIVIDE,yytext()));}
	".NOT."		{addSymbol(symbol(LexicalUnit.NOT,yytext()));}
	".AND."		{addSymbol(symbol(LexicalUnit.AND,yytext()));}
	".OR."		{addSymbol(symbol(LexicalUnit.OR,yytext()));}
	".EQ."		{addSymbol(symbol(LexicalUnit.EQUAL_COMPARE,yytext()));}
	".GE."		{addSymbol(symbol(LexicalUnit.GREATER_EQUAL,yytext()));}
	".GT."		{addSymbol(symbol(LexicalUnit.GREATER,yytext()));}
	".LE."		{addSymbol(symbol(LexicalUnit.SMALLER_EQUAL,yytext()));}
	".LT."		{addSymbol(symbol(LexicalUnit.SMALLER,yytext()));}
	".NE."		{addSymbol(symbol(LexicalUnit.DIFFERENT,yytext()));}
	{INTEGER}	{addSymbol(symbol(LexicalUnit.NUMBER,yytext()));}
	{VARNAME}              { addSymbol(symbol(LexicalUnit.VARNAME,yytext())); }
	{LineTerminator}       { addSymbol(symbol(LexicalUnit.ENDLINE," ")); }

}

^ {Comment}  { /* ignore */ }
{WhiteSpace} { /* ignore */ }
. 			 {}