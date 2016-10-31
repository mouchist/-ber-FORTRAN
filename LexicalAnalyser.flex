import java.util.LinkedList;
import java.util.List;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.io.FileWriter;
import java.util.Collections; 
import java.util.HashMap; 
import java.util.Map;
import java.util.ArrayList;

%%
%class LexicalAnalyser
%unicode
%line
%caseless
%type Symbol
%{

	private String inputFile = null;
  
	// tracking of Identifiers and their first occurence
	Map<String, Integer> symbolTable;
	
	// keeping track of the last token
	LexicalUnit lastToken;

	// List of all tokens
	ArrayList<Symbol> tokenList;

	private void symbol(LexicalUnit type){
		symbol(type, yytext());
	}

	private void symbol(LexicalUnit type, Object val){
		tokenList.add(new Symbol(type, yyline+1, yycolumn+1, val));
		lastToken = type;
	}

	private void foundIdentifier(){
		// ignore identifier if last token was 'PROGRAM'
		if (lastToken == LexicalUnit.PROGRAM){
			lastToken = LexicalUnit.VARNAME;
			return;
		}

		Integer whatLine = symbolTable.get(yytext());
  		if ( whatLine == null ) {
      			symbolTable.put(yytext(), yyline+1);
  		}
	}

	private void removeLastEndlineToken(){
		// special case if list is empty
		if (tokenList.size() == 0){
			return;
		}
		Symbol s = tokenList.get(tokenList.size()-1);
		if (s.getType() == LexicalUnit.ENDLINE){
			tokenList.remove(s);
		}
	}

	private void printTokens(){
		for (Symbol s : tokenList){
			System.out.println(s.toString());
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
	symbolTable = new HashMap<String, Integer>();
	lastToken = null;
	tokenList = new ArrayList<Symbol>();
%init}

%eof{
	removeLastEndlineToken();
	printTokens();
	printIdentifiers();
%eof} 

LineTerminator = \r\n|\r|\n

WhiteSpace = {LineTerminator} | [ \t\f]

CommentSymbol = [cC*dD!]

Number = [0-9]+

Identifier = [:jletter:][:jletterdigit:]*

Comment = {CommentSymbol} ~{LineTerminator}

%%

/* Empty line */
^{WhiteSpace}* {LineTerminator} {}

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
	")"	{symbol(LexicalUnit.RIGHT_PARENTHESIS);}

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
	{Identifier} 	{foundIdentifier();
			symbol(LexicalUnit.VARNAME);}
	
	{LineTerminator} {symbol(LexicalUnit.ENDLINE, " ");}

}

^{Comment} 		{ /* Ignore */ }
{WhiteSpace} 	{ /* Ignore */ }
. 			{}
