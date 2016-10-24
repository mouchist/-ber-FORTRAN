%%
%class LexicalAnalyser
%unicode
%line
%standalone

%{
public static int counter = 1;
%} 

EndOfLine = "\c" ? "\n"

LineTerminator = \r|\n|\r\n
InputCharacter = [^\r\n]

WhiteSpace = {LineTerminator} | [ \t\f]

CommentSymbol = [cC*dD!]
Comment = [^] {CommentSymbol} {WhiteSpace} {InputCharacter}* {LineTerminator}?

LetterLower = [a-z]
LetterUpper = [A-Z]
Number = [0-9]

Identifier = ({LetterLower}|{LetterUpper})({LetterLower}|{LetterUpper}|{Number})*

%%
{Identifier} {System.out.println(counter + " Token: " + yytext()); counter++;}
{Comment} {}
{WhiteSpace} {}
