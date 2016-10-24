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

Comments = [^] {CommentSymbol} {InputCharacter}* {LineTerminator}

LetterLower = [a-z]
LetterUpper = [A-Z]
Number = [0-9]

Identifier = ({LetterLower}|{LetterUpper})({LetterLower}|{LetterUpper}|{Number})+

%%
{Comments} {}
{Identifier} {System.out.println(yytext());}

