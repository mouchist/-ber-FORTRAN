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

LetterLower = [a-z]
LetterUpper = [A-Z]
Number = [0-9]

Identifier = ({LetterLower}|{LetterUpper})({LetterLower}|{LetterUpper}|{Number})+

%%
^{CommentSymbol} .* $ {}
{WhiteSpace} {}
