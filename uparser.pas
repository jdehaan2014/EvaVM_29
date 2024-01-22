unit uParser;

{$mode delphi}{$H+}
//{$ModeSwitch advancedrecords}

interface

uses
  SysUtils, uAST, uLexer, uToken;

{
  BNF

  Expr = Atom
       | List .

  Atom = Number        Result := TExpr.Create(Value.toDouble);
       | String        Result := TExpr.Create(Value);
       | Symbol .      Result := TExpr.Create(Value);

  List = '(' ListEntries ')' .  Expr := ListEntries;

  ListEntries = %empty               Result := TExpr.Create(specialize TObjectList<TExpr>.Create());
              | ListEntries Expr .   ListEntries.List.Add(Expr); Result := ListEntries;
}

Type

  TParser = record
    private
      function parseAtom: TExpr;
      function parseExpr: TExpr;
      function parseList: TExpr;
      function parseListEntries: TExpr;

    public
      function Parse: TExpr;
      constructor Create(const aSource: String);

    private
      Lexer: TLexer;
      Current: TToken;
      // the current token being parsed
      function Consume: TToken;
      // diverse error routines
      procedure ErrorAt(const Location: TLocation; const Msg: string);
      procedure Error(const Msg: string);
      // the expect method expects a certain token in a given syntax
      procedure Expect(const TokenTyp: TTokenTyp; const Message: String); overload;
      procedure Expect(const TokenTyp: TTokenTyp); overload;
      // match and consume the current token
      function Match(const Expected: TTokenTyp): Boolean;
      // advance to the next token
      procedure Next;
      // determine if it is the last token in the list
      function isLastToken: Boolean; inline;

  end;

implementation
uses Generics.Collections;

const
  ErrSyntax = 'Syntax error, "%s" expected.';

{ TParser }

{ BNF is used to describe grammar rules }

// Expr ::= Atom | List
function TParser.parseExpr: TExpr;
begin
  //Writeln(Current.toString);
  if Current.Typ in [ttNumber, ttString, ttSymbol] then
    Result := parseAtom
  else
    Result := parseList;
end;

// Atom ::= Number | String | Symbol
function TParser.parseAtom: TExpr;
var
  Token: TToken;
begin
  Token := Consume;
  case Token.Typ of
    ttNumber: Result := TExpr.Create(StrToFloat(Token.Value));
    ttString: Result := TExpr.Create(Token.Value);
    ttSymbol: Result := TExpr.Create(Token.Value);
  end;
end;

// List ::= '(' ListEntries ') .
function TParser.parseList: TExpr;
begin
  Expect(ttLeftParen);
  Result := parseListEntries;
  Expect(ttRightParen);
end;

// ListEntries ::= Empty | ListEntries Expr
function TParser.parseListEntries: TExpr;
var
  Entries: TObjectList<TExpr>;
begin
  Entries := TObjectList<TExpr>.Create();
  while Current.Typ <> ttRightParen do
    Entries.Add(parseExpr);
  Result := TExpr.Create(Entries);
end;

function TParser.Parse: TExpr;
begin
  Next;
  Result := parseExpr;
end;

constructor TParser.Create(const aSource: String);
begin
  Lexer := TLexer.Create(aSource);
end;

// parser helper routines

// return the current token, and move to the next
// if any error occurs then return the EOF End-of-file token
function TParser.Consume: TToken;
begin
  Result := Current;
  Next;
end;

// write an error with the location and message
// then synchronize to synch point
procedure TParser.ErrorAt(const Location: TLocation; const Msg: string);
begin
  WriteLn('@' + Location.toString + ': ' + Msg);
  //Synchronize(SynchronizeSet); // the set of tokens to synchronize at
end;

// add an error at the current location
procedure TParser.Error(const Msg: string);
begin
  ErrorAt(Current.Location, Msg);
end;

// if the tokentype is as expected then continue to the next token
// otherwise add an error to the list and try to synchronize
procedure TParser.Expect(const TokenTyp: TTokenTyp; const Message: String);
begin
  if Current.Typ = TokenTyp then
    Next
  else
    begin
      Error(Message);
      //Synchronize(SynchronizeSet);
    end;
end;

// expect a certain tokentype. if not found then add a syntax error
procedure TParser.Expect(const TokenTyp: TTokenTyp);
begin
  Expect(TokenTyp, Format(ErrSyntax, [TokenTyp.toString]));
end;

// try to match the current token with the expected token
// if true then move to the next token
function TParser.Match(const Expected: TTokenTyp): Boolean;
begin
  Result := Current.Typ = Expected;
  if Result then
    Next;
end;

// if the token is an error token received from the lexer then report it
// to the error list and increase index again, skipping the error token.
procedure TParser.Next;
begin
  while True do
    begin
      Current := Lexer.nextToken;

      if Current.Typ <> ttError then
        Break;
      ErrorAt(Current.Location, 'Error at ' + Current.Value);
      //Synchronize(SynchronizeSet);
    end;
end;

function TParser.isLastToken: Boolean; inline;
begin
  Result := not Lexer.hasNextToken;
end;


end.

