unit uLexer;

{$mode ObjFPC}{$H+}
{$ModeSwitch advancedrecords}
{$ModeSwitch arrayoperators}

interface

uses
  SysUtils, uToken;

type

  TLexer = record
    public
      constructor Create(const aSource: String);

      function nextToken: TToken;
      function hasNextToken: Boolean;
    private
      Source: String;
      lookAhead: Char;
      atEOF: Boolean;
      Index: Integer;
      Location: TLocation;
      function nextChar: Char;
      function Peek: Char;
      function PeekNext: Char;
      procedure Advance;
      procedure skipWhiteSpace;
      procedure MultiLineComment;
      procedure SingleLineComment;

      function getNumber: TToken;
      function getString: TToken;
      function getSymbol: TToken;
  end;


implementation
uses uChars;

const
  validSymbols: set of char =
    ['a'..'z', 'A'..'Z', '_', '0'..'9', '-','+','*','=','!','<','>','/'];

constructor TLexer.Create(const aSource: String);
begin
  Source := aSource;
  atEOF := False;
  Index := 1;
  Location := TLocation.Create(1, 1);
  Advance;
end;

function TLexer.nextToken: TToken;
begin
  skipWhiteSpace;
  if atEOF then
    Result := TToken.Create(ttEOF, '', Location)
  else if lookAhead.isDigit then
    Result := getNumber
  else if lookAhead.isDoubleQuote then
    Result := getString
  else if lookAhead = '/' then
    case PeekNext of
      '/': SingleLineComment;
      '*': MultiLineComment;
      otherwise
        Result := getSymbol
    end
  else if lookAhead in validSymbols then
    Result := getSymbol
  else
    begin
      case lookAhead of
        '(': Result := TToken.Create(ttLeftParen, '(', Location);
        ')': Result := TToken.Create(ttRightParen, ')', Location);
        otherwise
          Result := TToken.Create(ttError, 'Unknown symbol: ' + lookAhead, Location);;
      end;
      Advance;
    end;
end;

function TLexer.hasNextToken: Boolean;
begin
  Result := not atEOF;
end;

function TLexer.nextChar: Char;
begin
  if Index <= Length(Source) then
    begin
      Result := Source[Index];
      Inc(Index);
      Inc(Location.Col);
      if Result = LineEnding then
        begin
          Location.Col := 1;
          Inc(Location.Line);
        end;
    end
  else
    atEOF := True;
end;

function TLexer.Peek: Char;
begin
  Result := Source[Index];
end;

function TLexer.PeekNext: Char;
begin
  Result := Source[Index+1];
end;

procedure TLexer.Advance;
begin
  lookAhead := nextChar;
end;

procedure TLexer.skipWhiteSpace;
begin
  while (not atEOF) and lookAhead.isWhiteSpace do
    Advance;
end;

// single line comment starts with '//'
procedure TLexer.SingleLineComment;
begin
  // skip characters until the end of the current line
  while (Peek <> LineEnding) do
    Advance;
end;

// nested comments are allowed
procedure TLexer.MultiLineComment;
var
  Nesting: Integer = 1;
begin
  Advance;
  while Nesting > 0 do
    begin
      case Peek of
        Null:
          begin
            WriteLn('@' + Location.toString + ': ' + 'comment block not terminated');
            Exit;
          end;
        '/':
          if PeekNext = '*' then
            begin
              Advance;
              Inc(Nesting);
            end;
        '*':
          if PeekNext = '/' then
            begin
              Advance;
              Dec(Nesting);
            end;
      end;
      Advance;
    end;
end;

function TLexer.getNumber: TToken;
var
  Value: String = '';
begin
  while (not atEOF) and lookAhead.isDigit do
    begin
      Value += lookAhead;
      Advance;
    end;
  Result := TToken.Create(ttNumber, Value, Location);
end;

function TLexer.getString: TToken;
var
  Value: String = '"';
begin
  Advance;
  while (not atEOF) and (not lookAhead.isDoubleQuote) do
    begin
      Value += lookAhead;
      Advance;
    end;
  Value += lookAhead;
  Advance;
  Result := TToken.Create(ttString, Value, Location);
end;

function TLexer.getSymbol: TToken;
var
  Value: String = '';
begin
  while (not atEOF) and (lookAhead in validSymbols) do
    begin
      Value += lookAhead;
      Advance;
    end;
  Result := TToken.Create(ttSymbol, Value, Location);
end;



end.


