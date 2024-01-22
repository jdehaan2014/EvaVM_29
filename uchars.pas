unit uChars;

{$mode ObjFPC}{$H+}
{$ModeSwitch typehelpers}

interface

uses
  SysUtils;

const
  Null = #0;
  Bell = #7;
  BackSpace = #8;
  Tab = #9;
  VTab = #11;
  FormFeed = #12;
  CR = #13;
  Escape = #27;
  Space = #32;
  SingleQuote = #39; // single quote
  DoubleQuote = #34; // double quote

  //on Unix '^D' on windows ^Z (#26)
  {$IFDEF UNIX}
    FileEnding = ^D;
  {$ENDIF}
  {$IFDEF WINDOWS}
    FileEnding = ^Z //(#26)
  {$ENDIF}


type

  TCharHelper = type helper for Char
    function isWhiteSpace: Boolean;
    function isUnderscore: Boolean;
    function isLowCaseLetter: Boolean;
    function isUpCaseLetter: Boolean;
    function isAlpha: Boolean;
    function isDigit: Boolean;
    function isAlphaNum: Boolean;
    function isHexaDecimal: Boolean;
    function isBinaryDecimal: Boolean;
    function isOctoDecimal: Boolean;
    function isSingleQuote: Boolean;
    function isDoubleQuote: Boolean;
    function isDot: Boolean;
    function isFileEnding: Boolean;
    function isLineEnding: Boolean;
    function isNull: Boolean;
    function toUpper: Char;
    function toLower: Char;
  end;

implementation

{ TCharHelper }

function TCharHelper.isWhiteSpace: Boolean;
begin
  Result := Self in [Tab, LineEnding, Space];
end;

function TCharHelper.isUnderscore: Boolean;
begin
  Result := Self = '_';
end;

function TCharHelper.isLowCaseLetter: Boolean;
begin
  Result := Self in ['a'..'z'];
end;

function TCharHelper.isUpCaseLetter: Boolean;
begin
  Result := Self in ['A'..'Z'];
end;

function TCharHelper.isAlpha: Boolean;
begin
  Result := isLowCaseLetter or isUpCaseLetter;
end;

function TCharHelper.isDigit: Boolean;
begin
  Result := Self in ['0'..'9'];
end;

function TCharHelper.isAlphaNum: Boolean;
begin
  Result := isAlpha or isDigit;
end;

function TCharHelper.isHexaDecimal: Boolean;
begin
  Result := Self in
    ['0'..'9', 'A', 'B', 'C', 'D', 'E', 'F', 'a', 'b', 'c', 'd', 'e', 'f'];
end;

function TCharHelper.isBinaryDecimal: Boolean;
begin
  Result := Self in ['0', '1'];
end;

function TCharHelper.isOctoDecimal: Boolean;
begin
  Result := Self in ['0'..'7'];
end;

function TCharHelper.isSingleQuote: Boolean;
begin
  Result := Self = SingleQuote;
end;

function TCharHelper.isDoubleQuote: Boolean;
begin
  Result := Self = DoubleQuote;
end;

function TCharHelper.isDot: Boolean;
begin
  Result := Self = '.';
end;

function TCharHelper.isLineEnding: Boolean; inline;
begin
  Result := Self = LineEnding;
end;

function TCharHelper.isNull: Boolean;
begin
  Result := Self = #0;
end;

function TCharHelper.toUpper: Char;
begin
  Result := Upcase(Self);
end;

function TCharHelper.toLower: Char;
begin
  Result := LowerCase(Self);
end;

function TCharHelper.isFileEnding: Boolean; inline;
begin
  Result := Self = FileEnding;
end;


end.

