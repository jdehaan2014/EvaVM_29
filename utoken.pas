unit uToken;

{$mode ObjFPC}{$H+}
{$ModeSwitch advancedrecords}
{$ModeSwitch typehelpers}

interface

uses
  SysUtils;

type

  TTokenTyp = (
    ttSymbol,
    ttNumber,
    ttString,

    ttLeftParen, ttRightParen,

    ttError, ttEOF
  );

  // convert enum tokentyp to a string representation
  TTokenTypHelper = type helper for TTokenTyp
    function toString: String;
  end;



  // Keep track of token position
  TLocation = record
    Line, Col: Integer;
    constructor Create(const ALine, ACol: Integer);
    function toString: String;
  end;

  // a token holds a type, a value and a location
  TToken = record
    Typ: TTokenTyp;
    Value: String;
    Location: TLocation;
    constructor Create(ATyp: TTokenTyp; AValue: String; ALocation: TLocation);
    function toString: String;
  end;


implementation

const
  // holds string representations of the enums of tokentype
  TokenTypStr: array[TTokenTyp] of String = (
    'symbol', 'number', 'string', '(', ')', 'error', 'EOF'
  );

{ TTokenTypHelper }

function TTokenTypHelper.toString: String;
begin
  Result := TokenTypStr[Self];
end;


{ TLocation }

constructor TLocation.Create(const ALine, ACol: Integer);
begin
  Line := ALine;
  Col := ACol;
end;

function TLocation.toString: String;
begin
  Result := Format('[%d,%d]', [Line, Col]);
end;

{ TToken }

constructor TToken.Create(ATyp: TTokenTyp; AValue: String; ALocation: TLocation);
begin
  Typ := ATyp;
  Value := AValue;
  Location := ALocation;
end;

// used for debugging
function TToken.toString: String;
var StrTyp: String;
begin
  WriteStr(StrTyp, Typ);
  Result := 'Type: ' +  StrTyp + ' Value: "' + Value + '; Loc: ' + Location.toString;
end;

end.

