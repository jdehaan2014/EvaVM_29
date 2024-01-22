unit uGlobals;

{$mode delphi}{$H+}
{$ModeSwitch implicitfunctionspecialization}

interface

uses
  SysUtils, uVector, uValue;

type

  TGlobalVar = record
    Name: ShortString;
    Value: TValue;
    constructor Create(AName: ShortString; AValue: TValue);
  end;

  PGlobal = ^TGlobal;
  TGlobal = record
    // Global variables and functions
    Globals: TVector<TGlobalVar>;

    function get(const Index: Integer): TGlobalVar;
    procedure put(const Index: Integer; Value: TValue); overload;
    procedure put(const Name: ShortString; Value: TValue); overload;
    function getGlobalIndex(const Name: ShortString): Integer;
    function Exists(const Name: ShortString): Boolean;
    procedure Define(const Name: ShortString);
    procedure addConstant(const Name: ShortString; const Value: Double);
    procedure addNativeFunction(const Name: ShortString; const Arity: Integer;
      Fn: TNativeFn);
  end;

implementation

{ TGlobalVar }

constructor TGlobalVar.Create(AName: ShortString; AValue: TValue);
begin
  Name := AName;
  Value := AValue;
end;

{ TGlobal }

function TGlobal.get(const Index: Integer): TGlobalVar;
begin
  Result := Globals[Index];
end;

procedure TGlobal.put(const Index: Integer; Value: TValue);
begin
  if Index >= Globals.Count then
    begin
      Writeln('Global ', Index, ' doesn''t exist.');
      Halt;
    end;
  Globals.Data[Index].Value := Value;
end;

procedure TGlobal.put(const Name: ShortString; Value: TValue);
var
  Index: Integer;
begin
  Index := getGlobalIndex(Name);
  Put(Index, Value);
end;

function TGlobal.getGlobalIndex(const Name: ShortString): Integer;
var
  i: Integer;
begin
  if Globals.Count > 0 then
    begin
      for i := Globals.Count - 1 downto 0 do
        if Globals[i].Name = Name then
          Exit(i);
    end;
  Result := -1;
end;

function TGlobal.Exists(const Name: ShortString): Boolean;
begin
  Result := getGlobalIndex(Name) <> -1;
end;

// Register a global
procedure TGlobal.Define(const Name: ShortString);
var
  Index: Integer;
begin
  Index := getGlobalIndex(Name);
  // already defined
  if Index <> -1 then
    Exit;

  // set to default value 0
  Globals.Add(TGlobalVar.Create(Name, NumVal(0)));
end;

procedure TGlobal.addConstant(const Name: ShortString; const Value: Double);
begin
  if not Exists(Name) then
    Globals.Add(TGlobalVar.Create(Name, NumVal(Value)));
end;

procedure TGlobal.addNativeFunction(const Name: ShortString; const Arity: Integer;
  Fn: TNativeFn);
begin
  if not Exists(Name) then
    Globals.Add(TGlobalVar.Create(Name, AllocNative(Fn, Name, Arity)));
end;

end.

