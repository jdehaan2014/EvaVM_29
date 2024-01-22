unit uScope;

{$mode delphi}{$H+}
{$ModeSwitch advancedrecords}
{$ModeSwitch implicitfunctionspecialization}

interface

uses
  SysUtils, Generics.Collections;

{
  Scope analysis pass
}

type

  // type of the scope
  TScopeType = (stGlobal, stFunction, stBlock, stClass);

  // allocation type of the scope
  TAllocType = (atGlobal, atLocal, atCell);

  PScope = ^TScope;

  TScope = record
    // type of the scope
    scopeType: TScopeType;

    // parent scope
    Parent: PScope;

    // map of allocation info
    AllocInfo: TDictionary<ShortString, TAllocType>;

    // set of free variables
    FreeVar: THashSet<ShortString>;

    // set of own cell variables
    Cells: THashSet<ShortString>;

    private
      class operator Initialize(var Scope: TScope);
      class operator Finalize(var Scope: TScope);

      function Resolve(const Name: ShortString; AllocType: TAllocType): TPair<PScope, TAllocType>;
      procedure Promote(const Name: ShortString; OwnerScope: PScope);
    public
      procedure Init(aType: TScopeType; aParent: PScope);
      procedure addLocal(const Name: ShortString);
      procedure addFree(const Name: ShortString);
      procedure addCell(const Name: ShortString);
      function getNameGetter(const Name: ShortString): Byte;
      function getNameSetter(const Name: ShortString): Byte;
      procedure maybePromote(const Name: ShortString);
  end;

implementation
uses uOpcodes;

{ TScope }

class operator TScope.Initialize(var Scope: TScope);
begin
  Scope.scopeType := stGlobal;
  Scope.Parent := Nil;
  Scope.AllocInfo := TDictionary<ShortString, TAllocType>.Create;
  Scope.FreeVar := THashSet<ShortString>.Create;
  Scope.Cells := THashSet<ShortString>.Create;
end;

class operator TScope.Finalize(var Scope: TScope);
begin
  Scope.AllocInfo.Free;
  Scope.FreeVar.Free;
  Scope.Cells.Free;
  Scope := Default(TScope);
end;

procedure TScope.Init(aType: TScopeType; aParent: PScope);
begin
  scopeType := aType;
  Parent := aParent;
end;

// register a free variable
procedure TScope.addLocal(const Name: ShortString);
begin
  //allocInfo[Name] := IfThen(Typ = stGlobal, atGlobal, atLocal);
  allocInfo.AddOrSetValue(Name, IfThen(scopeType = stGlobal, atGlobal, atLocal));
end;

// register a free variable (parent cell)
procedure TScope.addFree(const Name: ShortString);
begin
  FreeVar.Add(Name);
  //allocInfo[Name] := atCell;
  allocInfo.AddOrSetValue(Name, atCell);
end;

// register an own cell
procedure TScope.addCell(const Name: ShortString);
begin
  Cells.Add(Name);
  //allocInfo[Name] := atCell;
  allocInfo.AddOrSetValue(Name, atCell);
end;

// potentially promotes a variable from local to cell
procedure TScope.maybePromote(const Name: ShortString);
var
  initAllocType, allocType: TAllocType;
  ownerScope: PScope;
  Allocation: TPair<PScope, TAllocType>;
begin
  initAllocType := IfThen(scopeType = stGlobal, atGlobal, atLocal);

  if AllocInfo.ContainsKey(Name) then
    initAllocType := AllocInfo[Name];

  // already promoted
  if initAllocType = atCell then Exit;

  // auto [ownerScope, allocType] = resolve(name, initAllocTyp);
  Allocation := Resolve(Name, initAllocType);
  allocType := Allocation.Value;
  ownerScope := Allocation.Key;

  // update the alloca type based on resolution
  AllocInfo.AddOrSetValue(Name, allocType);

  // if we resolve it as a cell, promote to heap
  if allocType = atCell then
    Promote(Name, ownerScope);
end;

// Resolves a variable in the scope chain
// Initially, a variable is trteated as local, however if during
// the resolution we passed the own function boundary, it is
// free and hence should be promoted to a cell, unless global.
function TScope.Resolve(const Name: ShortString; AllocType: TAllocType): TPair<
  PScope, TAllocType>;
begin
  // found in the current scope
  if AllocInfo.ContainsKey(Name) then
    Exit(TPair<PScope, TAllocType>.Create(@Self, AllocType));

  // we crossed the boundary of the function and still didn't
  // resolve a local variable - further resolution should be free
  if scopeType = stFunction then
    AllocType := atCell;

  if Parent = Nil then
    begin
      Writeln('[Scope]: reference error: ', Name, ' is not defined.');
      Halt;
    end;

  // if we resolve in the global scope , the resolution is global
  if Parent^.scopeType = stGlobal then
    AllocType := atGlobal;

  Result := Parent^.Resolve(Name, AllocType);
end;

// promotes a variable from local [stack] to cell [heap]
procedure TScope.Promote(const Name: ShortString; OwnerScope: PScope);
var
  Scope: PScope;
begin
  OwnerScope^.addCell(Name);
  // Thread the variable as free in all parent scopes
  // so it's propagated down to our scope.
  Scope := @Self;
  while Scope <> OwnerScope do
    begin
      Scope^.addFree(Name);
      Scope := Scope^.Parent;
    end;
end;

// returns get opcode based on allocation type
function TScope.getNameGetter(const Name: ShortString): Byte;
begin
  case AllocInfo[Name] of
    atGlobal: Result := OP_GET_GLOBAL;
    atLocal: Result := OP_GET_LOCAL;
    atCell: Result := OP_GET_CELL;
  end;
end;

// returns set opcode based on allocation type
function TScope.getNameSetter(const Name: ShortString): Byte;
begin
  case AllocInfo[Name] of
    atGlobal: Result := OP_SET_GLOBAL;
    atLocal: Result := OP_SET_LOCAL;
    atCell: Result := OP_SET_CELL;
  end;
end;

end.

