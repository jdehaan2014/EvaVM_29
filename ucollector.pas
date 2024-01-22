unit uCollector;

{$mode delphi}{$H+}
{$PointerMath ON}

interface

uses
  SysUtils, uValue, Generics.Collections;

type

  TCollector = record
    // main collection cycle
    procedure GC(Roots: THashSet<TTraceable>);

    // marking phase (trace)
    procedure Mark(Roots: THashSet<TTraceable>);

    // return all pointers within an object
    function getPointers(Obj: TTraceable): THashSet<TTraceable>;

    // sweep phase (reclaim)
    procedure Sweep;
  end;

implementation

{ TCollector }

// main collection cycle
procedure TCollector.GC(Roots: THashSet<TTraceable>);
begin
  Mark(Roots);
  Sweep;
end;

// marking phase (trace)
procedure TCollector.Mark(Roots: THashSet<TTraceable>);
var
  WorkList: TObjectList<TTraceable>;
  Obj, p: TTraceable;
begin
  WorkList := TObjectList<TTraceable>.Create(Roots, False); // does not own objects
  while WorkList.Count <> 0 do
    begin
      Obj := WorkList.Last;
      WorkList.Delete(WorkList.Count - 1);
      if not Obj.Marked then
        begin
          Obj.Marked := True;
          for p in getPointers(Obj) do
            WorkList.Add(p);
        end;
    end;
end;

// returns all pointers within this object
function TCollector.getPointers(Obj: TTraceable): THashSet<TTraceable>;
var
  EvaValue: TValue;
  fn: TFunctionObj;
  Cell: TCellObj;
  Instance: TInstanceObj;
  Prop: TPair<ShortString, TValue>;
begin
  Result := THashSet<TTraceable>.Create;

  EvaValue := ObjVal(TObj(Obj));

  // function cells are traced
  if isFunction(EvaValue) then
    begin
      fn := asFunction(EvaValue);
      for Cell in fn.Cells do
        Result.Add(TTraceable(Cell));
    end;

  // instance properties
  if isInstance(EvaValue) then
    begin
      Instance := asInstance(EvaValue);
      for Prop in Instance.Properties do
        if isObject(Prop.Value) then
          Result.Add(TTraceable(asObject(Prop.Value)));
    end;
end;



// sweep phase (reclaim)
procedure TCollector.Sweep;
var
  Obj: TTraceable;
  i: Integer = 0;
begin
  while TTraceable.Objects[i] <> TTraceable.Objects.Last do
    begin
      Obj := TTraceable.Objects[i];
      if Obj.Marked then
        begin
          // Alive object, reset the mark for future collection cycles
          Obj.Marked := False;
          Inc(i);
        end
      else
        i := TTraceable.Objects.Remove(Obj);
    end;
end;

end.

