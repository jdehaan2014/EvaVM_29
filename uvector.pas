unit uVector;

{$mode delphi}{$H+}
{$PointerMath ON}

interface

uses
  SysUtils, fgl;

type

  // Generic vector based on record with management operators
  // usage:
  // var IntVector: TVector<Integer>;
  // begin
  //   IntVector.Add(42); // no need to create and/or free the vector itself
  // end;
  //

  TVector<T> = record
    private type PT = ^T; // define pointer to the type

    private
      fItems: PT;
      fCount: Integer;           // number of items in the vector
      fCapacity: Integer;        // capacity of the vector

      procedure setCapacity(NewCapacity: Integer); // adjust capacity

      function getItem(i: Integer): T;
      procedure setItem(i: Integer; AValue: T);

      function getFirst: PT;
      function getLast: PT;

      class operator Initialize(var Vector: TVector<T>);
      class operator Finalize(var Vector: TVector<T>);

    public
      property Items[i:Integer]: T read getItem write setItem; default;
      property Data: PT read fItems write fItems;
      property Count: Integer read fCount;
      property Capacity: Integer read fCapacity write setCapacity;
      property First: PT read getFirst;
      property Last: PT read getLast;
      function Add(aValue: T): Integer;
      procedure AddRange(Values: array of T);
      procedure InsertAt(const Index: Integer; aValue: T);
      procedure Delete(const Index: Integer);
  end;


implementation
uses uCommon;

{ GVector }

procedure TVector<T>.setCapacity(NewCapacity: Integer);
begin
  if NewCapacity = fCapacity then Exit;

  // reallocate memory, reserve heap space for new capacity
  ReAllocMem(fItems, SizeOf(T)*NewCapacity);

  // initialize the newly reserved memory space
  FillChar(fItems[fCapacity], (NewCapacity - fCapacity) * SizeOf(T), #0);
  fCapacity := NewCapacity;
end;

function TVector<T>.getItem(i: Integer): T;
begin
  // no checking on bounds, so unsafe indexing possible !!!
  Result := fItems[i];
end;

procedure TVector<T>.setItem(i: Integer; AValue: T);
begin
  // no checking on bounds, so unsafe indexing possible !!!
  fItems[i] := AValue;
end;

function TVector<T>.getFirst: PT;
begin
  If FCount = 0 then
    Result := Nil
  else
    Result := @fItems[0];
end;

function TVector<T>.getLast: PT;
begin
  If FCount = 0 then
    Result := Nil
  else
    Result := @fItems[fCount - 1];
end;

class operator TVector<T>.Initialize(var Vector: TVector<T>);
begin
  Vector.fItems := Nil;
  Vector.fCount := 0;
  Vector.fCapacity := 0;
end;

class operator TVector<T>.Finalize(var Vector: TVector<T>);
begin
  ReAllocMem(Vector.fItems, 0);
  Vector.fItems := Nil;
  Vector.fCount := 0;
  Vector.fCapacity := 0;
end;

function TVector<T>.Add(aValue: T): Integer;
begin
  if fCapacity < (fCount + 1) then
    begin
      fCapacity := IfThen<Integer>(fCapacity < 16, 16, fCapacity * 2);

      // FPC heapmanager auto keeps track of old size.
      ReAllocMem(fItems, SizeOf(T)*fCapacity);
    end;

  fItems[fCount] := AValue;
  Result := fCount;
  Inc(fCount);
end;

procedure TVector<T>.AddRange(Values: array of T);
var
  Size: Integer;
begin
  Size := Length(Values);
  if Size = 0 then Exit;

  Capacity := Capacity + Size;

  System.Move(Values[0], fItems[fCount], SizeOf(T) * Size);
  fCount := fCount + Size;
end;

procedure TVector<T>.InsertAt(const Index: Integer; aValue: T);
var
  ItemLocation: PT;
  ItemSize: Integer = SizeOf(T);
begin
  if fCapacity < (fCount + 1) then
    begin
      fCapacity := IfThen<Integer>(fCapacity < 16, 16, fCapacity * 2);

      // FPC heapmanager auto keeps track of old size.
      ReAllocMem(fItems, SizeOf(T)*fCapacity);
    end;

  ItemLocation := @fItems[Index];
  System.Move(ItemLocation^, (ItemLocation + ItemSize)^, ItemSize * (fCount - Index));

  fItems[Index] := aValue;
  Inc(fCount);
end;

procedure TVector<T>.Delete(const Index: Integer);
var
  ListItem: PT;
begin
  if (Index < 0) or (Index >= fCount) then
    begin
      WriteLnFmt('Index out of list bounds: %d.', [Index]);
      Halt;
    end;
  Dec(fCount);
  ListItem := @fItems[Index];

  System.Move(fItems[Index+1], ListItem^, SizeOf(T) * (fCount - Index));

  // Shrink the list if appropriate
  if (fCapacity > 256) and (fCount < fCapacity shr 2) then
    begin
      fCapacity := fCapacity shr 1;
      ReallocMem(fItems, fCapacity * SizeOf(T));
    end;

  //FillChar(fItems[fCount], (fCapacity - fCount) * SizeOf(T), #0);
end;

end.

