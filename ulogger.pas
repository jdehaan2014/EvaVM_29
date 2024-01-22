unit uLogger;

{$mode ObjFPC}{$H+}
{$ModeSwitch implicitfunctionspecialization}

interface

uses
  SysUtils, uValue;

procedure ErrorLogMsg(const Text: String);
procedure Log(Value: Byte);
procedure Log(Value: Double);
procedure Log(Value: String);
procedure Log(Value: TValue);

implementation

procedure ErrorLogMsg(const Text: String);
begin
  WriteLn(Format('Fatal error: %s.', [Text]));
end;

procedure Log(Value: Byte);
begin
  WriteLn('Value = <', IntToHex(Value), '>');
end;

procedure Log(Value: Double);
begin
  WriteLn('Value = ', Value.ToString);
end;

procedure Log(Value: String);
begin
  WriteLn('Value = ', Value);
end;

procedure Log(Value: TValue);
begin
  Write('Value = (', ValueToTypeString(Value), '): ');
  case Value.Typ of
    vtNumber: Writeln(Value.Number.ToString);
    vtBoolean: Writeln(IfThen(Value.Bool, 'true', 'false'));
    vtObject: Writeln(Value.Obj.ToString);
//    otherwise WriteLn('unknown');
  end;
end;

end.

