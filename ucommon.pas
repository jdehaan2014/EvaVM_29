unit uCommon;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils;

procedure WriteFmt(const Fmt: String; const Args: array of const);
procedure WriteLnFmt(const Fmt: String; const Args: array of const);

implementation

procedure WriteFmt(const Fmt: String; const Args: array of const);
begin
  Write(Format(Fmt, Args));
end;

procedure WriteLnFmt(const Fmt: String; const Args: array of const);
begin
  WriteLn(Format(Fmt, Args));
end;


end.

