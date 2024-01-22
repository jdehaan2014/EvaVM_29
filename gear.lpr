program gear;

{$mode objfpc}{$H+}

uses
  SysUtils, uVM, uValue, uLogger, uCommon;

procedure PrintHelp;
begin
  Writeln(LineEnding, 'Usage: gear [options]', LineEnding);
  Writeln(' Options: ');
  Writeln('   -e ''expression'' [-d]  : Expression to parse.');
  Writeln('   -f <file> [-d]        : File to parse.');
  Writeln('   -d                    : Disassemble code.');
  Writeln;
end;

function ReadFile(const aFileName: TFileName): String;
var
  InputFile: THandle;
  BytesRead, FileSize: LongInt;
  Buffer: String = '';
begin
  try
    InputFile := FileOpen(aFileName, fmOpenRead); // open file for reading

    if InputFile = -1 then
      begin
        WriteLnFmt('Error opening file "%s".', [aFileName]);
        Halt;
      end;

    FileSize := FileSeek(InputFile, 0, fsFromEnd); // get filesize
    SetLength(Buffer, FileSize); // initialize source text string
    FileSeek(InputFile, 0, fsFromBeginning); // reset file pointer to 0

    // read file and save contents in result string; number of bytes read is returned.
    BytesRead := FileRead(InputFile, Buffer[1], FileSize);
    if BytesRead < FileSize then
      begin
        WriteLnFmt('Error reading file "%s".', [aFileName]);
        Halt;
      end;

    Result := Buffer;
  finally
    FileClose(InputFile);  // always close the file
  end;
end;

procedure Main(const aProgram: String; const Disassembly: Boolean = False);
var
  VM: TEvaVM;
  Result: TValue;
begin
  //TTraceable.printStats;
  Result := VM.Exec(aProgram, Disassembly);

  //VM.dumpStack;

  Log(Result);

  //TTraceable.printStats;
end;

var
  Mode, Prog: String;
begin
  if ParamCount < 2 then
    begin
      PrintHelp;
      Halt;
    end;

  // expression mode
  Mode := ParamStr(1);

  // simple expression
  if Mode = '-e' then
    // program to execute
    Prog := ParamStr(2)
  else if Mode = '-f' then
    Prog := ReadFile(ParamStr(2));

  // Disassembly?
  if (ParamCount = 3) and (ParamStr(3) = '-d') then
    Main(Prog, True)
  else
    Main(Prog);

  //TTraceable.printStats;
  //Writeln('All done.');
end.


{



    '  (def createCounter ()      ' +
    '    (begin ' +
    '      (var value 0) ' +
    '      (def inc () (set value (+ value 1))) ' +
    '      inc)) ' +
    '  (var fn1 (createCounter)) ' +
    '  (fn1) ' +
    '  (fn1) ' +
    '  (fn1) ' +
    '  (var fn2 (createCounter)) ' +
    '  (fn2) ' +
    '  (+ (fn1) (fn2)) ' +
    '         '


' (var x 10) ' +       // x global
' (def foo () x) ' +  // x global
//  ' (foo) ' +
' (begin ' +
'    (var y 100) ' + // cell
'    (set y 1000) ' +
'    (var q 300) ' + // local
'    q ' +
'    (+ x y) ' +   // y: cell, x: global
'    (begin ' +
'       (var z 200) ' +
'        z ' +
'       (def bar () (+ y z)) ' +   // cell both
'       (bar))) ' +


' (def square (x) (* x x)) ' +
' (square 2) // 4 ' +
' ' +
' (def factorial (x) ' +
'   (if (= x 1) ' +
'      1 ' +
'      (* (factorial (- x 1))))) ' +
' (factorial 5) // 120 '

' (def factorial (x) ' +
'   (if (= x 1) ' +
'      1 ' +
'      (* x (factorial (- x 1))))) ' +
' (factorial 5)   ' // 120


//' (def square (x) (* x x)) ' +
' ((lambda (x) (* x x)) 2) ' +
' (var sq (lambda (x) (* x x))) ' +
' (sq 2) '

'(var x 5)' +
'(set x (+ x 10)) ' +
'x' +
'(begin' +
'  (var z 100)' +
'  (set x 1000)' +
'    (begin' +
'      (var x 200)' +
'     x)' +
'  x)' +
'x'

'(var i 10)' +
'(var count 0) ' +
'(while (> i 0)' +
'(begin' +
'  (set i (- i 1))' +
'  (set count (+ count 1))))' +
'count'


// OOP

'       '   +
'  (class Point null      ' +
'    (def constructor (self x y)    ' +
'       (begin ' +
'          (set (prop self x) x)' +
'          (set (prop self y) y)' +
'       ))   ' +
'        ' +
'    (def calc (self)    ' +
'       (+ (prop self x) (prop self y)) ' +
'       )) ' +
'        ' +
'  (class Point3D Point      ' +
'        ' +
'    (def constructor (self x y z)    ' +
'       (begin ' +
'          ((prop (super Point3D) constructor) self x y)' +
'          (set (prop self z) z))' +
'       )   ' +
'        ' +
'    (def calc (self)    ' +
'       (+ ((prop (super Point3D) calc) self) (prop self z))) ' +
'       ) ' +
'        ' +
'        ' +
'  (var p (new Point3D 10 20 30))     ' +
'       ' +
'  ((prop p calc) p)     ' + // 60
'        '

}
