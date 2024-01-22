unit uVM;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}
{$ModeSwitch implicitfunctionspecialization}
{$ModeSwitch functionreferences}
{$ModeSwitch anonymousfunctions}

interface

uses
  SysUtils, uOpcodes, uValue, uParser, uCompiler, uCollector, uGlobals,
  Generics.Collections;

const
  cMaxStack = 8192;     // 8K stack
  cGCThreshold = 1024; // memory threshold after which GC is triggered

type

  // stack frame for function calls
  TFrame = record
    ra: PByte;        // return address of the caller (ip of the caller)
    bp: PValue;       // base pointer of the caller
    fn: TFunctionObj; // reference to the running function
    constructor Create(ReturnAddr: PByte; BasePointer: PValue; Func: TFunctionObj);
  end;

  TCallStack = specialize TStack<TFrame>;

type
  generic TAllocator1<T1> = function(const aArg1: T1): TValue;
  generic TAllocator2<T1, T2> = function(const aArg1: T1; const aArg2: T2): TValue;

  TEvaVM = record
    private
      ip: PByte;       // instruction pointer
      sp: PValue;      // stack pointer
      bp: PValue;      // base (frame) pointer
      Stack: array[0..cMaxStack-1] of TValue; // operands stack
      Global: TGlobal;
      Parser: TParser;
      Compiler: TCompiler; // the EVA compiler
      Collector: TCollector;  // the EVA garbage collector
      CallStack: TCallStack; // separate stack for function calls, keeps return address, etc.
      fn: TFunctionObj; // currently executing function

      class operator Initialize(var EvaVM: TEvaVM);
      class operator Finalize(var EvaVM: TEvaVM);

    public
      function Exec(const AProgram: String; const Disassembly: Boolean): TValue;
      procedure dumpStack;
    private
      procedure setGlobalVariables;
      function Eval: TValue;
      function ReadByte: Byte;
      function ReadShort: UInt16;
      function ReadConst: TValue;
      procedure Push(Value: TValue);
      function Pop: TValue;
      procedure BinaryOp(const Op: Byte);
      function toAddress(const Index: UInt16): PByte;
      function Peek(const OffSet: Integer = 0): TValue;
      procedure PopN(const Count: Integer);
      // Garbage collection operations
      function getStackGCRoots: specialize THashSet<TTraceable>;
      function getConstantGCRoots: specialize THashSet<TTraceable>;
      function getGlobalGCRoots: specialize THashSet<TTraceable>;
      function getGCRoots: specialize THashSet<TTraceable>;
      procedure maybeGC;
      generic function MEM<T1>(allocator: specialize TAllocator1<T1>; const aValue1: T1): TValue;
      generic function MEM<T1, T2>(allocator: specialize TAllocator2<T1, T2>; const aValue1: T1; const aValue2: T2): TValue;
  end;


implementation
uses uLogger, uAST;


generic function CompareValues<T>(const Op: Byte; V1, V2: T): Boolean;
begin
  case Op of
    0: Result := V1 < V2;
    1: Result := V1 > V2;
    2: Result := V1 = V2;
    3: Result := V1 >= V2;
    4: Result := V1 <= V2;
    5: Result := V1 <> V2;
  end;
end;

{ TFrame }

constructor TFrame.Create(ReturnAddr: PByte; BasePointer: PValue;
  Func: TFunctionObj);
begin
  ra := ReturnAddr;
  bp := BasePointer;
  fn := Func;
end;


{ EvaVM }

class operator TEvaVM.Initialize(var EvaVM: TEvaVM);
begin
  EvaVM.CallStack := TCallStack.Create;
  EvaVM.setGlobalVariables;
end;

class operator TEvaVM.Finalize(var EvaVM: TEvaVM);
begin
  EvaVM.CallStack.Free;
  TTraceable.Cleanup;
end;

// initialize global variables and function
procedure TEvaVM.setGlobalVariables;
begin

  Global.addNativeFunction('sqr', 1,
    function (Value: PValue): TValue
      begin Result := NumVal(sqr(asNum(Value[0]))) end);

  Global.addNativeFunction('sqrt', 1,
    function (Value: PValue): TValue
      begin Result := NumVal(sqrt(asNum(Value[0]))) end);

  Global.addNativeFunction('print', 1,
    function (Value: PValue): TValue
      begin Writeln(ValueToString(Value[0])); Result := NumVal(0) end);

  //Global.addNativeFunction('sum', 2,
  //  function (Value: PValue): TValue
  //    begin Result := NumVal(asNum(Value[0]) + asNum(Value[1])) end);

  Global.addConstant('version', 1);
end;

function TEvaVM.Exec(const AProgram: String; const Disassembly: Boolean): TValue;
var
  AST: TExpr;
begin
  try
    // 1. Parse the program
    Parser := TParser.Create('(begin ' + AProgram + ')');
    AST := Parser.Parse;
    //AST.Print;

    // 2. Compile the program to byte code
    Compiler.Init(@Global);  // use address here, since compiler needs pointer to Global
    Compiler.Compile(AST);
    fn := Compiler.getMainFunction;  // start from the main entry point

    // Set instruction pointer
    ip := @fn.CO.Code.Data[0]; // set instruction pointer to the start of the code
    sp := @Stack[0];        // set stack pointer to start of the stack
    bp := @Stack[0];        // init the base pointer to beginning of the stack

    // Debug disassembly
    if Disassembly then
      Compiler.DisassembleBytecode;

    Result := Eval;

  finally
    AST.Free;
  end;
end;

function TEvaVM.Eval: TValue;
var
  Opcode, Op, GlobalIndex, LocalIndex, Count, argsCount, cellIndex,
    cellCount: Byte;
  Operand1, Operand2, Value, fnResult, fnValue, Instance, ctorValue,
    Obj: TValue;
  Num1, Num2: Double;
  S1, S2: String;
  Condition: Boolean;
  Address: UInt16;
  Callee, newfn: TFunctionObj;
  CallerFrame: TFrame;
  CO: TCodeObj;
  i: Integer;
  classObject: TClassObj;
  Prop: ShortString;
  InstanceObj: TInstanceObj;
begin
  while True do
    begin
      //dumpStack;
      Opcode := ReadByte;
      case Opcode of
        OP_HALT:
          Exit(Pop);
        OP_CONST:
          Push(ReadConst);
        OP_ADD..OP_DIV:
          BinaryOp(Opcode);
        OP_COMPARE:
          begin
            Op := ReadByte;
            Operand2 := Pop;
            Operand1 := Pop;
            if isNum(Operand1) and isNum(Operand2) then
              begin
                Num1 := asNum(Operand1);
                Num2 := asNum(Operand2);
                Push(BoolVal(CompareValues(Op, Num1, Num2)));
              end
            else if isString(Operand1) and isString(Operand2) then
              begin
                S1 := asPasString(Operand1);
                S2 := asPasString(Operand2);
                Push(BoolVal(CompareValues(Op, S1, S2)));
              end
          end;
        OP_JUMP_IF_FALSE:
          begin
            Condition := asBool(Pop);
            Address := ReadShort;
            if not Condition then
              ip := toAddress(Address);
          end;
        OP_JUMP:
          ip := toAddress(ReadShort);
        OP_GET_GLOBAL:
          begin
            GlobalIndex := ReadByte;
            Push(Global.get(GlobalIndex).Value);
          end;
        OP_SET_GLOBAL:
          begin
            GlobalIndex := ReadByte;
            //Value := Peek(0);
            Value := Pop;
            Global.put(GlobalIndex, Value);
          end;
        OP_POP:
          Pop;
        OP_GET_LOCAL:
          begin
            LocalIndex := ReadByte;
            //if (LocalIndex < 0) or (LocalIndex >= Length(Stack)) then
            //  begin
            //    Writeln('OP_GET_LOCAL: Invalid variable index: ', LocalIndex);
            //    Halt(64);
            //  end;
            Push(bp[LocalIndex]);   // base pointer
          end;
        OP_SET_LOCAL:
          begin
            LocalIndex := ReadByte;
            Value := Peek(0);
            //if (LocalIndex < 0) or (LocalIndex >= Length(Stack)) then
            //  begin
            //    Writeln('OP_SET_LOCAL: Invalid variable index: ', LocalIndex);
            //    Halt(64);
            //  end;
            bp[LocalIndex] := Value;
          end;
        // Scope exit: clean up variables
        // note: variables sit right below the result of a block
        // so we move the result below, which will be the new top
        // after popping the variables
        OP_SCOPE_EXIT:
          begin
            Count := ReadByte;  // number of variables to pop
            // Move the result above the vars
            (sp - 1 - Count)^ := Peek(0);
            // Pop the vars
            PopN(Count);
          end;
        OP_CALL:
          begin
            argsCount := ReadByte;
            Value := Peek(argsCount); // function itself
            // native functions
            if isNative(Value) then
              begin
                // call with pointer to first argument: sp-argsCount
                fnResult := asNative(Value).Func(sp - argsCount);
                // Pop args + function
                PopN(argsCount + 1);
                // push result back to stack
                Push(fnResult);
              end
            else
              // user defined functions
              begin
                Callee := asFunction(Value);
                CallStack.Push(TFrame.Create(ip, bp, fn));
                // to access locals etc
                fn := Callee;
                // Shrink the cells vector to the size of only free vars, since
                // other own cells could be reallocated for each invocation:
                fn.Cells.Capacity := fn.CO.freeCount;
                // set the base pointer for the callee
                bp := sp - argsCount - 1;
                // jump to the function code
                ip := @Callee.CO.Code.Data[0];

              end;
          end;
        OP_RETURN:
          begin
            CallerFrame := CallStack.Peek; // get top of stack
            // restore ip, bp, fn for caller
            ip := CallerFrame.ra;
            bp := CallerFrame.bp;
            fn := CallerFrame.fn;
            CallStack.Pop;
          end;
        OP_GET_CELL:
          begin
            cellIndex := ReadByte;
            Push(fn.Cells[cellIndex].Value);
          end;
        OP_SET_CELL:
          begin
            cellIndex := ReadByte;
            Value := Peek(0);
            // Allocate the cell if it's not there yet
            if fn.Cells.Count <= cellIndex then
              begin

                fn.Cells.Add(asCell(MEM(@AllocCell, Value)));
                //fn.Cells.Add(asCell(AllocCell(Value)))
              end
            else
              fn.Cells[cellIndex].Value := Value;
          end;
        OP_LOAD_CELL:
          begin
            cellIndex := ReadByte;
            Push(CellVal(fn.Cells[cellIndex]));
          end;
        OP_MAKE_FUNCTION:
          begin
            CO := asCode(Pop);
            cellCount := ReadByte;

            fnValue := MEM(@AllocFunction, CO);

            newfn := asFunction(fnValue);
            // capture
            for i := 0 to cellCount - 1 do
              newfn.Cells.Add(asCell(Pop));
            Push(fnValue);
          end;
        OP_NEW:
          begin
            classObject := asClass(Pop);
            Instance := MEM(@AllocInstance, classObject);
            // push the constructor
            ctorValue := classObject.getProp('constructor');
            Push(ctorValue);
            // and the instance
            Push(Instance);
            // note: the code for constructor parameters is generated at compile time
            // followed by OP_CALL
          end;
        OP_GET_PROP:
          begin
            Prop := asPasString(ReadConst);
            Obj := Pop;
            if isInstance(Obj) then
              // dynamic lookup in the inheritance chain
              Push(asInstance(Obj).getProp(Prop))
            else if isClass(Obj) then
              Push(asClass(Obj).getProp(Prop))
            else
              begin
                Writeln('[VM]: Unknown object for OP_GET_PROP: ', Prop);
                Halt;
              end;
          end;
        OP_SET_PROP:
          begin
            Prop := asPasString(ReadConst);
            InstanceObj := asInstance(Pop); // todo: add classes
            Value := Pop;
            InstanceObj.Properties.AddOrSetValue(Prop, Value);
            Push(Value);
          end;
        otherwise
          ErrorLogMsg('Unknown opcode: ' + IntToStr(Opcode));
          Halt;
      end;
    end;
end;

// Reads the current byte and advances the instruction pointer
function TEvaVM.ReadByte: Byte;
begin
  Result := ip^;
  Inc(ip);
end;

function TEvaVM.ReadShort: UInt16;
begin
  Inc(IP, 2);
  Result := (IP[-2] shl 8) or IP[-1];
end;

function TEvaVM.ReadConst: TValue;
begin
  Result := fn.CO.Constants[ReadByte];
end;

procedure TEvaVM.Push(Value: TValue);
begin
  if sp - PValue(@Stack[0]) = cMaxStack then
    begin
      ErrorLogMsg('Stack overflow.');
      Halt;
    end;
  sp^ := Value;
  inc(sp);
end;

function TEvaVM.Pop: TValue;
begin
  if sp - PValue(@Stack[0]) = 0 then
    begin
      ErrorLogMsg('Pop(): Empty Stack.');
      Halt;
    end;
  dec(sp);
  Result := sp^;
end;

procedure TEvaVM.BinaryOp(const Op: Byte);
var
  Op1, Op2: TValue;
  s1, s2: String;
begin
  Op2 := Pop; // second operand is on the top of the stack
  Op1 := Pop; // followed by the first operand
  case Op of
    OP_ADD:
      begin
        if isNum(Op1) and isNum(Op2) then
          Push(NumVal(AsNum(Op1) + AsNum(Op2)))
        else if isString(Op1) and isString(Op2) then
          begin
            s1 := asPasString(Op1);
            s2 := asPasString(Op2);

            Push(MEM(@AllocString, s1 + s2));
          end;
      end;
    OP_SUB: Push(NumVal(AsNum(Op1) - AsNum(Op2)));
    OP_MUL: Push(NumVal(AsNum(Op1) * AsNum(Op2)));
    OP_DIV: Push(NumVal(AsNum(Op1) / AsNum(Op2)));
  end;
end;

function TEvaVM.toAddress(const Index: UInt16): PByte; inline;
begin
  Result := @fn.CO.Code.Data[Index];
end;

function TEvaVM.Peek(const OffSet: Integer): TValue;
begin
  if sp - PValue(@Stack[0]) = 0 then
    begin
      ErrorLogMsg('Peek(): Empty Stack.');
      Halt;
    end;

  Result := (sp - 1 - OffSet)^
end;

procedure TEvaVM.PopN(const Count: Integer);
begin
  if sp - PValue(@Stack[0]) = 0 then
    begin
      Writeln('PopN(): Empty Stack.');
      Halt;
    end;
  sp := sp - Count;
end;


// return stack GC roots
function TEvaVM.getStackGCRoots: specialize THashSet<TTraceable>;
var
  stackEntry: PValue;
begin
  Result := specialize THashSet<TTraceable>.create;
  stackEntry := sp;
  while stackEntry <> @Stack[0] do
    begin
      if isObject(stackEntry^) then
        Result.Add(TTraceable(stackEntry^.Obj));
      Dec(stackEntry);
    end;
end;

// return GC roots for constants
function TEvaVM.getConstantGCRoots: specialize THashSet<TTraceable>;
begin
  Result := Compiler.getConstantObjects;
end;

// return GC roots for globals
function TEvaVM.getGlobalGCRoots: specialize THashSet<TTraceable>;
var
  i: Integer;
begin
  Result := specialize THashSet<TTraceable>.create;
  for i := 0 to Global.Globals.Count - 1 do
    if isObject(Global.Globals[i].Value) then
      Result.Add(Global.Globals[i].Value.Obj);
end;

// Obtain GC roots: variables on the stack, globals, constants
function TEvaVM.getGCRoots: specialize THashSet<TTraceable>;
begin
  // stack
  Result := specialize THashSet<TTraceable>.create;
  Result.AddRange(getStackGCRoots);

  // constant pool
  Result.AddRange(getConstantGCRoots);

  // global
  Result.AddRange(getGlobalGCRoots);
end;


// Spawns a potential GC cycle
procedure TEvaVM.maybeGC;
var
  Roots: specialize THashSet<TTraceable>;
begin
  if TTraceable.bytesAllocated < cGCThreshold then Exit;

  Roots := getGCRoots;
  if Roots.Count = 0 then Exit;

  //Writeln('-------- Before GC stats ---------');
  //TTraceable.printStats;
  Collector.GC(Roots);
  //Writeln('-------- After GC stats  ---------');
  //TTraceable.printStats;
end;

generic function TEvaVM.MEM<T1>(allocator: specialize TAllocator1<T1>; const aValue1: T1): TValue; inline;
begin
  maybeGC;
  Result := allocator(aValue1);
end;

generic function TEvaVM.MEM<T1, T2>(allocator: specialize TAllocator2<T1, T2>; const aValue1: T1; const aValue2: T2): TValue;
begin
  maybeGC;
  Result := allocator(aValue1, aValue2);
end;

procedure TEvaVM.dumpStack;
var
  csp: PValue;
begin
  Writeln(LineEnding, '---------- Stack ----------');
  if sp - PValue(@Stack[0]) = 0 then
    Writeln('(empty)')
  else
    begin
      csp := sp - 1;
      while csp >= PValue(@Stack[0]) do
        begin
          Writeln(ValueToString(csp^));
          Dec(csp);
        end;
      Writeln;
    end;
end;

end.

