unit TestInstructionParser;

interface
uses TestFramework, InstructionParser;

type TestTInstructionParser = class(TTestCase)
  published
    procedure TestMakeInstruction;
    procedure TestImportInstruction;
    procedure TestCallInstruction;
    procedure TestListOfInstructions;
    procedure TestCallWithArguments;
  protected
    procedure SetUp; override;
  private
    Parser : TInstructionParser;
end;


implementation

uses SlimDirective, Instruction;

{ TestTSlimInstructionParser }

procedure TestTInstructionParser.SetUp;
begin
  Parser := TInstructionParser.Create;
end;


procedure TestTInstructionParser.TestMakeInstruction;
var
  directive : TSlimDirective;
  instruction : TInstruction;
begin
  directive := TSlimDirective.ListWith('id_0').Add('make')
    .Add('instance').Add('AnyClass').Add('argument');

  instruction := Parser.Parse(directive);

  CheckIs(instruction, TMakeInstruction);
  CheckEquals('id_0', Instruction.Id);
  CheckEquals('instance', (instruction as TMakeInstruction).InstanceName);
  CheckEquals('AnyClass', (instruction as TMakeInstruction).ClassToMake);
end;

procedure TestTInstructionParser.TestImportInstruction;
var
  directive : TSlimDirective;
  instruction : TInstruction;
begin
  directive := TSlimDirective.ListWith('id_import').Add('import').Add('a path');

  instruction := Parser.Parse(directive);

  CheckIs(instruction, TImportInstruction);
  CheckEquals('id_import', Instruction.Id);
  CheckEquals('a path', (instruction as TImportInstruction).Path);
end;

procedure TestTInstructionParser.TestCallInstruction;
var
  directive : TSlimDirective;
  instruction : TInstruction;
begin
  directive := TSlimDirective.ListWith('id_call').Add('call').Add('instance').Add('function');

  instruction := Parser.Parse(directive);

  CheckIs(instruction, TCallInstruction);
  CheckEquals('id_call', Instruction.Id);
  CheckEquals('instance', (instruction as TCallInstruction).InstanceName);
  CheckEquals('function', (instruction as TCallInstruction).FunctionName);
end;

procedure TestTInstructionParser.TestCallWithArguments;
var
  directive : TSlimDirective;
  instruction : TInstruction;
  callInstruction : TCallInstruction;
begin
  directive := TSlimDirective.ListWith('id').Add('call').Add('inst').Add('fct').Add('arg1').Add('arg2');

  instruction := Parser.Parse(directive);

  callInstruction := instruction as TCallInstruction;
  CheckNotNull(callInstruction.Arguments);
  CheckEquals(2, callInstruction.Arguments.Count);
end;

procedure TestTInstructionParser.TestListOfInstructions;
var
  directive : TSlimDirective;
  instruction : TInstruction;
begin
  directive := TSlimDirective.Create;
  directive.Add(TSlimDirective.ListWith('id_make').Add('make').Add('instance').Add('fixture'));
  directive.Add(TSlimDirective.ListWith('id_call').Add('call').Add('instance').Add('f'));

  instruction := Parser.Parse(directive);

  CheckIs(instruction, TListInstruction);
  CheckEquals(2, instruction.Length);

  CheckEquals('id_make', instruction.GetItem(0).Id);
  CheckEquals('id_call', instruction.GetItem(1).Id);
end;

initialization
  RegisterTest(TestTInstructionParser.Suite);
end.
