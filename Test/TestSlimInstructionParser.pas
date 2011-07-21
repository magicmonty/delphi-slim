unit TestSlimInstructionParser;

interface
uses TestFramework;

type TestTInstructionParser = class(TTestCase)
  published
    procedure TestMakeInstruction;
end;


implementation

uses SlimDirective, InstructionParser, Instruction;

{ TestTSlimInstructionParser }

procedure TestTInstructionParser.TestMakeInstruction;
var
  directive : TSlimDirective;
  parser : TInstructionParser;
  instruction : TInstruction;
begin
  directive := TSlimDirective.Create;
  directive.Add('id_0');
  directive.Add('make');
  directive.Add('instance');
  directive.Add('argument');

  instruction := parser.Parse(directive);

  CheckIs(instruction, TMakeInstruction);


end;

initialization
  RegisterTest(TestTInstructionParser.Suite);
end.
