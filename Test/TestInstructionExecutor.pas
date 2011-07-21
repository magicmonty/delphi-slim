unit TestInstructionExecutor;

interface

uses TestFramework, InstructionExecutor;

type TestTInstructionExecutor = class(TTestCase)
  published
    procedure TestExecution;

end;

implementation

uses SlimDirective, MockInstruction;

{ TestTInstructionExecutor }

procedure TestTInstructionExecutor.TestExecution;
var executor : TInstructionExecutor;
  mockInstruction : TMockInstruction;
  result : TSlimDirective;
begin
  executor := TInstructionExecutor.Create;
  mockInstruction := TMockInstruction.Create('ok');

  result := executor.Execute(mockInstruction);

  Check(mockInstruction.Executed, 'Instruction not executed');
  CheckSame(mockInstruction.ExecutionResult, result);
end;


initialization
  RegisterTest(TestTInstructionExecutor.Suite);
end.
