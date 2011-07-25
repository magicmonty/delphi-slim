unit TestInstructionExecutor;

interface

uses TestFramework, InstructionExecutor, MockInstruction;

type TestTInstructionExecutor = class(TTestCase)
  published
    procedure TestExecution;
    procedure TestExecutionWithContext;
    procedure SetUp; override;
  private
    Executor : TInstructionExecutor;
    MockInstruction : TMockInstruction;
end;

implementation

uses SlimDirective, SlimContext;

{ TestTInstructionExecutor }

procedure TestTInstructionExecutor.SetUp;
begin
  inherited;
  Executor := TInstructionExecutor.Create;
  mockInstruction := TMockInstruction.Create('ok');
end;

procedure TestTInstructionExecutor.TestExecution;
var result : TSlimDirective;
begin
  result := Executor.Execute(MockInstruction, nil);

  Check(mockInstruction.Executed, 'Instruction not executed');
  CheckSame(MockInstruction.ExecutionResult, result);
end;


procedure TestTInstructionExecutor.TestExecutionWithContext;
var context : TSlimContext;
begin
  context := TSlimContext.Create;

  Executor.Execute(MockInstruction, context);

  CheckSame(context, MockInstruction.ContextUsed);
end;

initialization
  RegisterTest(TestTInstructionExecutor.Suite);
end.
