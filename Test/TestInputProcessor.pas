unit TestInputProcessor;

interface

uses TestFramework, InputProcessor, SlimDirective, InstructionExecutor, Instruction, InstructionParser, SlimContext;

type TMockInstructionExecutor = class(TInstructionExecutor)
  public
    Response : TSlimDirective;
    LastInstruction : TInstruction;
    LastContextUsed : TSlimContext;
    constructor Create;
    function Execute(instruction : TInstruction; context : TSlimContext): TSlimDirective; override;
end;

type TMockInstructionParser = class(TInstructionParser)
  public
    DirectiveParsed : TSlimDirective;
    InstructionToReturn : TInstruction;
    function Parse(directive: TSlimDirective): TInstruction; override;
end;

type
  TestTInputProcessor = class(TTestCase)
    published
      procedure TestBye;
      procedure TestDontDisconnectOnOtherInputs;
      procedure TestInputIsWellProcessed;
      procedure TestContextIsGivenToInstructionExecutor;
    protected
      procedure SetUp; override;
    private
      MockInstructionExecutor : TMockInstructionExecutor;
      MockInstructionParser : TMockInstructionParser;
      Processor : TInputProcessor;
  end;

implementation

{ TestTInputProcessor }

procedure TestTInputProcessor.SetUp;
begin
  Processor := TInputProcessor.Create;
  MockInstructionExecutor := TMockInstructionExecutor.Create;
  processor.InstructionExecutor := MockInstructionExecutor;
  MockInstructionParser := TMockInstructionParser.Create;
  Processor.InstructionParser := MockInstructionParser;
end;

procedure TestTInputProcessor.TestBye;
var
  response : TResponse;
begin
  response := processor.Process('000003:bye', nil);

  CheckNotNull(response, 'I need a response');
  Check(response.MustDisconnect, 'The client must be disconnected');
  CheckNull(mockInstructionParser.DirectiveParsed, 'No directive should be parsed');
  CheckNull(MockInstructionExecutor.LastInstruction);
end;


procedure TestTInputProcessor.TestDontDisconnectOnOtherInputs;
var
  response : TResponse;
begin
  response := processor.Process('hello', nil);

  CheckFalse(response.MustDisconnect, 'The client must not be disconnected');
end;

procedure TestTInputProcessor.TestInputIsWellProcessed;
var
  response : TResponse;
  instruction : TInstruction;
begin
  MockInstructionExecutor.Response := TSlimStringDirective.Create('OK');
  instruction := TInstruction.Create;
  mockInstructionParser.InstructionToReturn := instruction;

  response := Processor.Process('000005:hello', nil);

  CheckEquals('000002:OK', response.Output);
  CheckSame(instruction, MockInstructionExecutor.LastInstruction, 'The last instruction wasn''t correct');
  CheckEquals('hello', mockInstructionParser.DirectiveParsed.Value);
end;

procedure TestTInputProcessor.TestContextIsGivenToInstructionExecutor;
var context : TSlimContext;
begin
  context := TSlimContext.Create;

  Processor.Process('000005:hello', context);

  CheckSame(context, MockInstructionExecutor.LastContextUsed);
end;

{ TMockInstructionExecutor }

constructor TMockInstructionExecutor.Create;
begin
  Response := TSlimStringDirective.Create('any');
end;

function TMockInstructionExecutor.Execute(instruction : TInstruction; context : TSlimContext): TSlimDirective;
begin
  LastInstruction := instruction;
  LastContextUsed := context;
  Result := Response;
end;

{ TMockInstructionParser }

function TMockInstructionParser.Parse(directive: TSlimDirective): TInstruction;
begin
  DirectiveParsed := directive;
  Result := InstructionToReturn;
end;

initialization
  RegisterTest(TestTInputProcessor.Suite);
end.
