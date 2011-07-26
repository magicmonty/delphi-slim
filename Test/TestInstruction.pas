unit TestInstruction;

interface

uses TestFramework, Instruction, SlimContext;

type TestTInstruction = class(TTestCase)
  strict private
    Context : TSlimContext;
  published
    procedure TestMake;
    procedure TestMakeWithClassNotFound;
    procedure TestListExecution;
    procedure TestImport;
    procedure TestMakeWithImportedPath;
    procedure TestContextGivenToListItems;
    procedure TestCallProcedureWithoutArgs;
    procedure TestCallFunctionWithoutArgs;
    procedure TestCallUnknownMethod;
    procedure TestCallProcedureWithArg;
  protected
    procedure SetUp; override;
end;

implementation

uses SlimDirective, MockInstruction, DummyFixtures;

{ TestTInstruction }


procedure TestTInstruction.SetUp;
begin
  Context := TSlimContext.Create;
end;

procedure TestTInstruction.TestMake;
var instruction : TMakeInstruction;
  response : TSlimDirective;
begin
  instruction := TMakeInstruction.Create('an_id', 'DummyFixtures.TDummyFixture', 'inst');
  DummyFixtures.TDummyFixture.ClassName; // rtti must load class

  response := instruction.Execute(Context);

  CheckNotNull(response);
  CheckEquals(2, response.Length);
  CheckEquals('an_id', response.GetItemValue(0));
  CheckEquals('OK', response.GetItemValue(1));
  CheckIs(context.GetRegisteredInstance('inst'), TDummyFixture);
end;

procedure TestTInstruction.TestMakeWithClassNotFound;
var instruction : TMakeInstruction;
  response : TSlimDirective;
begin
  instruction := TMakeInstruction.Create('an_id', 'TUnknownClass', '');

  response := instruction.Execute(Context);

  CheckNotNull(response);
  CheckEquals(2, response.Length);
  CheckEquals('an_id', response.GetItemValue(0));
  CheckEquals('__EXCEPTION__:message:<<NO_CLASS TUnknownClass>>', response.GetItemValue(1));
end;

procedure TestTInstruction.TestImport;
var instruction : TImportInstruction;
  response : TSlimDirective;
begin
  instruction := TImportInstruction.Create('id', 'DummyFixtures');

  response := instruction.Execute(Context);

  CheckEquals(2, response.Length);
  CheckEquals('id', response.GetItemValue(0));
  CheckEquals('OK', response.GetItemValue(1));
  CheckEquals(0, Context.ImportPaths.IndexOf('DummyFixtures'));
end;

procedure TestTInstruction.TestMakeWithImportedPath;
var makeResponse : TSlimDirective;
begin
  DummyFixtures.TDummyFixture.ClassName; // rtti must load class
  context.AddImportPath('DummyFixtures');

  makeResponse := TMakeInstruction.Create('id', 'TDummyFixture', '').Execute(context);

  CheckEquals('OK', makeResponse.GetItemValue(1));
end;

procedure TestTInstruction.TestListExecution;
var instruction : TListInstruction;
  response : TSlimDirective;
begin
  instruction := TListInstruction.Create;
  instruction.Add(TMockInstruction.Create('response_1'));
  instruction.Add(TMockInstruction.Create('response_2'));

  response := instruction.Execute(nil);

  CheckEquals(2, response.Length);
  CheckEquals('response_1', response.GetItemValue(0));
  CheckEquals('response_2', response.GetItemValue(1));
end;

procedure TestTInstruction.TestContextGivenToListItems;
var instruction : TListInstruction;
  childInstruction : TMockInstruction;
begin
  instruction := TListInstruction.Create;
  childInstruction := TMockInstruction.Create();
  instruction.Add(childInstruction);

  instruction.Execute(Context);

  CheckSame(context, childInstruction.ContextUsed);
end;


procedure TestTInstruction.TestCallProcedureWithoutArgs;
var instruction : TCallInstruction;
  dummyFixture : TDummyFixture;
  response : TSlimDirective;
begin
  dummyFixture := TDummyFixture.Create;
  Context.RegisterInstance('instance', dummyFixture);
  instruction := TCallInstruction.Create('id', 'instance', 'DummyMethod');

  response := instruction.Execute(context);

  Check(dummyFixture.DummyMethodCalled, 'I expected the method would be called');
  CheckEquals(2, response.Length);
  CheckEquals('id', response.GetItemValue(0));
  CheckEquals('/__VOID__/', response.GetItemValue(1));
end;



procedure TestTInstruction.TestCallFunctionWithoutArgs;
var instruction : TCallInstruction;
  dummyFixture : TDummyFixture;
  response : TSlimDirective;
begin
  dummyFixture := TDummyFixture.Create;
  dummyFixture.TheAnswer := '42';
  Context.RegisterInstance('instance', dummyFixture);
  instruction := TCallInstruction.Create('id', 'instance', 'DummyFunction');

  response := instruction.Execute(context);

  CheckEquals('42', response.GetItemValue(1));
end;

procedure TestTInstruction.TestCallUnknownMethod;
var instruction : TCallInstruction;
  response : TSlimDirective;
begin
  Context.RegisterInstance('instance', TDummyFixture.Create);
  instruction := TCallInstruction.Create('id', 'instance', 'UnknownMethod');

  response := instruction.Execute(context);

  CheckEquals(2, response.Length);
  CheckEquals('__EXCEPTION__:message:<<NO_METHOD_IN_CLASS UnknownMethod TDummyFixture>>', response.GetItemValue(1));
end;

procedure TestTInstruction.TestCallProcedureWithArg;
var instruction : TCallInstruction;
  dummyFixture : TDummyFixture;
  response : TSlimDirective;
begin
  dummyFixture := TDummyFixture.Create;
  Context.RegisterInstance('instance', dummyFixture);
  instruction := TCallInstruction.Create('id', 'instance', 'MethodWithArg');
  instruction.AddArgument('hello');

  response := instruction.Execute(context);

  CheckEquals('hello', dummyFixture.ArgUsedToCallMethod);
  CheckEquals(2, response.Length);
  CheckEquals('id', response.GetItemValue(0));
  CheckEquals('/__VOID__/', response.GetItemValue(1));
end;

initialization
  RegisterTest(TestTInstruction.Suite);
end.
