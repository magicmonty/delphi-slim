unit TestInstruction;

interface

uses TestFramework, Instruction;

type TestTInstruction = class(TTestCase)
  published
    procedure TestMake;
    procedure TestMakeWithClassNotFound;
    procedure TestListExecution;
    procedure TestImport;
    procedure TestMakeWithImportedPath;
    procedure TestContextGivenToListItems;
end;

implementation

uses SlimDirective, MockInstruction, DummyFixtures, SlimContext;

{ TestTInstruction }

procedure TestTInstruction.TestMake;
var instruction : TMakeInstruction;
  response : TSlimDirective;
begin
  instruction := TMakeInstruction.Create('an_id', 'DummyFixtures.TDummyFixture');
  DummyFixtures.TDummyFixture.ClassName; // rtti must load class

  response := instruction.Execute(nil);

  CheckNotNull(response);
  CheckEquals(2, response.Length);
  CheckEquals('an_id', response.GetItem(0).Value);
  CheckEquals('OK', response.GetItem(1).Value);
end;

procedure TestTInstruction.TestMakeWithClassNotFound;
var instruction : TMakeInstruction;
  response : TSlimDirective;
begin
  instruction := TMakeInstruction.Create('an_id', 'TUnknownClass');

  response := instruction.Execute(TSlimContext.Create);

  CheckNotNull(response);
  CheckEquals(2, response.Length);
  CheckEquals('an_id', response.GetItem(0).Value);
  CheckEquals('__EXCEPTION__:message:<<NO_CLASS TUnknownClass>>', response.GetItem(1).Value);
end;



procedure TestTInstruction.TestImport;
var instruction : TImportInstruction;
  response : TSlimDirective;
  context : TSlimContext;
begin
  instruction := TImportInstruction.Create('id', 'DummyFixtures');
  context := TSlimContext.Create;

  response := instruction.Execute(context);

  CheckEquals(2, response.Length);
  CheckEquals('id', response.GetItem(0).Value);
  CheckEquals('OK', response.GetItem(1).Value);
  CheckEquals(0, context.ImportPaths.IndexOf('DummyFixtures'));
end;

procedure TestTInstruction.TestMakeWithImportedPath;
var makeResponse : TSlimDirective;
  context : TSlimContext;
begin
  DummyFixtures.TDummyFixture.ClassName; // rtti must load class
  context := TSlimContext.Create;
  context.AddImportPath('DummyFixtures');

  makeResponse := TMakeInstruction.Create('id', 'TDummyFixture').Execute(context);

  CheckEquals('OK', makeResponse.GetItem(1).Value);
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
  CheckEquals('response_1', response.GetItem(0).Value);
  CheckEquals('response_2', response.GetItem(1).Value);
end;

procedure TestTInstruction.TestContextGivenToListItems;
var instruction : TListInstruction;
  context : TSlimContext;
  childInstruction : TMockInstruction;
begin
  instruction := TListInstruction.Create;
  childInstruction := TMockInstruction.Create();
  instruction.Add(childInstruction);
  context := TSlimContext.Create;

  instruction.Execute(context);

  CheckSame(context, childInstruction.ContextUsed);
end;


initialization
  RegisterTest(TestTInstruction.Suite);
end.
