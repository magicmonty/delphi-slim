unit TestInstruction;

interface

uses TestFramework, Instruction;

type TestTInstruction = class(TTestCase)
  published
    procedure TestMake;
    procedure TestMakeWithClassNotFound;
    procedure TestListExecution;
    procedure TestImport;
end;

implementation

uses SlimDirective, MockInstruction, DummyFixtures;

{ TestTInstruction }

procedure TestTInstruction.TestMake;
var instruction : TMakeInstruction;
  response : TSlimDirective;
begin
  instruction := TMakeInstruction.Create;
  instruction.Id := 'an_id';
  instruction.ClassToMake := 'DummyFixtures.TDummyFixture';
  DummyFixtures.TDummyFixture.ClassName; // rtti must load class

  response := instruction.Execute;

  CheckNotNull(response);
  CheckEquals(2, response.Length);
  CheckEquals('an_id', response.GetItem(0).Value);
  CheckEquals('OK', response.GetItem(1).Value);
end;

procedure TestTInstruction.TestMakeWithClassNotFound;
var instruction : TMakeInstruction;
  response : TSlimDirective;
begin
  instruction := TMakeInstruction.Create;
  instruction.Id := 'an_id';
  instruction.ClassToMake := 'TUnknownClass';

  response := instruction.Execute;

  CheckNotNull(response);
  CheckEquals(2, response.Length);
  CheckEquals('an_id', response.GetItem(0).Value);
  CheckEquals('__EXCEPTION__:message:<<NO_CLASS TUnknownClass>>', response.GetItem(1).Value);
end;

procedure TestTInstruction.TestImport;
var instruction : TImportInstruction;
  response : TSlimDirective;
begin
  instruction := TImportInstruction.Create;
  instruction.Id := 'id';
  instruction.Path := 'DummyFixtures';

  response := instruction.Execute;

  CheckEquals(2, response.Length);
  CheckEquals('id', response.GetItem(0).Value);
  CheckEquals('OK', response.GetItem(1).Value);
end;

procedure TestTInstruction.TestListExecution;
var instruction : TListInstruction;
  response : TSlimDirective;
begin
  instruction := TListInstruction.Create;
  instruction.Add(TMockInstruction.Create('response_1'));
  instruction.Add(TMockInstruction.Create('response_2'));

  response := instruction.Execute;

  CheckEquals(2, response.Length);
  CheckEquals('response_1', response.GetItem(0).Value);
  CheckEquals('response_2', response.GetItem(1).Value);
end;

initialization
  RegisterTest(TestTInstruction.Suite);
end.
