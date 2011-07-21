unit TestRtti;

interface

uses TestFramework;

type TestTRtti = class(TTestCase)
  procedure TestFindType;
end;

type DummyClass = class end;

implementation

uses Rtti;

{ TestTRTTi }

procedure TestTRtti.TestFindType;
var context : TRttiContext;
  foundType : TRttiType;
begin
  context := TRttiContext.Create;
  DummyClass.ClassName; // to let Rtti know about this class

  foundType := context.FindType('TestRtti.DummyClass');

  CheckNotNull(foundType);
end;


initialization
  RegisterTest(TestTRtti.Suite);
end.
