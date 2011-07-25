unit TestRtti;

interface

uses TestFramework;

type TestTRtti = class(TTestCase)
  procedure TestFindType;
  procedure TestFindTypeInCompiledUnit;
end;

type DummyClass = class end;

implementation

uses Rtti, SysUtils, Classes, Dialogs;

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


procedure TestTRtti.TestFindTypeInCompiledUnit;
var
  package : HMODULE;
  context : TRttiContext;
  foundType : TRttiType;
begin
  package := LoadPackage('..\..\CompiledUnits\FixturesPackage.bpl');
  context := TRttiContext.Create;

  foundType := context.FindType('CompiledFixtures.TCompiledFixture');

  CheckNotNull(foundType);
  UnloadPackage(package);
end;

initialization

  RegisterTest(TestTRtti.Suite);
end.
