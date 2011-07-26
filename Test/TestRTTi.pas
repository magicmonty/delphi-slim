unit TestRtti;

interface

uses TestFramework;

type TestTRtti = class(TTestCase)
  procedure TestFindType;
  procedure TestFindTypeInCompiledUnit;
  procedure TestGetTypedInstanceAfterCreation;
  procedure TestInvokeMethodOnStaticInstance;
  procedure TestInvokeMethodOnDynamicInstance;
end;

type DummyClass = class
  procedure MyMethod;
end;

implementation

uses Rtti, SysUtils, Classes, Dialogs;

var MyMethodCalled : Boolean;

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



procedure TestTRtti.TestInvokeMethodOnDynamicInstance;
var context : TRttiContext;
  foundType : TRttiInstanceType;
  dummy : TValue;
begin
  context := TRttiContext.Create;
  DummyClass.ClassName; // to let Rtti know about this class
  foundType := context.FindType('TestRtti.DummyClass') as TRttiInstanceType;
  dummy := foundType.GetMethod('Create').Invoke(foundType.MetaclassType, []);

  foundType.GetMethod('MyMethod').Invoke(dummy, []);

  Check(dummy.IsInstanceOf(DummyClass));
  Check(MyMethodCalled);
end;


procedure TestTRtti.TestGetTypedInstanceAfterCreation;
var context : TRttiContext;
  foundType : TRttiInstanceType;
  dummy : TValue;
  dummyInstance : DummyClass;
begin
  context := TRttiContext.Create;
  DummyClass.ClassName; // to let Rtti know about this class
  foundType := context.FindType('TestRtti.DummyClass') as TRttiInstanceType;
  dummy := foundType.GetMethod('Create').Invoke(foundType.MetaclassType, []);
  MyMethodCalled := False;

  dummyInstance := dummy.AsType<DummyClass>;
  dummyInstance.MyMethod;

  Check(MyMethodCalled);
end;

procedure TestTRtti.TestInvokeMethodOnStaticInstance;
var context : TRttiContext;
  foundType : TRttiInstanceType;
  dummy : DummyClass;
begin
  context := TRttiContext.Create;
  foundType := context.FindType('TestRtti.DummyClass') as TRttiInstanceType;
  MyMethodCalled := False;
  dummy := DummyClass.Create;

  foundType.GetMethod('MyMethod').Invoke(dummy, []);

  Check(MyMethodCalled);
end;

{ DummyClass }

procedure DummyClass.MyMethod;
begin
  MyMethodCalled := True;
end;

initialization
  RegisterTest(TestTRtti.Suite);
end.
