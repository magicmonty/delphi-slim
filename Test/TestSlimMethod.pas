unit TestSlimMethod;

interface

uses TestFramework;

type TestTSlimMethod = class(TTestCase)
  procedure TestConvertReturnValueToString;
  procedure TestConvertIntegerArgument;
  procedure TestConvertSeveralIntegerArguments;
end;

implementation

uses SlimMethod, Classes;

type TDummy = class
  function GetInt42 : Integer;
  function Successor(value : Integer) : Integer;
  function Sum(a, b, c : Integer) : Integer;
end;

{ TestTSlimMethod }

procedure TestTSlimMethod.TestConvertIntegerArgument;
var slimMethod : TSlimMethod;
  callResult : string;
begin
  slimMethod := TSlimMethod.Create(TDummy.Create, 'Successor');

  callResult := slimMethod.Call(['42']);

  CheckEquals('43', callResult);
end;

procedure TestTSlimMethod.TestConvertReturnValueToString;
var slimMethod : TSlimMethod;
  callResult : string;
begin
  slimMethod := TSlimMethod.Create(TDummy.Create, 'GetInt42');

  callResult := slimMethod.Call(TStringList.Create);

  CheckEquals('42', callResult);
end;

procedure TestTSlimMethod.TestConvertSeveralIntegerArguments;
var slimMethod : TSlimMethod;
  callResult : string;
begin
  slimMethod := TSlimMethod.Create(TDummy.Create, 'Sum');

  callResult := slimMethod.Call(['1', '2', '3']);

  CheckEquals('6', callResult);
end;

{ TDummy }

function TDummy.GetInt42: Integer;
begin
  Result := 42;
end;

function TDummy.Successor(value: Integer): Integer;
begin
  Result := value + 1;
end;

function TDummy.Sum(a, b, c: Integer): Integer;
begin
  Result := a + b + c;
end;

initialization
  RegisterTest(TestTSlimMethod.Suite);
end.
