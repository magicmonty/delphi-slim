unit DummyFixtures;

interface

type TDummyFixture = class
  DummyMethodCalled : Boolean;
  ArgUsedToCallMethod : string;
  TheAnswer : string;
  procedure DummyMethod;
  function DummyFunction : string;
  procedure MethodWithArg(arg : string);
end;

implementation

{ TDummyFixture }

procedure TDummyFixture.DummyMethod;
begin
  DummyMethodCalled := True;
end;

procedure TDummyFixture.MethodWithArg(arg: string);
begin
  ArgUsedToCallMethod := arg;
end;

function TDummyFixture.DummyFunction: string;
begin
  Result := TheAnswer;
end;

end.
