unit TestSlimDirectiveSerializer;

interface

uses TestFramework, SlimDirective, SlimDirectiveSerializer;

type TestTSlimDirectiveSerializer = class(TTestCase)
  published
    procedure TestSimpleString;
    procedure TestList;
    procedure TestListInList;
end;

implementation

{ TestTSlimDirectiveSerializer }





procedure TestTSlimDirectiveSerializer.TestSimpleString;
var directive : TSlimDirective;
  serializer : TSlimDirectiveSerializer;
  result : string;
begin
  directive := TSlimStringDirective.Create('hello');
  serializer := TSlimDirectiveSerializer.Create;

  result := serializer.Serialize(directive);

  CheckEquals('000005:hello', result);
end;

procedure TestTSlimDirectiveSerializer.TestList;
var directive : TSlimDirective;
  serializer : TSlimDirectiveSerializer;
  result : string;
begin
  directive := TSlimDirective.Create().Add('one').Add('two');
  serializer := TSlimDirectiveSerializer.Create;

  result := serializer.Serialize(directive);

  CheckEquals('000031:[000002:000003:one:000003:two:]', result);
end;

procedure TestTSlimDirectiveSerializer.TestListInList;
var directive : TSlimDirective;
  subdirective : TSlimDirective;
  serializer : TSlimDirectiveSerializer;
  result : string;
begin
  subdirective := TSlimDirective.Create().Add('21').Add('22');
  directive := TSlimDirective.Create().Add('one').Add(subdirective);
  serializer := TSlimDirectiveSerializer.Create;

  result := serializer.Serialize(directive);

  CheckEquals('000057:[000002:000003:one:000029:[000002:000002:21:000002:22:]:]', result);
end;

initialization
  RegisterTest(TestTSlimDirectiveSerializer.Suite);
end.
