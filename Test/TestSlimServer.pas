unit TestSlimServer;

interface

uses
  TestFramework, SlimServer, IdTCPClient;

type
  TestTSlimServer = class(TTestCase)
    strict private
      FSlimServer : TSlimServer;
    private
      function CreateClient(port : Integer) : TIdTCPClient;
    protected
      procedure SetUp; override;
      procedure TearDown; override;

    published
      procedure TestCanConnect;
      // ignoré car trop long
      //procedure TestCanStop;
      procedure TestWillReceiveVersionOnConnect;
      procedure TestBye;
      procedure TestCommandProcessed;
      procedure TestSeveralCommands;
  end;

implementation

uses
  Windows, SysUtils, IdExceptionCore, IdStack, IdException, InputProcessor;

type TMockInputProcessor = class(TInputProcessor)
  public
    ProcessedInput : string;
    ProcessResult : TResponse;
    constructor Create;
    function Process(input: string) : TResponse; override;
end;


procedure TestTSlimServer.SetUp;
begin
  FSlimServer := TSlimServer.Create(5555);

  FSlimServer.Start;
end;



procedure TestTSlimServer.TearDown;
begin
  FSlimServer.Stop;
end;



procedure TestTSlimServer.TestCanConnect;
begin
  CreateClient(5555).Connect;
end;




//procedure TestTSlimServer.TestCanStop;
//var
//  anotherServer : TSlimServer;
//begin
//  anotherServer := TSlimServer.Create(5556);
//  anotherServer.Start;
//
//  anotherServer.Stop;
//
//  try
//    CreateClient(5556).Connect;
//    Fail('J''attendais un timeout ');
//  except
//    on EIdConnectTimeout do;
//  end;
//end;




procedure TestTSlimServer.TestWillReceiveVersionOnConnect;
var client : TIdTCPClient;
begin
  client := CreateClient(5555);
  client.Connect;

  CheckEquals('Slim -- V0.3', client.IOHandler.ReadLn());
end;


procedure TestTSlimServer.TestBye;
var client : TIdTCPClient;
begin
  client := CreateClient(5555);
  client.Connect;
  client.IOHandler.ReadLn();

  client.IOHandler.Write('000003:bye');

  try
    client.IOHandler.ReadLn();
    Fail('Connection should be closed');
  except on    EIdConnClosedGracefully do;
  end;
end;


procedure TestTSlimServer.TestCommandProcessed;
var client : TIdTCPClient;
  mockInputProcessor : TMockInputProcessor;
begin
  mockInputProcessor := TMockInputProcessor.Create;
  FSlimServer.InputProcessor := mockInputProcessor;
  client := CreateClient(5555);
  client.Connect;
  client.IOHandler.ReadLn();

  client.IOHandler.Write('000005:hello');

  Sleep(100);
  CheckEquals('000005:hello', mockInputProcessor.ProcessedInput);
  mockInputProcessor.Free;
end;

procedure TestTSlimServer.TestSeveralCommands;
var client : TIdTCPClient;
  mockInputProcessor : TMockInputProcessor;
  mySecondResponse : TResponse;
  tmp : string;
begin
  mockInputProcessor := TMockInputProcessor.Create;
  FSlimServer.InputProcessor := mockInputProcessor;
  client := CreateClient(5555);
  client.Connect;
  client.IOHandler.ReadLn();
  client.IOHandler.Write('000005:hello');
  Sleep(100);
  mySecondResponse := TResponse.Create;
  mySecondResponse.Output := 'second response';
  mockInputProcessor.ProcessResult := mySecondResponse;

  client.IOHandler.Write('000006:second');

  Sleep(100);
  tmp := client.IOHandler.ReadString(15);
  CheckEquals('second response', tmp);
  Sleep(100);
  mockInputProcessor.Free;
end;

function TestTSlimServer.CreateClient(port : Integer) : TIdTCPClient;
begin
  Result := TIdTCPClient.Create(nil);
  Result.port := port;
  Result.Host := '127.0.0.1';
  Result.ConnectTimeout := 100;
  Result.ReadTimeout := 200;
end;

{ TMockInputProcessor }

constructor TMockInputProcessor.Create;
begin
  ProcessResult := TResponse.Create;
end;

function TMockInputProcessor.Process(input: string) : TResponse;
begin
  ProcessedInput := input;
  Result := ProcessResult;
end;

initialization
  RegisterTest(TestTSlimServer.Suite);
end.
