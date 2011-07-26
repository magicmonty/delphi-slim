unit TestSlimServer;

interface

uses
  TestFramework, SlimServer, IdTCPClient, InputProcessor, SlimContext;

const SLEEP_TIME = 5;

type TMockInputProcessor = class(TInputProcessor)
  public
    ProcessedInput : string;
    ProcessResult : TResponse;
    ContextUsed : TSlimContext;
    constructor Create;
    function Process(input: string; context : TSlimContext) : TResponse; override;
end;

type TestTSlimServer = class(TTestCase)
  published
    procedure TestWillReceiveVersionOnConnect;
    procedure TestBye;
    procedure TestCommandProcessed;
    procedure TestSeveralCommands;
    procedure TestAContextIsUsed;
    procedure TestTheSameContextIsUsedForEachInput;
    procedure TestPackagePathsAreGivenToContext;
    procedure TestPackagePathsAreGivenToContext2;
    procedure TestPackagesAreLoaded;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  strict private
    FSlimServer : TSlimServer;
    Client : TIdTCPClient;
    MockInputProcessor : TMockInputProcessor;
    WelcomeLineRead : string;
    function CreateClient(port : Integer) : TIdTCPClient;
    procedure WriteToServer(msg : string);
    function PackageIsLoaded(packageName : string) : Boolean;
end;

implementation

uses
  Windows, SysUtils, IdExceptionCore, IdStack, IdException, Logger, Rtti;



procedure TestTSlimServer.SetUp;
begin
  doLog := False;

  FSlimServer := TSlimServer.Create(5555, '');
  MockInputProcessor := TMockInputProcessor.Create;
  FSlimServer.InputProcessor := MockInputProcessor;
  FSlimServer.Start;

  Client := CreateClient(5555);
  Client.Connect;

  WelcomeLineRead := client.IOHandler.ReadLn();
end;


procedure TestTSlimServer.TearDown;
begin
  MockInputProcessor.Free;
  MockInputProcessor := nil;
  Client.Disconnect;
  Client.Free;
  Client := nil;
  FSlimServer.Stop;
end;

procedure TestTSlimServer.TestWillReceiveVersionOnConnect;
begin
  CheckEquals('Slim -- V0.3', WelcomeLineRead);
end;



procedure TestTSlimServer.TestBye;
begin
  MockInputProcessor.ProcessResult := TResponse.Disconnection;

  Client.IOHandler.Write('000003:bye');

  try
    Client.IOHandler.ReadLn();
    Fail('Connection should be closed');
  except on    EIdConnClosedGracefully do;
  end;
end;


procedure TestTSlimServer.TestCommandProcessed;
begin
  WriteToServer('000005:hello');

  CheckEquals('000005:hello', MockInputProcessor.ProcessedInput);
end;

procedure TestTSlimServer.TestSeveralCommands;
begin
  WriteToServer('000005:hello');
  MockInputProcessor.ProcessResult := TResponse.Normal('second response');

  WriteToServer('000006:second');

  CheckEquals('second response', client.IOHandler.ReadString(15));
end;

procedure TestTSlimServer.TestAContextIsUsed;
begin
  WriteToServer('000005:hello');

  CheckNotNull(MockInputProcessor.ContextUsed);
end;

procedure TestTSlimServer.TestTheSameContextIsUsedForEachInput;
var contextUsedForFirstInput : TSlimContext;
begin
  WriteToServer('000005:hello');
  contextUsedForFirstInput := MockInputProcessor.ContextUsed;

  WriteToServer('000005:there');

  CheckSame(MockInputProcessor.ContextUsed, contextUsedForFirstInput);
end;

procedure TestTSlimServer.TestPackagePathsAreGivenToContext;
var anotherServer : TSlimServer;
  context : TSlimContext;
begin
  anotherServer := TSlimServer.Create(1234, 'packagePath1;packagePath2');

  context := anotherServer.SlimContext;

  CheckNotNull(context.PackagePaths);
  CheckEquals(2, context.PackagePaths.Count);
  CheckEquals('packagePath1', context.PackagePaths[0]);
  CheckEquals('packagePath2', context.PackagePaths[1]);
end;

procedure TestTSlimServer.TestPackagePathsAreGivenToContext2;
var anotherServer : TSlimServer;
  context : TSlimContext;
begin
  anotherServer := TSlimServer.Create(1234, 'pac1;pac 2;pac 3');

  context := anotherServer.SlimContext;

  CheckNotNull(context.PackagePaths);
  CheckEquals(3, context.PackagePaths.Count);
  CheckEquals('pac1', context.PackagePaths[0]);
  CheckEquals('pac 2', context.PackagePaths[1]);
  CheckEquals('pac 3', context.PackagePaths[2]);
end;

procedure TestTSlimServer.TestPackagesAreLoaded;
var
  server : TSlimServer;
begin
  server := TSlimServer.Create(1234, '..\..\CompiledUnits\FixturesPackage.bpl');

  Check(PackageIsLoaded('CompiledUnits\FixturesPackage.bpl'));

  server.SlimContext.Free;
end;

function TestTSlimServer.PackageIsLoaded(packageName: string): Boolean;
var
  rttiContext : TRttiContext;
  packages : TArray<TRttiPackage>;
  i : Integer;
begin
  rttiContext := TRttiContext.Create;
  packages := rttiContext.GetPackages();
 for i := Low(packages) to High(packages) do
  begin
    if  Pos(packageName, packages[i].Name) > 0 then
      Exit(True);
  end;
  Exit(False);
end;

function TestTSlimServer.CreateClient(port : Integer) : TIdTCPClient;
begin
  Result := TIdTCPClient.Create(nil);
  Result.port := port;
  Result.Host := '127.0.0.1';
  Result.ReadTimeout := 50;
end;

procedure TestTSlimServer.WriteToServer(msg: string);
begin
  Client.IOHandler.Write(msg);
  Sleep(SLEEP_TIME);
end;

{ TMockInputProcessor }

constructor TMockInputProcessor.Create;
begin
  ProcessResult := TResponse.Create;
end;

function TMockInputProcessor.Process(input: string; context : TSlimContext) : TResponse;
begin
  ProcessedInput := input;
  Result := ProcessResult;
  ContextUsed := context;
end;

initialization
  RegisterTest(TestTSlimServer.Suite);
end.
