unit SlimServer;

interface

uses
  SysUtils, Classes, WinSock, Windows, IdTCPServer, IdContext, InputProcessor, SlimContext;

type
  TSlimServer = class
    strict private
      _tcpServer : TIdTCPServer;
      _port : Integer;
      _slimContext : TSlimContext;
      procedure TcpServerConnect(AContext : TIdContext);
      procedure TcpServerExecute(AContext : TIdContext);
      procedure WriteToClient(AContext : TIdContext; msg : string; newLine : Boolean = False);
      function ReadInput(AContext : TIdContext) : string;
      procedure AddPackagesPathsToContext(packagePaths : string);
      procedure PackageLoaded;
    public
      InputProcessor : TInputProcessor;
      Active : Boolean;
      property SlimContext : TSlimContext read _slimContext;
      constructor Create(port : Integer; packagePaths : string);
      destructor Destroy; override;
      procedure Start;
      procedure Stop;
  end;

implementation

uses Logger;

constructor TSlimServer.Create(port : Integer; packagePaths : string);
begin
  _port := port;
  InputProcessor := TInputProcessor.Create;
  Active := True;
  PackageLoaded;
  AddPackagesPathsToContext(packagePaths);
  _slimContext.Init;
end;

destructor TSlimServer.Destroy;
begin
  _tcpServer.Free;
  inherited Destroy;
end;


procedure TSlimServer.Start;
begin
  _tcpServer := TIdTCPServer.Create(nil);
  _tcpServer.DefaultPort := _port;
  _tcpServer.OnConnect := TcpServerConnect;
  _tcpServer.OnExecute := TcpServerExecute;
  _tcpServer.Active := True;
end;

procedure TSlimServer.Stop;
begin
  _tcpServer.Active := False;
end;

procedure TSlimServer.PackageLoaded;
begin
  _slimContext := TSlimContext.Create;
end;

procedure TSlimServer.AddPackagesPathsToContext(packagePaths : string);
var
  paths: TStrings;
  i: Integer;
begin
  paths := TStringList.Create;
  ExtractStrings([';'], [' '], PChar(packagePaths), paths);
  for i := 0 to paths.Count - 1 do
    _slimContext.AddPackagePath(paths[i]);
end;

procedure TSlimServer.TcpServerConnect(AContext: TIdContext);
begin
  WriteToClient(AContext, 'Slim -- V0.3', True);
end;

procedure TSlimServer.TcpServerExecute(AContext : TIdContext);
var
  response : TResponse;
begin
  response := InputProcessor.Process(ReadInput(AContext), SlimContext);

  if response.MustDisconnect then
  begin
    AContext.Connection.Disconnect;
    Active := False;
  end
  else
  begin
    WriteToClient(AContext, response.Output);
  end;
end;

function TSlimServer.ReadInput(AContext : TIdContext): string;
var
  sizePart : string;
  tailPart : string;
  size : Integer;
begin
  sizePart := AContext.Connection.IOHandler.ReadString(6);
  size := StrToInt(sizePart);
  tailPart := AContext.Connection.IOHandler.ReadString(size + 1);
  Result := sizePart + tailPart;
  Log('<-- ' + Result);
end;


procedure TSlimServer.WriteToClient(AContext : TIdContext; msg : string; newLine : Boolean);
begin
    Log('--> ' + msg);
    if  newLine then
      AContext.Connection.IOHandler.WriteLn(msg)
    else
      AContext.Connection.IOHandler.Write(msg);
end;

end.
