unit SlimServer;

interface

uses
  SysUtils, Classes, WinSock, Windows, IdTCPServer, IdContext, InputProcessor, SlimContext;

type
  TSlimServer = class
    public
      InputProcessor : TInputProcessor;
      Active : Boolean;
      constructor Create(port : Integer; packagePaths : string);
      destructor Destroy; override;
      procedure Start;
      procedure Stop;
    strict private
      TcpServer : TIdTCPServer;
      Port : Integer;
      SlimContext : TSlimContext;
      procedure TcpServerConnect(AContext : TIdContext);
      procedure TcpServerExecute(AContext : TIdContext);
      procedure WriteToClient(AContext : TIdContext; msg : string; newLine : Boolean = False);
      function ReadInput(AContext : TIdContext) : string;
  end;

implementation

uses Logger;

constructor TSlimServer.Create(port : Integer; packagePaths : string);
var paths : TStrings;
begin
  Self.Port := port;
  InputProcessor := TInputProcessor.Create;
  Active := True;
  SlimContext := TSlimContext.Create;

  paths := TStringList.Create;
  ExtractStrings([';'], [' '], PChar(packagePaths), paths);
  SlimContext.AddPackagePath(paths[0]);
  SlimContext.AddPackagePath(paths[1]);
end;

destructor TSlimServer.Destroy;
begin
  TcpServer.Free;
  inherited Destroy;
end;

procedure TSlimServer.Start;
begin
  TcpServer := TIdTCPServer.Create(nil);
  TcpServer.DefaultPort := Port;
  TcpServer.OnConnect := TcpServerConnect;
  TcpServer.OnExecute := TcpServerExecute;
  TcpServer.Active := True;
end;

procedure TSlimServer.Stop;
begin
  TcpServer.Active := False;
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
