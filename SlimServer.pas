unit SlimServer;

interface

uses
  SysUtils, Classes, WinSock, Windows, IdTCPServer, IdContext, InputProcessor, Logger;

type
  TSlimServer = class
    private
      FTCPServer : TIdTCPServer;
      port : Integer;
      procedure WriteToClient(AContext : TIdContext; msg : string; newLine : Boolean = False);
    public
      InputProcessor : TInputProcessor;
      Active : Boolean;
      destructor Destroy; override;
      procedure TCPServerConnect(AContext : TIdContext);
      procedure TCPServerExecute(AContext : TIdContext);
      constructor Create(port : Integer);
      procedure Start;
      procedure Stop;
  end;

implementation



constructor TSlimServer.Create(port : Integer);
begin
  Self.port := port;
  InputProcessor := TInputProcessor.Create;
  Active := True;
end;



destructor TSlimServer.Destroy;
begin
  FTCPServer.Free;
  inherited Destroy;
end;



procedure TSlimServer.Start;
begin
  FTCPServer := TIdTCPServer.Create(nil);
  FTCPServer.DefaultPort := port;
  FTCPServer.OnConnect := TCPServerConnect;
  FTCPServer.OnExecute := TCPServerExecute;
  FTCPServer.Active := True;
end;



procedure TSlimServer.Stop;
begin
  FTCPServer.Active := False;
end;



procedure TSlimServer.TCPServerConnect(AContext: TIdContext);
begin
  WriteToClient(AContext, 'Slim -- V0.3', True);
end;

procedure TSlimServer.TCPServerExecute(AContext : TIdContext);
var
  lCmdLine : string;
  lCmdLine2 : string;
  size : Integer;
  response : TResponse;
begin
  lCmdLine := AContext.Connection.IOHandler.ReadString(6);
  size := StrToInt(lCmdLine);
  lCmdLine2 := AContext.Connection.IOHandler.ReadString(size + 1);
  Log('--> ' + lCmdLine + lCmdLine2);

  response := InputProcessor.Process(lCmdLine + lCmdLine2);

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



procedure TSlimServer.WriteToClient(AContext : TIdContext; msg : string; newLine : Boolean);
begin
    Log('<-- ' + msg);
    if  newLine then
      AContext.Connection.IOHandler.WriteLn(msg)
    else
      AContext.Connection.IOHandler.Write(msg);
end;

end.
