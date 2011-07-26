program DelphiSlim;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  SlimServer in 'SlimServer.pas',
  InputProcessor in 'InputProcessor.pas',
  SlimDirectiveDeserializer in 'SlimDirectiveDeserializer.pas',
  SlimDirective in 'SlimDirective.pas',
  InstructionParser in 'InstructionParser.pas',
  Instruction in 'Instruction.pas',
  InstructionExecutor in 'InstructionExecutor.pas',
  SlimDirectiveSerializer in 'SlimDirectiveSerializer.pas',
  Logger in 'Logger.pas',
  SlimContext in 'SlimContext.pas',
  SlimMethod in 'SlimMethod.pas';

var
  port  : Integer;
  packagesPaths : string;
  slimServer : TSlimServer;

begin

  try
    packagesPaths := ParamStr(1);
    port := StrToInt(ParamStr(2));
    slimServer := TSlimServer.Create(port, packagesPaths);

    slimServer.Start;
    while slimServer.Active do ;
  except
    on E: Exception do
      Log(E.ClassName + ': ' + E.Message);
  end;
end.
