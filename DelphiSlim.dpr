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
  Fixtures in 'Fixtures.pas';

var slimServer : TSlimServer;

begin

  try
    slimServer := TSlimServer.Create(StrToInt(ParamStr(1)));
    slimServer.Start;
    while slimServer.Active do ;
  except
    on E: Exception do
      Log(E.ClassName + ': ' + E.Message);
  end;
end.
