program DelphiSlimTests;
{

  Projet de test DUnit Delphi
  -------------------------
  Ce projet contient le framework de test DUnit et les exécuteurs de test GUI/Console.
  Ajoutez "CONSOLE_TESTRUNNER" à l'entrée des définitions conditionnelles des options
  de projet pour utiliser l'exécuteur de test console.  Sinon, l'exécuteur de test GUI sera
  utilisé par défaut.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  TestSlimServer in 'TestSlimServer.pas',
  InputProcessor in '..\InputProcessor.pas',
  SlimServer in '..\SlimServer.pas',
  TestInputProcessor in 'TestInputProcessor.pas',
  TestSlimDirectiveDeserializer in 'TestSlimDirectiveDeserializer.pas',
  TestInstructionParser in 'TestInstructionParser.pas',
  InstructionParser in '..\InstructionParser.pas',
  SlimDirective in '..\SlimDirective.pas',
  SlimDirectiveDeserializer in '..\SlimDirectiveDeserializer.pas',
  Instruction in '..\Instruction.pas',
  TestInstruction in 'TestInstruction.pas',
  InstructionExecutor in '..\InstructionExecutor.pas',
  TestSlimDirectiveSerializer in 'TestSlimDirectiveSerializer.pas',
  SlimDirectiveSerializer in '..\SlimDirectiveSerializer.pas',
  Logger in '..\Logger.pas',
  TestInstructionExecutor in 'TestInstructionExecutor.pas',
  MockInstruction in 'MockInstruction.pas',
  DummyFixtures in 'DummyFixtures.pas',
  TestRTTi in 'TestRTTi.pas',
  SlimContext in '..\SlimContext.pas',
  SlimMethod in '..\SlimMethod.pas',
  TestSlimMethod in 'TestSlimMethod.pas';

{R *.RES}

begin
  Application.Initialize;
  if IsConsole then
    with TextTestRunner.RunRegisteredTests do
      Free
  else
    GUITestRunner.RunRegisteredTests;
end.

