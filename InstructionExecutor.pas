unit InstructionExecutor;

interface

uses SlimDirective, Instruction;

type TInstructionExecutor = class
  function Execute(instruction : TInstruction) : TSlimDirective; virtual;
end;

implementation

{ TInstructionExecutor }

function TInstructionExecutor.Execute(instruction : TInstruction): TSlimDirective;
begin
  Result := instruction.Execute;
end;

end.
