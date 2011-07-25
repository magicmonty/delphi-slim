unit InstructionExecutor;

interface

uses SlimDirective, Instruction, SlimContext;

type TInstructionExecutor = class
  function Execute(instruction : TInstruction; context : TSlimContext) : TSlimDirective; virtual;
end;

implementation

{ TInstructionExecutor }

function TInstructionExecutor.Execute(instruction : TInstruction; context : TSlimContext): TSlimDirective;
begin
  Result := instruction.Execute(context);
end;

end.
