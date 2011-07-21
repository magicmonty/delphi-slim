unit MockInstruction;

interface

uses Instruction, SlimDirective;

type TMockInstruction = class(TInstruction)
  public
    Executed : Boolean;
    ExecutionResult : TSlimDirective;
    function Execute: TSlimDirective; override;
    constructor Create(result : string);
end;

implementation

{ TMockInstruction }

constructor TMockInstruction.Create(result: string);
begin
  ExecutionResult := TSlimStringDirective.Create(result);
end;

function TMockInstruction.Execute: TSlimDirective;
begin
  Executed := True;
  Result := ExecutionResult;
end;

end.
