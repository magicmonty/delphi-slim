unit MockInstruction;

interface

uses Instruction, SlimDirective, SlimContext;

type TMockInstruction = class(TInstruction)
  public
    Executed : Boolean;
    ExecutionResult : TSlimDirective;
    ContextUsed : TSlimContext;
    function Execute(context : TSlimContext): TSlimDirective; override;
    constructor Create; overload;
    constructor Create(result : string); overload;
end;

implementation

{ TMockInstruction }

constructor TMockInstruction.Create(result: string);
begin
  ExecutionResult := TSlimStringDirective.Create(result);
end;

constructor TMockInstruction.Create;
begin
  ExecutionResult := TSlimStringDirective.Create('');
end;

function TMockInstruction.Execute(context : TSlimContext): TSlimDirective;
begin
  Executed := True;
  Result := ExecutionResult;
  ContextUsed := context;
end;

end.
