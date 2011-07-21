unit InputProcessor;


interface

uses Classes, InstructionExecutor, InstructionParser;

type TResponse = class
  MustDisconnect : Boolean;
  Output : string;
end;

type TInputProcessor = class
  public
    InstructionExecutor : TInstructionExecutor;
    InstructionParser : TInstructionParser;
    constructor Create;
    function Process(input : string) : TResponse; virtual;
end;

implementation

uses SlimDirectiveSerializer, SlimDirectiveDeserializer, SlimDirective, Instruction, Logger;

{ TInputProcessor }

constructor TInputProcessor.Create;
begin
  InstructionParser := TInstructionParser.Create;
  InstructionExecutor := TInstructionExecutor.Create;
end;

function TInputProcessor.Process(input: string): TResponse;
var serializer : TSlimDirectiveSerializer;
  deserializer : TSlimDirectiveDeserializer;
  directiveResponse : TSlimDirective;
  instructionToExecute : TInstruction;
  directive : TSlimDirective;
begin
  Result := TResponse.Create;
  Result.MustDisconnect := input = '000003:bye';
  if Result.MustDisconnect then
    Exit(Result);

  serializer := TSlimDirectiveSerializer.Create;
  deserializer := TSlimDirectiveDeserializer.Create;
  directive := deserializer.Deserialize(input);
  instructionToExecute := InstructionParser.Parse(directive);
  directiveResponse := InstructionExecutor.Execute(instructionToExecute);

  Result.Output := serializer.Serialize(directiveResponse);
end;


end.
