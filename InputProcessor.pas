unit InputProcessor;


interface

uses Classes, InstructionExecutor, InstructionParser, SlimContext;

type TResponse = class
  strict private
    _MustDisconnect : Boolean;
    _Output : string;
  public
    constructor Normal(output : string);
    constructor Disconnection;
    property MustDisconnect : Boolean read _MustDisconnect;
    property Output : String read _Output;
end;

type TInputProcessor = class
  public
    InstructionExecutor : TInstructionExecutor;
    InstructionParser : TInstructionParser;
    constructor Create;
    function Process(input : string; context : TSlimContext) : TResponse; virtual;
end;

implementation

uses SlimDirectiveSerializer, SlimDirectiveDeserializer, SlimDirective, Instruction, Logger;

{ TInputProcessor }

constructor TInputProcessor.Create;
begin
  InstructionParser := TInstructionParser.Create;
  InstructionExecutor := TInstructionExecutor.Create;
end;

function TInputProcessor.Process(input: string; context : TSlimContext): TResponse;
var serializer : TSlimDirectiveSerializer;
  deserializer : TSlimDirectiveDeserializer;
  directiveResponse : TSlimDirective;
  instructionToExecute : TInstruction;
  directive : TSlimDirective;
begin
  if input = '000003:bye' then Exit(TResponse.Disconnection);

  serializer := TSlimDirectiveSerializer.Create;
  deserializer := TSlimDirectiveDeserializer.Create;
  directive := deserializer.Deserialize(input);
  instructionToExecute := InstructionParser.Parse(directive);
  directiveResponse := InstructionExecutor.Execute(instructionToExecute, context);

  Result := TResponse.Normal(serializer.Serialize(directiveResponse));
end;


{ TResponse }

constructor TResponse.Disconnection;
begin
  Self._MustDisconnect := True;
end;

constructor TResponse.Normal(output: string);
begin
  Self._Output := output;
  Self._MustDisconnect := False;
end;

end.
