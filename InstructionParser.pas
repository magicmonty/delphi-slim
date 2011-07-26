unit InstructionParser;

interface

uses SlimDirective, Instruction;

type TInstructionBuilder = class
  public
    function Build(directive : TSlimDirective) : TInstruction;
  protected
    function DoBuild(directive : TSlimDirective) : TInstruction; virtual; abstract;
end;

type TListInstructionBuilder = class(TInstructionBuilder)
  protected
    function DoBuild(directive: TSlimDirective): TInstruction; override;
end;

type TImportInstructionBuilder = class(TInstructionBuilder)
  public
    function DoBuild(directive : TSlimDirective) : TInstruction; override;
end;

type TMakeInstructionBuilder = class(TInstructionBuilder)
  public
    function DoBuild(directive : TSlimDirective) : TInstruction;  override;
end;

type TCallInstructionBuilder = class(TInstructionBuilder)
  public
    function DoBuild(directive: TSlimDirective): TInstruction; override;
end;

type TInstructionParser = class
  public
    function Parse(directive : TSlimDirective) : TInstruction; virtual;
  private
    function GetInstructionBuilder(directive : TSlimDirective) : TInstructionBuilder;
end;


implementation

uses Logger, SysUtils;

{ TInstructionParser }

function TInstructionParser.GetInstructionBuilder(directive : TSlimDirective): TInstructionBuilder;
begin
  if directive.GetItem(0) is TSlimStringDirective then
  begin
    if directive.GetItemValue(1) = 'make' then
      Exit(TMakeInstructionBuilder.Create);

    if directive.GetItemValue(1) = 'import' then
      Exit(TImportInstructionBuilder.Create);

    if directive.GetItemValue(1) = 'call' then
      Exit(TCallInstructionBuilder.Create);
  end
  else begin
    Exit(TListInstructionBuilder.Create);
  end;

  Exit(nil);
end;

function TInstructionParser.Parse(directive: TSlimDirective): TInstruction;
begin
  Exit(GetInstructionBuilder(directive).Build(directive));
end;

{ TInstructionBuilder }

function TInstructionBuilder.Build(directive: TSlimDirective): TInstruction;
begin
  Result := DoBuild(directive);
  Result.Id := directive.GetItemValue(0);
end;

{ TMakeInstructionBuilder }

function TMakeInstructionBuilder.DoBuild(directive : TSlimDirective) : TInstruction;
var
  makeInstruction : TMakeInstruction;
begin
  makeInstruction := TMakeInstruction.Create;
  makeInstruction.InstanceName := directive.GetItemValue(2);
  makeInstruction.ClassToMake := directive.GetItemValue(3);
  Exit(makeInstruction);
end;

{ TImportInstructionBuilder }

function TImportInstructionBuilder.DoBuild(directive: TSlimDirective): TInstruction;
var  importInstruction : TImportInstruction;
begin
  importInstruction := TImportInstruction.Create;
  importInstruction.Path := directive.GetItemValue(2);
  Exit(importInstruction);
end;

{ TCallInstructionBuilder }

function TCallInstructionBuilder.DoBuild(directive: TSlimDirective): TInstruction;
var callInstruction : TCallInstruction;
  i : Integer;
begin
  callInstruction := TCallInstruction.Create;
  callInstruction.InstanceName := directive.GetItemValue(2);
  callInstruction.FunctionName := directive.GetItemValue(3);
  for i := 4 to directive.Length - 1 do
    callInstruction.AddArgument(directive.GetItemValue(i));
  Exit(callInstruction);
end;

{ TListInstructionBuilder }

function TListInstructionBuilder.DoBuild(directive: TSlimDirective): TInstruction;
var i : Integer;
  parser : TInstructionParser;
begin
  Result := TListInstruction.Create;
  for i := 0 to directive.Length - 1 do
  begin
    parser := TInstructionParser.Create;
    parser.Parse(directive.GetItem(i));
    Result.Add(parser.Parse(directive.GetItem(i)));
  end;
end;

end.
