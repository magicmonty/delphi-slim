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
    if directive.GetItem(1).Value = 'make' then
      Exit(TMakeInstructionBuilder.Create);

    if directive.GetItem(1).Value = 'import' then
      Exit(TImportInstructionBuilder.Create);

    if directive.GetItem(1).Value = 'call' then
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
  Result.Id := directive.GetItem(0).Value;
end;

{ TMakeInstructionBuilder }

function TMakeInstructionBuilder.DoBuild(directive : TSlimDirective) : TInstruction;
var
  makeInstruction : TMakeInstruction;
begin
  makeInstruction := TMakeInstruction.Create;
  makeInstruction.InstanceName := directive.GetItem(2).Value;
  makeInstruction.ClassToMake := directive.GetItem(3).Value;
  Exit(makeInstruction);
end;

{ TImportInstructionBuilder }

function TImportInstructionBuilder.DoBuild(directive: TSlimDirective): TInstruction;
var  importInstruction : TImportInstruction;
begin
  importInstruction := TImportInstruction.Create;
  importInstruction.Path := directive.GetItem(2).Value;
  Exit(importInstruction);
end;

{ TCallInstructionBuilder }

function TCallInstructionBuilder.DoBuild(
  directive: TSlimDirective): TInstruction;
var callInstruction : TCallInstruction;
begin
  callInstruction := TCallInstruction.Create;
  callInstruction.InstanceName := directive.GetItem(2).Value;
  callInstruction.FunctionName := directive.GetItem(3).Value;
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
