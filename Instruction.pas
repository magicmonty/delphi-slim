unit Instruction;

interface

uses SlimDirective;

type TInstruction = class
  public
    Id : string;
    function GetLength : Integer; virtual;
    function GetItem(index : Integer) : TInstruction;
    property Length : Integer read GetLength;
    function Execute : TSlimDirective; virtual;
    procedure Add(item : TInstruction);
  private
    instructions : array of TInstruction;
end;

type TListInstruction = class(TInstruction)
  public
    function Execute: TSlimDirective; override;
end;

type TMakeInstruction = class(TInstruction)
  public
    InstanceName : string;
    ClassToMake : string;
    function Execute: TSlimDirective; override;
end;

type TImportInstruction = class(TInstruction)
  public
    Path : string;
    function Execute: TSlimDirective; override;
end;

type TCallInstruction = class(TInstruction)
  public
    InstanceName : string;
    FunctionName : string;
    function Execute: TSlimDirective; override;
end;

implementation

uses Logger, Classes, Rtti;

{ TInstruction }

procedure TInstruction.Add(item : TInstruction);
begin
  SetLength(instructions, GetLength() + 1);
  instructions[GetLength() - 1] := item;
end;

function TInstruction.Execute: TSlimDirective;
begin
//  Log('Execute ' + Self.ClassName + ' ' + Id);
end;

function TInstruction.GetItem(index: Integer): TInstruction;
begin
  Result := instructions[index];
end;

function TInstruction.GetLength: Integer;
begin
  Result := System.Length(instructions);
end;

{ TMakeInstruction }

function TMakeInstruction.Execute: TSlimDirective;
var typeFound : TRttiType;
begin
  inherited;
  Result := TSlimDirective.Create;
  Result.Add(Id);
  typeFound := TRttiContext.Create.FindType(ClassToMake);
  if typeFound = nil then
  begin
    Result.Add('__EXCEPTION__:message:<<NO_CLASS ' + ClassToMake + '>>');
  end else
  begin
    Result.Add('OK');
  end;

end;

{ TImportInstruction }

function TImportInstruction.Execute: TSlimDirective;
begin
  inherited;
  Result := TSlimDirective.Create;
  Result.Add(Id);
  Result.Add('OK');
end;

{ TCallInstruction }

function TCallInstruction.Execute: TSlimDirective;
begin
  inherited;
  // NON TESTÉ
  Result := TSlimDirective.Create;
  Result.Add(Id);
  Result.Add('/__VOID__/');
end;

{ TListInstruction }

function TListInstruction.Execute: TSlimDirective;
var i : Integer;
begin
  inherited;
  Result := TSlimDirective.Create;
  for i := 0 to GetLength - 1 do
  begin
    Result.Add(GetItem(i).Execute);
  end;
end;

end.
