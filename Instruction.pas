unit Instruction;

interface

uses SlimDirective, SlimContext;

type TInstruction = class
  public
    Id : string;
    constructor Create; overload;
    constructor Create(id : string); overload;
    function GetLength : Integer; virtual;
    function GetItem(index : Integer) : TInstruction;
    property Length : Integer read GetLength;
    function Execute(context : TSlimContext) : TSlimDirective; virtual;
    procedure Add(item : TInstruction);
  private
    instructions : array of TInstruction;
end;

type TListInstruction = class(TInstruction)
  public
    function Execute(context : TSlimContext): TSlimDirective; override;
end;

type TMakeInstruction = class(TInstruction)
  public
    InstanceName : string;
    ClassToMake : string;
    constructor Create; overload;
    constructor Create(id, classToMake : string); overload;
    function Execute(context : TSlimContext): TSlimDirective; override;
end;

type TImportInstruction = class(TInstruction)
  public
    Path : string;
    constructor Create; overload;
    constructor Create(id, path : string); overload;
    function Execute(context : TSlimContext): TSlimDirective; override;
end;

type TCallInstruction = class(TInstruction)
  public
    InstanceName : string;
    FunctionName : string;
    function Execute(context : TSlimContext): TSlimDirective; override;
end;

implementation

uses Logger, Classes, Rtti;

{ TInstruction }

procedure TInstruction.Add(item : TInstruction);
begin
  SetLength(instructions, GetLength() + 1);
  instructions[GetLength() - 1] := item;
end;

constructor TInstruction.Create;
begin

end;

constructor TInstruction.Create(id: string);
begin
  Self.Id := id;
end;

function TInstruction.Execute(context : TSlimContext): TSlimDirective;
begin
//  Log('Execute ' + Self.ClassName + ' ' + Id);
  Result := TSlimDirective.Create;
  Result.Add(Id);
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

constructor TMakeInstruction.Create;
begin

end;

constructor TMakeInstruction.Create(id, classToMake: string);
begin
  inherited Create(id);
  Self.ClassToMake := classToMake;
end;

function TMakeInstruction.Execute(context : TSlimContext): TSlimDirective;
var typeFound : TRttiType;
  importPath : string;
begin
  Result := inherited;
  typeFound := TRttiContext.Create.FindType(ClassToMake);
  if typeFound = nil then
  begin
    for importPath in context.ImportPaths do
    begin
      typeFound := TRttiContext.Create.FindType(importPath + '.' + ClassToMake);
      if typeFound <> nil then
        break;
    end;
  end;
  if typeFound = nil then
    Result.Add('__EXCEPTION__:message:<<NO_CLASS ' + ClassToMake + '>>')
  else
  begin
    Result.Add('OK');
  end;

end;

{ TImportInstruction }

constructor TImportInstruction.Create(id, path: string);
begin
  inherited Create(id);
  Self.Path := path;
end;

constructor TImportInstruction.Create;
begin
end;

function TImportInstruction.Execute(context : TSlimContext): TSlimDirective;
begin
  Result := inherited;
  Result.Add('OK');
  context.AddImportPath(Path);
end;

{ TCallInstruction }

function TCallInstruction.Execute(context : TSlimContext): TSlimDirective;
begin
  inherited;
  // NON TESTÉ
  Result := TSlimDirective.Create;
  Result.Add(Id);
  Result.Add('/__VOID__/');
end;

{ TListInstruction }

function TListInstruction.Execute(context : TSlimContext): TSlimDirective;
var i : Integer;
begin
  inherited;
  Result := TSlimDirective.Create;
  for i := 0 to GetLength - 1 do
  begin
    Result.Add(GetItem(i).Execute(context));
  end;
end;

end.
