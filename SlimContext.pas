unit SlimContext;

interface

uses Classes;

type TSlimContext = class
  public
    ImportPaths : TStrings;
    PackagePaths : TStrings;
    constructor Create;
    procedure AddImportPath(path : string);
    procedure AddPackagePath(path : string);
end;

implementation

{ TSlimContext }

procedure TSlimContext.AddImportPath(path: string);
begin
  ImportPaths.Add(path);
end;

procedure TSlimContext.AddPackagePath(path : string);
begin
  PackagePaths.Add(path);
end;

constructor TSlimContext.Create;
begin
  ImportPaths := TStringList.Create;
  PackagePaths := TStringList.Create;
end;

end.
