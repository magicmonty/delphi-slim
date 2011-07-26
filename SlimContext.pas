unit SlimContext;

interface

uses Classes, Generics.Collections;

type TSlimContext = class
  strict private
    _loadedPackages : TList<HMODULE>;
    _instances : TDictionary<string, TObject>;
  public
    ImportPaths : TStrings;
    PackagePaths : TStrings;
    constructor Create;
    procedure AddImportPath(path : string);
    procedure AddPackagePath(path : string);
    procedure Init;
    function GetRegisteredInstance(instanceName : string) : TObject;
    procedure RegisterInstance(instanceName : string; instance : TObject);
    destructor Destroy; override;
end;

implementation

uses SysUtils;

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
  _instances := TDictionary<string,TObject>.Create;
end;

destructor TSlimContext.Destroy;
var package : HMODULE;
begin
  for package in _loadedPackages do
    UnloadPackage(package);
  inherited;
end;



procedure TSlimContext.Init;
var packagePath : string;
begin
  _loadedPackages := TList<HMODULE>.Create;
  for packagePath in PackagePaths do
  begin
    try
      _loadedPackages.Add(LoadPackage(packagePath));
    except
    end;
  end;
end;

procedure TSlimContext.RegisterInstance(instanceName: string; instance: TObject);
begin
  _instances.Add(instanceName, instance);
end;

function TSlimContext.GetRegisteredInstance(instanceName: string): TObject;
begin
  Result := _instances[instanceName];
end;

end.
