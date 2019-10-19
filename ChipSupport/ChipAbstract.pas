unit ChipAbstract;

interface


uses System.Classes, System.IniFiles,
    AbstractExtention, ChipAbstractInterface;


  type
  
  TChipAbstract = class;
  TChipClass = class of TChipAbstract;
  TChipAbstract = class(TAbstractExtention, IChipAbstract)
  private
    {IChipAbstract}
    function GetInstance : TObject; stdcall;
    function IChipAbstract.BitMinIndex = IChipAbstract_BitMinIndex;
    function IChipAbstract.BitMaxIndex = IChipAbstract_BitMaxIndex ;
    function IChipAbstract.ChipGUID  = IChipAbstract_ChipGUID;
    function IChipAbstract.ChipName = IChipAbstract_ChipName;
    function IChipAbstract.BitMinValue = IChipAbstract_BitMinValue;
    function IChipAbstract.BitMeanValue = IChipAbstract_BitMeanValue;
    function IChipAbstract.BitMaxValue = IChipAbstract_BitMaxValue;
    function IChipAbstract.BitDefValue = IChipAbstract_BitDefValue;
    procedure IChipAbstract.ReadControlledBitsIndex = IChipAbstract_ReadControlledBitsIndex;
    function IChipAbstract.ReadIntConstant = IChipAbstract_ReadIntConstant;  
    function IChipAbstract.ReadStrConstant = IChipAbstract_ReadStrConstant;
    function IChipAbstract.IsIdenty = IChipAbstract_IsIdenty;
    procedure IChipAbstract.Assign = IChipAbstract_Assign;
    function IChipAbstract_IsIdenty(const ASource : IChipAbstract; const ABitsIndex : TChipBytes=nil): boolean; stdcall;
    procedure IChipAbstract_Assign(const ASource : IChipAbstract; const ABitsIndex : TChipBytes=nil); stdcall;
    function IChipAbstract_BitMinIndex : byte; stdcall;
    function IChipAbstract_BitMaxIndex : byte; stdcall;
    function IChipAbstract_ChipGUID : TGUID; stdcall;
    function IChipAbstract_ChipName: String; stdcall;
    function IChipAbstract_BitMinValue(AIndex : byte): smallint; stdcall;
    function IChipAbstract_BitMeanValue(AIndex : byte): smallint; stdcall;
    function IChipAbstract_BitMaxValue(AIndex : byte): smallint; stdcall;
    function IChipAbstract_BitDefValue(AIndex : byte): smallint; stdcall;
    procedure IChipAbstract_ReadControlledBitsIndex(const ASection : string; ADefault : Boolean; out oBitsIndex : TChipBytes); stdcall;
    function IChipAbstract_ReadIntConstant(const ASection, AIdent : string; var oConst : Integer): Boolean;overload; stdcall;
    function IChipAbstract_ReadStrConstant(const ASection, AIdent : string; var oConst : String): Boolean;overload; stdcall;
  protected
    function GetBitValue(AIndex : byte):smallint; virtual; stdcall; abstract;
    procedure SetBitValue(AIndex : byte; AValue: smallInt); virtual; stdcall; abstract;
  public
    function IsIdenty(const ASource : TChipAbstract; const ABitsIndex : TChipBytes=nil): boolean; overload;
    //function IsIdenty(const ASource : IChipAbstract; const ABitsIndex : TChipBytes=nil): boolean;overload;
    procedure Assign(const ASource : TChipAbstract; const ABitsIndex : TChipBytes=nil); overload;
    //procedure Assign(const ASource : IChipAbstract; const ABitsIndex : TChipBytes=nil); overload;
    procedure Clear; virtual;stdcall;
    function CreateClone(AOwner : TComponent) : TChipAbstract; virtual;
    procedure SaveToStream(AStream : TMemoryStream); virtual; abstract;
    procedure LoadFromStream(AStream : TStream); virtual; abstract;   
    procedure SaveToIni(AIniFile : TCustomIniFile; const ASection : string; const ABitsIndex : TChipBytes=nil); virtual;
    procedure LoadFromIni(AIniFile : TCustomIniFile; const ASection : string; const ABitsIndex : TChipBytes=nil); virtual;
    class function ChipClassType : TChipClass; virtual; abstract;
    class function ChipName: String; virtual; abstract;
    class function ChipGUID : TGUID; virtual; abstract;
    class function BitMinIndex : byte; virtual; abstract;
    class function BitMaxIndex : byte; virtual; abstract;
    class function BitName(AIndex : byte): String; virtual; abstract;
    class function BitMinValue(AIndex : byte): smallint; virtual; abstract;
    class function BitMeanValue(AIndex : byte): smallint; virtual; abstract;
    class function BitMaxValue(AIndex : byte): smallint; virtual; abstract;
    class function BitDefValue(AIndex : byte): smallint; virtual; abstract;
    class procedure ReadControlledBitsIndex(const ASection : string; ADefault : Boolean; out oBitsIndex : TChipBytes);
    class function ReadConstant(const ASection, AIdent : string; var oConst : Integer): Boolean;overload;
    class function ReadConstant(const ASection, AIdent : string; var oConst : String): Boolean;overload;
    property BitValue[AIndex : byte]:smallint read GetBitValue write SetBitValue;
  end;
  
procedure ReadControlledBitsIndex(AChipClass : TChipClass; const ASection : string; ADefault : Boolean; out oBitsIndex : TChipBytes);
function ReadConstant(AChipClass : TChipClass; const ASection, AIdent : string; var oConst : Integer): Boolean; overload;
function ReadConstant(AChipClass : TChipClass; const ASection, AIdent : string; var oConst : String): Boolean; overload;


const
C_Default_CHIP : TGUID = '{00000000-0000-0000-0000-000000000000}';

implementation

uses
System.SysUtils;

function ReadConstant(AChipClass : TChipClass; const ASection, AIdent : string; var oConst : Integer):Boolean;
var
  vFileName : TFileName;
  f : TIniFile;
begin
  vFileName := ExtractFilePath(ParamStr(0))+AChipClass.ChipName+'.ini';
  f := TIniFile.Create(vFileName);
  try
    Result := f.ValueExists(ASection, AIdent);
    if Result then
      oConst := f.ReadInteger(ASection, AIdent, 0);
  finally
    FreeAndNil(f);
  end;
end;

function ReadConstant(AChipClass : TChipClass; const ASection, AIdent : string; var oConst : String):Boolean;
var
  vFileName : TFileName;
  f : TIniFile;
begin
  vFileName := ExtractFilePath(ParamStr(0))+AChipClass.ChipName+'.ini';
  f := TIniFile.Create(vFileName);
  try
    Result := f.ValueExists(ASection, AIdent);
    if Result then
      oConst := f.ReadString(ASection, AIdent, '');
  finally
    FreeAndNil(f);
  end;
end;

procedure ReadControlledBitsIndex(AChipClass : TChipClass; const ASection : string; ADefault : Boolean; out oBitsIndex : TChipBytes);
var
  vFileName : TFileName;
  f : TIniFile;
  vBitIndex : Byte;
begin
  vFileName := ExtractFilePath(ParamStr(0))+AChipClass.ChipName+'.ini';
  f := TIniFile.Create(vFileName);
  try
    SetLength(oBitsIndex, 0);
    for vBitIndex := AChipClass.BitMinIndex to AChipClass.BitMaxIndex do
    begin
      if f.ReadBool(ASection, AChipClass.BitName(vBitIndex), ADefault) then
      begin
        SetLength(oBitsIndex, Length(oBitsIndex)+1);
        oBitsIndex[High(oBitsIndex)] := vBitIndex;
      end;
    end;
  finally
    FreeAndNil(f);
  end;
end;

{ TImsAbstract }

procedure TChipAbstract.Assign(const ASource: TChipAbstract; const ABitsIndex: TChipBytes);
var
  i : byte;
begin
  if not Assigned(ABitsIndex) then
  begin
    for I := BitMinIndex to BitMaxIndex do
      BitValue[i] := ASource.BitValue[i];
  end else if (Length(ABitsIndex) > 0) then
    for I := Low(ABitsIndex) to High(ABitsIndex) do
      BitValue[ABitsIndex[i]] := ASource.BitValue[ABitsIndex[i]];
end;

function TChipAbstract.IsIdenty(const ASource: TChipAbstract; const ABitsIndex: TChipBytes): boolean;
var
  i : byte;
begin
  result := False;
  if not Assigned(ABitsIndex) then
  begin
    for I := BitMinIndex to BitMaxIndex do
    begin
      result := BitValue[i] = ASource.BitValue[i];
      if not result then
        break;
    end;
  end else if (Length(ABitsIndex) > 0) then
    for I := Low(ABitsIndex) to High(ABitsIndex) do
    begin
      result := BitValue[ABitsIndex[i]] = ASource.BitValue[ABitsIndex[i]];
      if not result then
        break;
    end;
end;

procedure TChipAbstract.Clear;
var
  i : byte;
begin
  for i := BitMinIndex to BitMaxIndex do
    self.BitValue[i] := 0;
end;

function TChipAbstract.CreateClone(AOwner : TComponent): TChipAbstract;
begin
  result := ChipClassType.Create(AOwner);
  result.Assign(self);
end;

function TChipAbstract.GetInstance: TObject;
begin
  Result := self;
end;

procedure TChipAbstract.IChipAbstract_Assign(const ASource: IChipAbstract; const ABitsIndex: TChipBytes);
var
  i : byte;
begin
  if not Assigned(ABitsIndex) then
  begin
    for I := BitMinIndex to BitMaxIndex do
      BitValue[i] := ASource.BitValue[i];
  end else if (Length(ABitsIndex) > 0) then
    for I := Low(ABitsIndex) to High(ABitsIndex) do
      BitValue[ABitsIndex[i]] := ASource.BitValue[ABitsIndex[i]];
end;

function TChipAbstract.IChipAbstract_BitDefValue(AIndex: byte): smallint;
begin
  Result := BitDefValue(AIndex);
end;

function TChipAbstract.IChipAbstract_BitMaxIndex: byte;
begin
  Result := BitMaxIndex;
end;

function TChipAbstract.IChipAbstract_BitMaxValue(AIndex: byte): smallint;
begin
  Result := BitMaxValue(AIndex);
end;

function TChipAbstract.IChipAbstract_BitMeanValue(AIndex: byte): smallint;
begin
  Result := BitMeanValue(AIndex);
end;

function TChipAbstract.IChipAbstract_BitMinIndex: byte;
begin
  Result := BitMinIndex;
end;

function TChipAbstract.IChipAbstract_BitMinValue(AIndex: byte): smallint;
begin
  Result := BitMinValue(AIndex);
end;

function TChipAbstract.IChipAbstract_ChipGUID: TGUID;
begin
  Result := ChipGUID;
end;

function TChipAbstract.IChipAbstract_ChipName: String;
begin
  Result := ChipName;
end;

function TChipAbstract.IChipAbstract_IsIdenty(const ASource: IChipAbstract; const ABitsIndex: TChipBytes): boolean;
var
  i : byte;
begin
  result := False;
  if not Assigned(ABitsIndex) then
  begin
    for I := BitMinIndex to BitMaxIndex do
    begin
      result := BitValue[i] = ASource.BitValue[i];
      if not result then
        break;
    end;
  end else if (Length(ABitsIndex) > 0) then
    for I := Low(ABitsIndex) to High(ABitsIndex) do
    begin
      result := BitValue[ABitsIndex[i]] = ASource.BitValue[ABitsIndex[i]];
      if not result then
        break;
    end;
end;

function TChipAbstract.IChipAbstract_ReadIntConstant(const ASection, AIdent: string; var oConst: Integer): Boolean;
begin
  Result := ReadConstant(ASection, AIdent, oConst);
end;

function TChipAbstract.IChipAbstract_ReadStrConstant(const ASection, AIdent: string; var oConst: String): Boolean;
begin
  Result := ReadConstant(ASection, AIdent, oConst);
end;

procedure TChipAbstract.IChipAbstract_ReadControlledBitsIndex(const ASection: string; ADefault: Boolean; out oBitsIndex: TChipBytes);
begin
  ReadControlledBitsIndex(ASection, ADefault, oBitsIndex);
end; 

procedure TChipAbstract.LoadFromIni(AIniFile: TCustomIniFile;
  const ASection: string; const ABitsIndex : TChipBytes);
var
  i : byte;
begin
  if not Assigned(ABitsIndex) then
  begin
    for I := BitMinIndex to BitMaxIndex do
      BitValue[i] := SmallInt(AIniFile.ReadInteger(ASection, BitName(i), BitValue[i]));
  end else if (Length(ABitsIndex) > 0) then
  begin
    for i := Low(ABitsIndex) to High(ABitsIndex) do
      BitValue[ABitsIndex[i]] := SmallInt(AIniFile.ReadInteger(ASection, BitName(ABitsIndex[i]), BitValue[ABitsIndex[i]]));
  end;
end;

procedure TChipAbstract.SaveToIni(AIniFile: TCustomIniFile;
  const ASection: string; const ABitsIndex : TChipBytes);
var
  i : byte;
begin
  if not Assigned(ABitsIndex) then
  begin
    for I := BitMinIndex to BitMaxIndex do
      AIniFile.WriteInteger(ASection, BitName(i), BitValue[i]);
  end else if (Length(ABitsIndex) > 0) then
  begin
    for i := Low(ABitsIndex) to High(ABitsIndex) do
      AIniFile.WriteInteger(ASection, BitName(ABitsIndex[i]), BitValue[ABitsIndex[i]]);
  end;
end;

class function TChipAbstract.ReadConstant(const ASection, AIdent: string; var oConst: Integer): Boolean;
begin
  Result := ChipAbstract.ReadConstant(ChipClassType, ASection, AIdent, oConst);
end;

class function TChipAbstract.ReadConstant(const ASection, AIdent: string; var oConst: String): Boolean;
begin
  Result := ChipAbstract.ReadConstant(ChipClassType, ASection, AIdent, oConst);
end;

class procedure TChipAbstract.ReadControlledBitsIndex(const ASection: string;
  ADefault: Boolean; out oBitsIndex: TChipBytes);
begin
  ChipAbstract.ReadControlledBitsIndex(ChipClassType, ASection, ADefault, oBitsIndex);
end;

end.
