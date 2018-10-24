unit ToleranceObj;

interface

uses System.Classes,
    VdpDMAccess;

  type
  TToleranceItem = record
    Index : byte;
    dF25, dF,
    Iter_dF25, Iter_dF : double;
    Name : ShortString;
  end;
  pToleranceItem = ^TToleranceItem;
  TToleranceGroup = class(TList)
   private
    fID : Integer;
    fGroupName : String;
   protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    function Get(Index: Integer): pToleranceItem;
   public
    function NewItem : pToleranceItem;
    function First : pToleranceItem; reintroduce;
    function Last : pToleranceItem; reintroduce;
    procedure Clear; override;
    property Items[Index: Integer]: pToleranceItem read Get; default;
    procedure Read(const ASource : TStrings; AID: integer);overload;
    procedure Write(const ADest: TStrings); overload;
    procedure Assign(const ASource : TToleranceGroup);
    property ID : Integer read fID;
    property GroupName : String read fGroupName;
    function Load(const ADM: IDMAccess; const AConnection : TvdConnection; const AID: integer):boolean;
    function LoadByCardID(const ADM: IDMAccess; const AConnection : TvdConnection; const AID: integer):boolean;
    function DeleteFormDB(const ADM: IInterface): boolean;
    function Save(const ADM: IInterface; const AGroupName : String): boolean;
  end;


implementation
uses System.SysUtils,
System.IniFiles,
Data.DB,
FireDAC.Stan.Param,
Vodopad.CustomIniHelpers, FireDAC.Stan.Intf;


{ TTolerance }

procedure TToleranceGroup.Assign(const ASource: TToleranceGroup);
var
vSt : TStringList;
begin
  vSt := TStringList.Create;
  try
    ASource.Write(vSt);
    Read(vSt, ASource.fID);
  finally
    FreeAndNil(vSt);
  end;
end;

procedure TToleranceGroup.Clear;
begin
  while count > 0 do
    delete(count -1);
  inherited;
end;

function TToleranceGroup.First: pToleranceItem;
begin
  result := Get(0);
end;

function TToleranceGroup.Get(Index: Integer): pToleranceItem;
begin
  Result := pToleranceItem(inherited Get(Index));
end;

function TToleranceGroup.Last: pToleranceItem;
begin
  result := Get(Count - 1);
end;

function TToleranceGroup.NewItem: pToleranceItem;
begin
  New(result);
  result^.Index := byte(count + 1);
  result^.dF25 := 0;
  result^.dF := 0;
  result^.Iter_dF25 := 0;
  result^.Iter_dF := 0;
  inherited Add(result);
end;

procedure TToleranceGroup.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
      Dispose(Ptr);
  inherited Notify(Ptr, Action);
end;

procedure TToleranceGroup.Read(const ASource: TStrings; AID: integer);
var
vPMLocalFormatSettings: TFormatSettings;
vIni : TMemIniFile;
i : Integer;
vItem : pToleranceItem;
begin
  self.Clear;
  if ASource.Count < 1 then
    exit;
  fID := AID;
  vPMLocalFormatSettings:= TFormatSettings.Create(1033);
  vIni := TMemIniFile.Create('');
  try
    vIni.SetStrings(ASource);
    vIni.ReadSections(ASource);
    i := 0;
    while i < ASource.Count do
    begin
      vItem := self.NewItem;
      vItem^.Name := ASource[i];
      vItem^.dF := Vodopad.CustomIniHelpers.ReadFloat(vIni, ASource[i],'dF',0.2, vPMLocalFormatSettings);
      vItem^.dF25 := Vodopad.CustomIniHelpers.ReadFloat(vIni, ASource[i],'dF25',0.2, vPMLocalFormatSettings);
      vItem^.Iter_dF := Vodopad.CustomIniHelpers.ReadFloat(vIni, ASource[i],'Iter_dF',0.2, vPMLocalFormatSettings);
      vItem^.Iter_dF25 := Vodopad.CustomIniHelpers.ReadFloat(vIni, ASource[i],'Iter_dF25',0.2, vPMLocalFormatSettings);
      inc(i);
    end;
  finally
    FreeAndNil(vIni);
  end;
end;

procedure TToleranceGroup.Write(const ADest: TStrings);
var
vPMLocalFormatSettings: TFormatSettings;
vIni : TMemIniFile;
i : Integer;
vItem : pToleranceItem;
begin
  vPMLocalFormatSettings:= TFormatSettings.Create(1033);
  vIni := TMemIniFile.Create('');
  try
    i := 0;
    while i < self.Count do
    begin
      vItem := self.Items[i];
      Vodopad.CustomIniHelpers.WriteFloat(vIni, vItem^.Name,'dF', vItem^.dF, vPMLocalFormatSettings);
      Vodopad.CustomIniHelpers.WriteFloat(vIni, vItem^.Name,'dF25',vItem^.dF25, vPMLocalFormatSettings);
      Vodopad.CustomIniHelpers.WriteFloat(vIni, vItem^.Name,'Iter_dF',vItem^.Iter_dF, vPMLocalFormatSettings);
      Vodopad.CustomIniHelpers.WriteFloat(vIni, vItem^.Name,'Iter_dF25',vItem^.Iter_dF25, vPMLocalFormatSettings);
      inc(i);
    end;
    vIni.GetStrings(ADest);
  finally
    FreeAndNil(vIni);
  end;
end;


function TToleranceGroup.Load(const ADM: IDMAccess; const AConnection : TvdConnection; const AID: integer):boolean;
var
vQR : TvdQuery;
vStrings : TStringList;
//vST : TStream;
begin
  result := False;
  fID := AID;
  Self.Clear;
  vQR := ADM.CreateReadQuery(nil, 'SELECT TOLERANCES.ID, TOLERANCES.NAME, TOLERANCES."FILE" '+
                          'FROM TOLERANCES WHERE (TOLERANCES.ID = :TOLERANCE_ID)', AConnection);
  vStrings := TStringList.Create;
  try
     try
      vQR.Prepare;
      vQR.ParamByName('TOLERANCE_ID').AsInteger := AID;
      vQR.Open;
      if (vQR.RecordCount  > 0)
      and (not vQR.FieldByName('FILE').IsNull) then
      begin
        fGroupName := vQR.FieldByName('NAME').AsString;
        vStrings.Text := vQR.FieldByName('FILE').AsWideString;
        Read(vStrings, fID);
      end;

      vQR.Close;
     except on e : Exception do
        ADM.OnException(vQR, e);
     end;
  finally
    Result := True;
    FreeAndNil(vQR);
    FreeAndNil(vStrings);
  end;
end;

function TToleranceGroup.LoadByCardID(const ADM: IDMAccess; const AConnection : TvdConnection;
  const AID: integer): boolean;
var
vSelectQR : TvdQuery;
begin
  result := false;
  vSelectQR := ADM.CreateReadQuery(nil, 'SELECT TOLERANCE '+
    'FROM RCARD WHERE (RCARD.ID = :RCARD_ID) ORDER BY TOLERANCE', AConnection);
  try
    vSelectQR.Prepare;
    try
      vSelectQR.ParamByName('RCARD_ID').AsInteger := AID;
      vSelectQR.Open;
      if (vSelectQR.RecordCount > 0) then
        result := Load(ADM, AConnection, vSelectQR.FieldByName('TOLERANCE').AsInteger);
    except
      on E : exception do
      begin
        result := false;
        ADM.OnException(vSelectQR, E);
      end;
    end;
  finally
    FreeAndNil(vSelectQR);
  end;
end;

function TToleranceGroup.Save(const ADM: IInterface; const AGroupName : String):boolean;
var
vDM : IDMAccess;
vSP : TvdStoredProc;
vST : TStringList;
vSParam : TFDParam;
begin
  Result := False;
  if not Supports(ADM, IDMAccess, vDM) then
    Exit;
  vST := TStringList.Create;
  vSP := vDM.CreateWriteStoredProc(nil, 'SAVE_TOLERANCE');
  try
    Write(vST);
    vSP.Prepare;
    vSP.ParamByName('ID').AsInteger := fID;
    vSP.ParamByName('NAME').AsString := AGroupName;
    vSParam := vSP.ParamByName('FILE');
    vSParam.AsWideMemo := vST.Text;
    try
      vSP.Open;
      fID := vSP.FieldByName('OUT_ID').AsInteger;
      vDM.Commit(vSP);
      fGroupName := AGroupName;
      result := True;
    except
      on e : Exception do
        vDM.OnException(vSP, e);
    end;
  finally
    FreeAndNil(vSP);
    FreeAndNil(vST);
    vDM := nil;
  end;
end;

function TToleranceGroup.DeleteFormDB(const ADM: IInterface):boolean;
var
vDM : IDMAccess;
vSP : TvdStoredProc;
begin
  Result := False;
  if not Supports(ADM, IDMAccess, vDM) then
    Exit;
  vSP := vDM.CreateWriteStoredProc(nil, 'DELETE_TOLERANCE');
  try
    vSP.Prepare;
    vSP.ParamByName('ID').AsInteger := fID;
    try
      vSP.ExecProc;
      vDM.Commit(vSP);
      Result := True;
    except
      on e : Exception do
        vDM.OnException(vSP, e);
    end;
  finally
    FreeAndNil(vSP);
    vDM := nil;
  end;
end;

end.
