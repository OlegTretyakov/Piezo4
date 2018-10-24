unit VdpDataModule;

interface
 uses
  System.Classes, System.SysUtils, System.IniFiles, VdpDMAccess, LoggerInterface,
  FireDAC.Stan.Def, FireDAC.Stan.Async, FireDac.DApt, FireDAC.Comp.Client, FireDAC.Phys.FB;

 type
 TVdpDM = class (TComponent, IDMAccess, ILoggingConfStoreAccess)
  strict private
   fDriverLink: TFDPhysFBDriverLink;
   fConnection : TvdConnection;
   fLogger : ILogger;
   fConnectionName : string;
  private
   function CreateReadTransaction(const AOwner : TComponent; const AConnection : TvdConnection):TvdTransaction; stdcall;
   function CreateWriteTransaction(const AOwner : TComponent; const AConnection : TvdConnection):TvdTransaction; stdcall;
   procedure Log(Level: TLogInfo; const AMessage: string); overload; stdcall;
   procedure Log(Level: TLogInfo; Instance: TObject); overload; stdcall;
   procedure OnError(ASender, AInitiator: TObject; var AException: Exception);
   {$IFDEF DEBUG}
   procedure DatabaseBeforeDisconnect(Sender: TObject);
   {$ENDIF}
   {ILoggingConfStoreAccess}
   procedure LoadLoggerConfig(const ALogger : ILogger); stdcall;
   procedure SaveLoggerConfig(const ALogger : ILogger); stdcall;
   procedure LoadLoggersConfig(const ALoggers : ILoggers); stdcall;
   procedure SaveLoggersConfig(const ALoggers : ILoggers); stdcall;
  public
   constructor Create(AOwner: TComponent);override;
   destructor Destroy; override;
   procedure SetLogger(const ALogger : ILogger);
   {IDMAccess}
   function DBHomePath : string; stdcall;
   function MeasuresHomePath : string; stdcall;
   function Connected: Boolean; stdcall;
   function CreateConnection(const AOwner : TComponent) : TvdConnection;stdcall;
   procedure CloseConnection(const AConnection : TvdConnection);stdcall;
   function CreateQuery(const AOwner : TComponent; const AConnection : TvdConnection = nil):TvdQuery; stdcall;
   function CreateStoredProc(const AOwner : TComponent; const AConnection : TvdConnection = nil):TvdStoredProc; stdcall;
   function CreateReadQuery(const AOwner : TComponent; const AConnection : TvdConnection = nil) : TvdQuery; overload; stdcall;
   function CreateReadStoredProc(const AOwner : TComponent; const AConnection : TvdConnection = nil) : TvdStoredProc; overload;stdcall;
   function CreateReadQuery(const AOwner : TComponent; const SQLText : string; const AConnection : TvdConnection = nil) : TvdQuery; overload; stdcall;
   function CreateReadStoredProc(const AOwner : TComponent; const AStoredProcName : string; const AConnection : TvdConnection = nil) : TvdStoredProc; overload; stdcall;
   function CreateWriteQuery(const AOwner : TComponent; const AConnection : TvdConnection = nil) : TvdQuery; overload; stdcall;
   function CreateWriteStoredProc(const AOwner : TComponent; const AConnection : TvdConnection = nil) : TvdStoredProc; overload; stdcall;
   function CreateWriteQuery(const AOwner : TComponent; const SQLText : string; const AConnection : TvdConnection = nil) : TvdQuery; overload; stdcall;
   function CreateWriteStoredProc(const AOwner : TComponent; const AStoredProcName : string; const AConnection : TvdConnection = nil) : TvdStoredProc; overload; stdcall;
   procedure Commit(const ADataSet : TvdDataSet); stdcall;
   procedure OnException(const ADataSet : TvdDataSet; E : Exception); stdcall;
   function Connect: boolean;
 end;



implementation
uses
System.IOUtils,
FireDAC.Comp.DataSet,
FireDAC.Stan.Intf,
FireDAC.Stan.Pool,
IniRoutines,
FireDAC.Stan.Option,
Data.DB,
FireDAC.Phys.FBDef,
FireDAC.Phys.IBWrapper,
Winapi.Windows;


constructor TVdpDM.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fDriverLink := TFDPhysFBDriverLink.Create(self);
  fLogger := nil;
  fConnection := nil;
  FDManager.SilentMode := True;
  FDManager.ResourceOptions.KeepConnection := True;
  FDManager.ResourceOptions.AutoReconnect := True;
  FDManager.ResourceOptions.CmdExecMode := amBlocking;
  FDManager.ResourceOptions.SilentMode := True;
end;

function TVdpDM.CreateConnection(const AOwner : TComponent): TvdConnection;
var
vCT : Cardinal;
begin
  Result := TvdConnection.Create(AOwner);
  vCT := GetCurrentThreadID;
  {$IFDEF DEBUG}
  Log(lDebug, Format('Thread ID:%d create connection',[vCT]));
  {$ENDIF}
  Result.Tag := vCT;
  Result.ConnectionDefName := fConnectionName;
  Result.OnError := OnError;
  Result.ConnectedStoredUsage := [];
  Result.ResourceOptions.SilentMode := true;
  Result.ResourceOptions.PreprocessCmdText := True;
  Result.ResourceOptions.ParamCreate := True;
  Result.FetchOptions.Mode := fmAll;
  Result.FetchOptions.CursorKind := ckDynamic;
  Result.FetchOptions.Items := [fiBlobs, fiDetails, fiMeta];
  Result.LoginPrompt := false;
  Result.TxOptions.DisconnectAction := xdRollback;
  Result.TxOptions.Isolation := xiSnapshot;
  Result.TxOptions.ReadOnly := True;
  Result.TxOptions.AutoCommit := true;
  Result.TxOptions.AutoStart := true;
  Result.TxOptions.AutoStop := true;
  //Result.TxOptions.EnableNested := False;
  Result.Params.Pooled := true;
  Result.UpdateOptions.ReadOnly := false;
  Result.UpdateOptions.LockWait := false;
  Result.UpdateOptions.EnableDelete := True;
  Result.UpdateOptions.EnableInsert := True;
  Result.UpdateOptions.EnableUpdate := True;
  Result.UpdateOptions.LockMode :=lmPessimistic;
  Result.UpdateOptions.RefreshMode := rmOnDemand;
  {$IFDEF DEBUG}
  Result.BeforeDisconnect := DatabaseBeforeDisconnect;
  {$ENDIF}
  Result.Transaction := CreateReadTransaction(Result, Result);
  Result.UpdateTransaction := CreateWriteTransaction(Result, Result);
end;

procedure TVdpDM.CloseConnection(const AConnection: TvdConnection);
begin
  if Assigned(AConnection.UpdateTransaction) and AConnection.UpdateTransaction.Active then
    AConnection.UpdateTransaction.Rollback;
  if Assigned(AConnection.Transaction) and AConnection.Transaction.Active then
    AConnection.Transaction.Rollback;
  if AConnection.Connected then
    AConnection.Close;
end;

function TVdpDM.CreateReadTransaction(const AOwner: TComponent; const AConnection : TvdConnection): TvdTransaction;
begin
  result := TvdTransaction.Create(AOwner);
  result.Options.Isolation := xiReadCommitted;
  result.Options.AutoCommit := false;

  result.Options.AutoStart := true;
  result.Options.AutoStop := false;
  result.Options.ReadOnly := true;
  result.Options.EnableNested := False;
  result.Options.DisconnectAction := xdCommit;
  {result.Options.Params.Add('read');
  result.Options.Params.Add('nowait');
  result.Options.Params.Add('rec_version');
  result.Options.Params.Add('read_committed'); }
  result.Connection := AConnection;
end;

function TVdpDM.CreateWriteTransaction(const AOwner: TComponent; const AConnection : TvdConnection): TvdTransaction;
begin
  result := TvdTransaction.Create(AOwner);
  result.Options.Isolation := xiSnapshot;
  result.Options.AutoCommit := true;
  result.Options.AutoStart := true;
  result.Options.AutoStop := true;
  result.Options.ReadOnly := False;
  result.Options.EnableNested := False;
  result.Options.DisconnectAction := xdRollback;
  {result.Options.Params.Add('write');
  result.Options.Params.Add('nowait');
  result.Options.Params.Add('rec_version');
  result.Options.Params.Add('read_committed'); }
  result.Connection := AConnection;
end;

destructor TVdpDM.Destroy;
begin
  try
    if Connected then
      CloseConnection(fConnection);
    FreeAndNil(fConnection);
    FreeAndNil(fDriverLink);
    FDManager.Close;
  finally
    fLogger := nil;
    inherited Destroy;
  end;
end;

procedure TVdpDM.Log(Level: TLogInfo; const AMessage: string);
begin
  if Assigned(fLogger) then
    fLogger.Log(Level, AMessage);
end;  

procedure TVdpDM.Log(Level: TLogInfo; Instance: TObject);
begin
  if Assigned(fLogger) then
    fLogger.Log(Level, Instance);
end;

function TVdpDM.CreateQuery(const AOwner: TComponent; const AConnection : TvdConnection): TvdQuery;
var
vCT : Cardinal;
begin
  result := TvdQuery.Create(AOwner);

  if assigned(AConnection) then
    result.Connection := AConnection
  else
    result.Connection := fConnection;
  vCT := GetCurrentThreadID;
  if (vCT <> result.Connection.Tag) then
  begin
    Log(lWarning, Format('Thread ID %d tryed create query with connection for Thread ID %d ',
    [vCT, result.Connection.Tag]));
  end;
  result.Transaction := result.Connection.Transaction;
  result.UpdateTransaction := result.Connection.UpdateTransaction;

  result.FieldOptions.PositionMode := poFirst;
  result.ResourceOptions.CmdExecMode := amBlocking;
  result.UpdateOptions.RequestLive := False;
  result.ResourceOptions.PreprocessCmdText := True;
  Result.ResourceOptions.ParamCreate := True;
  result.UpdateOptions.ReadOnly := True;
  result.UpdateOptions.FetchGeneratorsPoint := gpNone;
  result.OnError := OnError;
  result.FetchOptions.AutoClose := True;
  result.FetchOptions.AutoFetchAll := afAll;
  result.FetchOptions.Mode := fmAll;
  result.FetchOptions.Items := [fiBlobs, fiDetails, fiMeta];
end;

function TVdpDM.CreateStoredProc(const AOwner: TComponent; const AConnection : TvdConnection): TvdStoredProc;
var
vCT : Cardinal;
begin
  result := TvdStoredProc.Create(AOwner);

  if assigned(AConnection) then
    result.Connection := AConnection
  else
    result.Connection := fConnection;
  vCT := GetCurrentThreadID;
  if (vCT <> result.Connection.Tag) then
  begin
    Log(lWarning, Format('Thread ID %d tryed create stored procedure with connection for Thread ID %d ',
    [vCT, result.Connection.Tag]));
  end;

  result.Transaction := result.Connection.Transaction;
  result.UpdateTransaction := result.Connection.UpdateTransaction;
  result.FieldOptions.PositionMode := poFirst;
  result.ResourceOptions.CmdExecMode := amBlocking;
  result.ResourceOptions.PreprocessCmdText := True;
  Result.ResourceOptions.ParamCreate := True;
  result.UpdateOptions.RequestLive := False;
  result.UpdateOptions.ReadOnly := True;
  result.UpdateOptions.FetchGeneratorsPoint := gpNone;
  result.OnError := OnError;
  result.FetchOptions.AutoClose := True;
  result.FetchOptions.AutoFetchAll := afAll;
  result.FetchOptions.Mode := fmAll;
  result.FetchOptions.Items := [fiBlobs, fiDetails, fiMeta];
end;

function TVdpDM.CreateReadQuery(const AOwner : TComponent; const AConnection : TvdConnection): TvdQuery;
begin
  result := CreateQuery(AOwner, AConnection);
end;

function TVdpDM.CreateReadQuery(const AOwner : TComponent; const SQLText: string; const AConnection : TvdConnection): TvdQuery;
begin
  result := CreateReadQuery(AOwner, AConnection);
  result.SQL.Text := SQLText;
end; 

function TVdpDM.CreateReadStoredProc(const AOwner : TComponent; const AConnection : TvdConnection): TvdStoredProc;
begin
  result := CreateStoredProc(AOwner, AConnection);
end;

function TVdpDM.CreateReadStoredProc(const AOwner : TComponent; const AStoredProcName: string; const AConnection : TvdConnection): TvdStoredProc;
begin
  result := CreateReadStoredProc(AOwner, AConnection);
  result.StoredProcName := AStoredProcName;
end;

function TVdpDM.CreateWriteQuery(const AOwner : TComponent; const AConnection : TvdConnection): TvdQuery;
begin
  result := CreateQuery(AOwner, AConnection);
  result.Transaction := result.Connection.UpdateTransaction;
  result.UpdateTransaction := result.Connection.UpdateTransaction;
  result.UpdateOptions.ReadOnly := false;
  result.UpdateOptions.EnableDelete := True;
  result.UpdateOptions.EnableInsert := True;
  result.UpdateOptions.EnableUpdate := True;
end; 

function TVdpDM.CreateWriteQuery(const AOwner : TComponent; const SQLText: string; const AConnection : TvdConnection): TvdQuery;
begin
  result := CreateWriteQuery(AOwner, AConnection);
  result.SQL.Text := SQLText;
end;

function TVdpDM.CreateWriteStoredProc(const AOwner : TComponent; const AConnection : TvdConnection): TvdStoredProc;
begin
  result := CreateStoredProc(AOwner, AConnection);
  result.Transaction := result.Connection.UpdateTransaction;
  result.UpdateTransaction := result.Connection.UpdateTransaction;
  result.UpdateOptions.ReadOnly := false;
  result.UpdateOptions.EnableDelete := True;
  result.UpdateOptions.EnableInsert := True;
  result.UpdateOptions.EnableUpdate := True;
end;

function TVdpDM.CreateWriteStoredProc(const AOwner : TComponent; const AStoredProcName: string; const AConnection : TvdConnection): TvdStoredProc;
begin
  result := CreateWriteStoredProc(AOwner, AConnection);
  result.StoredProcName := AStoredProcName;
end;

function TVdpDM.Connect: boolean;
var
vFileName : string;
vIniFile : TIniFile;
vConDef : TStringList;
vParams : TFDPhysFBConnectionDefParams;
begin
  if Connected then
  begin
    Result := True;
    Exit;
  end;

  vFileName := ExtractFilePath(ParamStr(0))+ChangeFileExt(ExtractFileName(ParamStr(0)), '.ini');
  vConDef := TStringList.Create;
  vIniFile := TIniFile.Create(vFileName);
  try
    vConDef.Add('Protocol=Local');
    vConDef.Add('CharacterSet=UTF8');
    vConDef.Add('SQLDialect=3');
    vConDef.Add('Pooled=True');
    vConDef.Add('POOL_CleanupTimeout=60000');
    vConDef.Add('POOL_ExpireTimeout=180000');
    vConDef.Add('POOL_MaximumItems=50');
    fConnectionName := vIniFile.ReadString('Current connection', 'Name', '');
    vFileName := vIniFile.ReadString(fConnectionName, 'DBName', 'base\Piezo4.FDB');
    if not System.ioutils.TDirectory.IsRelativePath(ExtractFilePath(vFileName)) then
      vConDef.Add('Database='+vFileName)
    else
      vConDef.Add('Database='+ExtractFilePath(ParamStr(0))+ vFileName);
    vConDef.Add('User_Name='+vIniFile.ReadString(fConnectionName, 'UserName', 'SYSDBA'));
    vConDef.Add('Password='+vIniFile.ReadString(fConnectionName, 'Password', 'masterkey'));

    fDriverLink.VendorHome := vIniFile.ReadString(fConnectionName, 'LibHome', EmptyStr);
    fDriverLink.VendorLib := vIniFile.ReadString(fConnectionName, 'Lib', 'fbembed.dll');

    FDManager.AddConnectionDef(fConnectionName, fDriverLink.BaseDriverID, vConDef);
  finally
    FreeAndNil(vIniFile);
    FreeAndNil(vConDef);
    Result := FileExists(fDriverLink.VendorHome + fDriverLink.VendorLib);
  end;

  if not Result then
  begin
    Log(lWarning, 'Database driver link library not exists');
    Exit;
  end;
  FDManager.Active := true;
  fConnection := CreateConnection(Self);
  Result := False;
  if fConnection.Params is TFDPhysFBConnectionDefParams then
    vParams := fConnection.Params as TFDPhysFBConnectionDefParams
  else
    Exit;

  try
    fConnection.Connected := true;
    result := fConnection.Connected;
    if result then
    begin
      self.Log(lEvent, Format('DataBase: %s', [vParams.Database]));
    end;
  except on E:Exception do
    begin
      result := false;
        Log(lException, E);
        Log(lWarning, Format('Connection error. Connection params: '+
        'Base Name %s; Lib Name %s; User Name %s; Password %s',
        [vParams.Database, fDriverLink.VendorHome + fDriverLink.VendorLib,
        vParams.UserName, vParams.Password]));
    end;
  end;
end;

function TVdpDM.Connected: Boolean;
begin
  Result := Assigned(fConnection) and fConnection.Connected;
end;
{$IFDEF DEBUG}
procedure TVdpDM.DatabaseBeforeDisconnect(Sender: TObject);
begin
  Log(lDebug, Format('Thread ID:%d close connection',[GetCurrentThreadID]));
end;
{$ENDIF}
function TVdpDM.DBHomePath: string;
begin
  Result := ExtractFilePath(FDManager.ConnectionDefs.ConnectionDefByName(fConnectionName).Params.Database);
end;

function TVdpDM.MeasuresHomePath: string;
begin
  Result := ExtractFilePath(FDManager.ConnectionDefs.ConnectionDefByName(fConnectionName).Params.Database);
end;

procedure TVdpDM.Commit(const ADataSet: TvdDataSet);
begin
  try
    if Assigned(ADataSet.Connection.UpdateTransaction) and ADataSet.Connection.UpdateTransaction.Active then
      ADataSet.Connection.UpdateTransaction.Commit;
    {if Assigned(ADataSet.UpdateTransaction) and ADataSet.UpdateTransaction.Active then
      ADataSet.UpdateTransaction.Commit;  }
    ADataSet.CloseStreams;
  finally
    if ADataSet.Active then
      ADataSet.Close;
  end;
end;

procedure TVdpDM.OnError(ASender, AInitiator: TObject; var AException: Exception);
begin
  Log(lException, AException);
  Log(lWarning, Format('Error in thread ID:%d ',[GetCurrentThreadID]));
  if Assigned(ASender) then
    Log(lError, ASender);
  if assigned(AInitiator) then
    Log(lError, AInitiator);
end;

procedure TVdpDM.OnException(const ADataSet: TvdDataSet; E: Exception);
var
vCT : Cardinal;
begin
  try
    ADataSet.CloseStreams;
    {if Assigned(ADataSet.Transaction) and ADataSet.Transaction.Active then
      ADataSet.Transaction.Rollback;
    if Assigned(ADataSet.UpdateTransaction) and ADataSet.UpdateTransaction.Active then
      ADataSet.UpdateTransaction.Rollback; }
    vCT := GetCurrentThreadID;
    if (ADataSet is TvdStoredProc) then
      Log(lError, Format('Thread ID:%d Error in exec stored procedure %s',[vCT, (ADataSet as TvdStoredProc).StoredProcName]))
    else
    if (ADataSet is TvdQuery) then
       Log(lError, Format('Thread ID:%d Error in exec query %s',[vCT, (ADataSet as TvdQuery).SQL.Text]));
    Log(lException, E);
  finally
    if ADataSet.Active then
      ADataSet.Close;
  end;
end;

procedure TVdpDM.SetLogger(const ALogger : ILogger);
begin
  fLogger := ALogger;
end;

procedure TVdpDM.LoadLoggerConfig(const ALogger : ILogger);
var
vQR : TvdQuery;
vStr : TStringList;
vIni : TMemIniFile;
begin
  try
    vStr := TStringList.Create;
    vIni := TMemIniFile.Create('');
    vQR := CreateReadQuery(nil, 'SELECT FILE FROM CONFIG WHERE TYPE = :TYPE');
    try
      vQR.ParamByName('TYPE').AsString := 'LogConfig';
      vQR.Open;
      vStr.Text := vQR.FieldByName('FILE').AsWideString;
      vQR.Close;

      if vStr.Count > 0 then
      begin
        vIni.SetStrings(vStr);
        IniRoutines.LoadLoggerConfig(vIni, ALogger);
      end;
    finally
      FreeAndNil(vStr);
      FreeAndNil(vIni);
      FreeAndNil(vQR);
    end;
  except
    on e : Exception do
      Log(lException, E);
  end;
end;

procedure TVdpDM.LoadLoggersConfig(const ALoggers: ILoggers);
var
vQR : TvdQuery;
vStr : TStringList;
vIni : TMemIniFile;
begin
  try
    vStr := TStringList.Create;
    vIni := TMemIniFile.Create('');
    vQR := CreateReadQuery(nil, 'SELECT FILE FROM CONFIG WHERE TYPE = :TYPE');
    try
      vQR.ParamByName('TYPE').AsString := 'LogConfig';
      vQR.Open;
      vStr.Text := vQR.FieldByName('FILE').AsWideString;
      vQR.Close;
      if vStr.Count > 0 then
      begin
        vIni.SetStrings(vStr);
        IniRoutines.LoadLoggersConfig(vIni, ALoggers);
      end;
    finally
      FreeAndNil(vStr);
      FreeAndNil(vIni);
      FreeAndNil(vQR);
    end;
  except
    on e : Exception do
      Log(lException, E);
  end;
end;

procedure TVdpDM.SaveLoggerConfig(const ALogger : ILogger);
var
vQR : TvdQuery;
vStr : TStringList;
vIni : TMemIniFile;
vUpdate : boolean;
begin
  try
    vUpdate := false;
    vStr := TStringList.Create;
    vIni := TMemIniFile.Create('');
    try
      vQR := CreateReadQuery(nil, 'SELECT FILE FROM CONFIG WHERE TYPE = :TYPE');
      try
        vQR.ParamByName('TYPE').AsString := 'LogConfig';
        vQR.Open;
        vStr.Text := vQR.FieldByName('FILE').AsWideString;
        if vStr.Count > 0 then
        begin
          vIni.SetStrings(vStr);
          vUpdate := True;
        end;
        vQR.Close;
      finally
        FreeAndNil(vQR);
      end;


      IniRoutines.SaveLoggerConfig(vIni, ALogger);
      vStr.Clear;
      vIni.GetStrings(vStr);

      if vUpdate then
        vQR := CreateWriteQuery(nil, 'UPDATE CONFIG SET FILE = :FILE WHERE TYPE = :TYPE')
      else
        vQR := CreateWriteQuery(nil, 'INSERT INTO CONFIG (TYPE, FILE) VALUES (:TYPE, :FILE)');
      try
        vQR.ParamByName('TYPE').AsString := 'LogConfig';
        vQR.ParamByName('FILE').AsWideMemo := vStr.Text;
        vQR.ExecSQL;
        Commit(vQR);
      finally
        FreeAndNil(vQR);
      end;
    finally
      FreeAndNil(vStr);
      FreeAndNil(vIni);
    end;
  except
    on e : Exception do
      Log(lException, E);
  end;
end; 

procedure TVdpDM.SaveLoggersConfig(const ALoggers: ILoggers);
var
vQR : TvdQuery;
vStr : TStringList;
vIni : TMemIniFile;
vUpdate : boolean;
begin
  try
    vUpdate := false;
    vStr := TStringList.Create;
    vIni := TMemIniFile.Create('');
    try
      vQR := CreateReadQuery(nil, 'SELECT FILE FROM CONFIG WHERE TYPE = :TYPE');
      try
        vQR.ParamByName('TYPE').AsString := 'LogConfig';
        vQR.Open;
        vStr.Text := vQR.FieldByName('FILE').AsWideString;
        if vStr.Count > 0 then
        begin
          vIni.SetStrings(vStr);
          vUpdate := True;
        end;
        vQR.Close;
      finally
        FreeAndNil(vQR);
      end;

      IniRoutines.SaveLoggersConfig(vIni, ALoggers);

      vStr.Clear;
      vIni.GetStrings(vStr);


      if vUpdate then
        vQR := CreateWriteQuery(nil, 'UPDATE CONFIG SET FILE = :FILE WHERE TYPE = :TYPE')
      else
        vQR := CreateWriteQuery(nil, 'INSERT INTO CONFIG (TYPE, FILE) VALUES (:TYPE, :FILE)');

      try
        vQR.ParamByName('TYPE').AsString := 'LogConfig';
        vQR.ParamByName('FILE').AsWideMemo := vStr.Text;
        vQR.ExecSQL;
        Commit(vQR);
      finally
        FreeAndNil(vQR);
      end;
    finally
      FreeAndNil(vStr);
      FreeAndNil(vIni);
    end;
  except
    on e : Exception do
      Log(lException, E);
  end;
end;

end.
